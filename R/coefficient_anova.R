#'Compare coefficients of all shared predictors across models via ANOVA.
#'
#'Compare coefficients of all shared predictors across models via ANOVA using
#'\code{\link{coef_welch}}.
#'
#'For each predictor variable in the models, runs \code{\link{coef_welch}} to
#'test for equality of regression parameters across models.
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'@param padj Adjustment of p-values for multiple comparisons. Value must come
#'  from \code{\link{p.adjust.methods}}.
#'
#'@return Data frame of coefficient information. Data frame includes coefficient
#'  estimates for each model as well as results from ANOVA test comparing
#'  coefficients across models.
#'
#'@seealso \code{\link{coef_welch}}
#'
#'@examples
#'  states = as.data.frame(state.x77)
#'
#'  m1 = lm(`Life Exp` ~ Income + Illiteracy, data=states,
#'          subset=state.region=='Northeast')
#'  m2 = lm(`Life Exp` ~ Income + Illiteracy, data=states,
#'          subset=state.region=='South')
#'  m3 = lm(`Life Exp` ~ Income + Illiteracy, data=states,
#'          subset=state.region=='North Central')
#'  m4 = lm(`Life Exp` ~ Income + Illiteracy, data=states,
#'          subset=state.region=='West')
#'
#'  mList = list(m1, m2, m3, m4)
#'
#'  coefficient_anova(model_list = mList,
#'                    model_names = c('Northeast', 'South',
#'                                    'North Central', 'West'))
#'
#'@importFrom stats p.adjust.methods vcov p.adjust
#'
#'@export


coefficient_anova = function(model_list, model_names = NULL, padj=p.adjust.methods){


  # check assumptions -------------------------------------

  model_list_checks(model_list)

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }


  # basic statistics --------------------------------------

  if(is.null(model_names)) model_names = paste('Model', 1:length(model_list))

  b_mat = sapply(model_list,function(m){
    m$coefficients[-1]                                # remove intercept
  })
  colnames(b_mat) = model_names

  var_mat = sapply(model_list,function(m){
    m$df.residual * diag(vcov(m))[-1]                 # remove intercept
  })

  n_vect = sapply(model_list, function(m){
    length(m$residuals)
  })

  anova_mat0 = sapply(1:nrow(b_mat), function(i){
    coef_welch(bs=b_mat[i,], vars=var_mat[i,], ns=n_vect,
               p=nrow(b_mat))
  })

  anova_mat = cbind(b_mat, t(anova_mat0))
  anova_mat[, ncol(anova_mat)] = p.adjust(anova_mat[, ncol(anova_mat)],
                                          method=padj)
  rownames(anova_mat) = NULL
  anova_res = data.frame(Coefficient = rownames(b_mat), anova_mat)
  anova_res$Coefficient = as.character(anova_res$Coefficient)


  # return results ----------------------------------------

  return(anova_res)
}
