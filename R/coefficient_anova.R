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
#'@param padj Adjustment of p-values for multiple comparisons. Must come from
#'  \code{\link{p.adjust.methods}}.
#'
#'@return Data frame of coefficient information. Data frame includes coefficient
#'  estimates for each model as well as results from ANOVA test comparing
#'  coefficients across models.
#'
#'@seealso \code{\link{coef_welch}}
#'
#'@examples
#'  # Simulate data N = 500
#'
#'  m = rep(1:4, each=N)
#'
#'  x1 = rnorm(n=N*4)
#'  x2 = rnorm(n=N*4)
#'  x3 = rnorm(n=N*4)
#'
#'  y = x1 + x2 + x3 + rnorm(n=N*4)
#'
#'  dat = data.frame(m, x1, x2, x3, y)
#'
#'  m1 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==1)
#'  m2 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==2)
#'  m3 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==3)
#'  m4 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==4)
#'
#'  mList = list(m1, m2, m3, m4)
#'
#'  # Compare coefficients across models
#'
#'  coefficient_anova(model_list = mList, model_names = c('Group A', 'Group B',
#'  'Group C', 'Group D'))
#'
#' @export


coefficient_anova = function(model_list, model_names = NULL, padj=p.adjust.methods){

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
  anova_mat[,8] = p.adjust(anova_mat[,8], method=padj)
  rownames(anova_mat) = NULL
  anova_res = data.frame(Coefficient = rownames(b_mat), anova_mat)
  anova_res$Coefficient = as.character(anova_res$Coefficient)

  return(anova_res)
}
