#'Compare regression residual standard deviation across multiple models.
#'
#'Compare regression residual standard deviation across multiple \code{lm}
#'models using Levene's test.
#'
#'Levene's test is often suggested as a diagnostic check for homoskedasticity in
#'ANOVA analyses. This function works for linear models (\code{\link{lm}})
#'only.
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'
#'@return Numeric vector of Levene test results. This includes an F-statistic,
#'  numerator  degrees of freedom, denominator degrees of freedom, and the
#'  associated p-value.
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
#'  residual_levene(model_list = mList)
#'
#'@importFrom stats pf sigma
#'
#'@export


residual_levene = function(model_list, model_names=NULL){


  # check assumptions -------------------------------------

  model_list_checks(model_list)

  model_family = stats::family(model_list[[1]])$family
  if(model_family != 'gaussian'){
    stop('Residual test only appropriate for normally distributed errors.')
  }

  if(class(model_list[[1]])[1] == 'svyglm'){
    warning('Analysis does not use survey weights.')
  }

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }


  # basic statistics --------------------------------------

  M = length(model_list)

  residList = lapply(model_list, function(m) get_resid(m))

  nList = lapply(model_list, function(m) get_n(m))
  n = Reduce('+', nList)

  dfList = lapply(model_list, function(m) get_df(m))
  dfSum = Reduce('+', dfList)

  resid_sds = sapply(model_list, function(m) sigma(m))

  if(is.null(model_names)){
    names(resid_sds) = paste('Model', 1:length(model_list))
  }else{
    names(resid_sds) = model_names
  }


  # Levene's test statistic, p-value ----------------------

  absResidList = lapply(residList, function(r) abs(r))

  absResidMeans = lapply(absResidList, function(absr) mean(absr))

  absResidMeanAll = mean(unlist(absResidList))


  numTerms = sapply(1:length(model_list), function(m){
    nList[[m]] * (absResidMeans[[m]] - absResidMeanAll)^2
    })
  numerator = sum(numTerms)

  dfnum = M - 1


  denomTerms = sapply(1:length(model_list), function(m){
    sum((absResidList[[m]] - absResidMeans[[m]])^2)
  })
  denominator = sum(denomTerms)

  dfdenom = dfSum


  fstat = (numerator / dfnum) / (denominator / dfdenom)

  pval = 1 - pf(fstat, df1=dfnum, df2=dfdenom)


  # return results ----------------------------------------

  ret = c(resid_sds, f=fstat, df1=dfnum, df2=dfdenom, p=pval)
  return(ret)
}
