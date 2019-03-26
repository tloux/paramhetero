#'Compare regression residual standard deviation across multiple models.
#'
#'Compare regression residual standard deviation across multiple \code{lm}
#'models using Levene's test.
#'
#'Levene's test is often suggested as a diagnostic check for homoskedasticity in
#'ANOVA analyses. This function works for linear regression (\code{\link{lm}})
#'only.
#'
#'@param model_list A list of regression models.
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
#'@export


residual_levene = function(model_list){


  # basic statistics --------------------------------------

  M = length(model_list)

  residList = lapply(model_list, function(m) m$residuals)

  nList = lapply(model_list, function(m) nrow(m$model))
  n = Reduce('+', nList)

  dfList = lapply(model_list, function(m) m$df.residual)
  dfSum = Reduce('+', dfList)


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


  fstat = (numerator / dfnum) / (denominator / dedenom)

  pval = 1 - pf(fstat, df1=dfnum, df2=dfdenom)


  # return ------------------------------------------------

  ret = c(f=fstat, df1=dfnum, df2=dfdenom, p=pval)
  return(ret)
}
