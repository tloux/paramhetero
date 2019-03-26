#'Compare coefficient vectors using MANOVA.
#'
#'Compare coefficient vectors using MANOVA across multiple models.
#'
#'Removes intercept from each coefficient vector, then performs a MANOVA
#'analysis to compare remaining regression coefficients across models. Specifically, for models
#'
#'\deqn{\mathbf{Y}_m = \beta_{m0} + \mathbf{X}_m \mathbf{B}_m}
#'
#'for \eqn{i = 1, 2, ..., M}, test the null hypothesis
#'
#'\deqn{\mathbf{B}_1 = \mathbf{B}_2 = ... = \mathbf{B}_m}
#'
#'against the alternative hypothesis that not all coefficient vectors are equal.
#'
#'@param model_list A list of regression models.
#'
#'@return Numeric vector of MANOVA test results. This includes Wilk's lambda,
#'  MANOVA F-statistic, numerator degrees of freedom, denominator degrees of
#'  freedom, and the associated p-value.
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
#'  coefficient_manova(model_list = mList)
#'
#'@export


coefficient_manova = function(model_list){


  # check assumptions -------------------------------------
  
  model_list_checks(model_list)
  
  
  # basic statistics --------------------------------------

  M = length(model_list)

  bList = lapply(model_list, function(m) coefficients(m)[-1])
  p = length(bList[[1]])

  covList = lapply(model_list, function(m) vcov(m)[-1, -1])

  nList = lapply(model_list, function(m) nrow(m$model))
  n = Reduce('+', nList)

  dfList = lapply(model_list, function(m) m$df.residual)


  # mean covariate vector ---------------------------------

  b_terms = lapply(1:length(model_list), function(k){
    nList[[k]] * bList[[k]]
  })

  b_mean = Reduce('+', b_terms) / n


  # between group matrix ----------------------------------

  B_terms = lapply(1:length(model_list), function(k){

    bdiff = bList[[k]] - b_mean

    nList[[k]] * (bdiff %*% t(bdiff))
  })

  B = Reduce('+', B_terms)


  # within group matrix -----------------------------------

  W_terms = lapply(1:length(model_list), function(k){
    dfList[[k]]^2 * covList[[k]]
  })

  W = Reduce('+', W_terms)


  # Wilk's lambda, p-value --------------------------------

  wilks = det(W) / det(B + W)

  a = n - M - (p - M + 2) / 2

  bdenom = p^2 + (M-1)^2 - 5

  if(bdenom > 0){
    b = sqrt((p^2*(M-1)^2 - 4) / bdenom)
  }else{
    b = 1
  }

  c = (p*(M - 1) - 2) / 2

  fstat = ((1 - wilks^(1/b)) / wilks^(1/b)) * ((a*b - c) / (p*(M - 1)))

  numdf = p * (M - 1)
  denomdf = a * b - c

  w_pval = 1 - pf(fstat, df1=numdf, df2=denomdf)


  # return ------------------------------------------------

  ret = c(lambda=wilks, f=fstat, df1=numdf, df2=denomdf, p=w_pval)
  return(ret)
}
