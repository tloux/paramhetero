#'Compare coefficient vectors across models
#'
#'Compare coefficient vectors, after removing intercept, across multiple models.
#'
#'This function currently supports comparing coefficient vectors from two
#'models. The intercepts of the models are removed, if they exist, and the
#'coefficient vectors are compared by Hotelling's \eqn{T^2} test. This can be
#'considered as an initial omnibus test for differences among the coefficients
#'before searching through all coefficients for individual differences using,
#'for example, \code{compare_coefs}.
#'
#'@param model_list A list of regression models.
#'
#'@return List of test results. This includes the chi-squared statistic, degrees
#'  of freedom, and p-value.
#'
#'@examples
#'  ##Simulate data
#'
#'  N = 500
#'
#'  m = rep(1:2, each=N)
#'
#'  x1 = rnorm(n=N*2)
#'  x2 = rnorm(n=N*2)
#'  x3 = rnorm(n=N*2)
#'
#'  y = x1 + x2 + x3 + rnorm(n=N*2)
#'
#'  dat = data.frame(m, x1, x2, x3, y)
#'
#'  m1 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==1)
#'  m2 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==2)
#'
#'  mList = list(m1, m2)
#'
#'  compare_coef_vectors(model_list = mList)
#'
#'@export



compare_coef_vectors = function(model_list){

  # check assumptions ----

  model_list_checks(model_list)


  # send to calculations ----

  if(length(model_list) == 2){
    res = compare_two_coef_vectors(model_list=model_list)
  }else{
    stop('Can only compare two models at a time.')
  }


  # return results ----

  return(res)

}
