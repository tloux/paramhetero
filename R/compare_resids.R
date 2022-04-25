#'Compare regression residual standard deviation across models
#'
#'Compare residual standard deviation across models. Works for linear regression
#'(\code{\link{lm}}) only.
#'
#'This function currently supports comparing residual standard deviation from
#'two models. Residuals are assumed to be normally distributed (as also assumed
#'in the linear model itself) and are compared by an F test.
#'
#'@param model_list A list of regression models.
#'
#'@return Vector of results. This includes the residual standard deviation from
#'  each model, the F statistic comparing the standard deviations, the numerator
#'  and denominator degrees of freedom, and the p-value.
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
#'  compare_resids(model_list = mList)
#'
#'@export


compare_resids = function(model_list){

  # check assumptions ----

  model_list_checks(model_list)

  model_family = stats::family(model_list[[1]])$family
  if(model_family != 'gaussian'){
    stop('Residual test only appropriate for normally distributed residuals')
  }


  # send to calculations ----

  if(length(model_list) == 2){
    res = compare_two_resids(model_list=model_list)
  }else{
    stop('Can only compare two models at a time.')
  }


  # return results ----

  return(res)

}
