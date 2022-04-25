#'Compare shared coefficients across models
#'
#'Compares predictor coefficients across models.
#'
#'This function currently supports comparing coefficients from two models. For
#'each model predictor, coefficients are compared across models. P-values come
#'from a two-sided alternative hypothesis. They can, and should, be adjusted for
#'multiple testing to reduce the probability of chance significant findings.
#'
#'@param model_list A list of regression models.
#'@param padj A method from \code{p.adjust.methods} for adjusting coefficient
#'  p-values for multiple testing.
#'
#'@return Data frame of shared coefficients, the difference between them, the
#'  standard error of the difference, the test statistic comparing them, and the
#'  p-value adjusted using the method provided in \code{padj}.
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
#'  compare_coefs(model_list = mList, padj='fdr')
#'
#'@export


compare_coefs = function(model_list, padj='none'){

  message(paste('Using multiple testing adjustment', padj))


  # check assumptions ----

  model_list_checks(model_list)


  # send to calculations ----

  if(length(model_list) == 2){
    res = compare_two_coefs(model_list=model_list, padj=padj)
  }else{
    stop('Can only compare two models at a time.')
  }


  # return results ----

  return(res)

}
