#'@importFrom stats pchisq
#'@importFrom MASS ginv

compare_two_coef_vectors = function(model_list){

  # sample statistics ----

  b_list = lapply(model_list, function(m) get_coefs(m))

  cov_list = lapply(model_list, function(m) get_vcov(m))

  p = length(b_list[[1]])


  # Hotelling's test statistic, p-value ----

  b_diff = b_list[[1]] - b_list[[2]]

  cov_diff = cov_list[[1]] + cov_list[[2]]
  cov_inv = ginv(cov_diff)

  teststat = t(b_diff) %*% cov_inv %*% b_diff

  pval = 1 - pchisq(teststat, df=p)


  # return results ----

  ret = c(chisq=teststat, df=p, p=pval)
  return(ret)
}
