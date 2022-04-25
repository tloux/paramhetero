#'@importFrom stats pnorm p.adjust sigma

compare_two_coefs = function(model_list, padj='none'){

  # sample statistics ----

  b_list = lapply(model_list, function(m) get_coefs(m))

  var_list = lapply(model_list, function(m) diag(get_vcov(m)))


  # test statistics, p-values ----

  b_diffs = b_list[[1]] - b_list[[2]]

  se_diffs = sqrt(var_list[[1]] + var_list[[2]])

  z_stats = b_diffs / se_diffs

  pvals0 = pnorm(-abs(z_stats))
  pvals = p.adjust(p=pvals0, method=padj)


  # return results ----

  ret = data.frame(coef = names(b_list[[1]]),
                   Model1 = b_list[[1]],
                   Model2 = b_list[[2]],
                   b_diff = b_diffs,
                   se_diff = se_diffs,
                   z = z_stats,
                   p = pvals)
  return(ret)
}
