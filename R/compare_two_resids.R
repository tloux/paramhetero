#'@importFrom stats pf

compare_two_resids = function(model_list){

  # sample statistics ----

  resid_sds = lapply(model_list, function(m) sigma(m))

  df_list = lapply(model_list, function(m) get_df(m))


  # test statistic, p-value ----

  f_stat = resid_sds[[1]]^2 / resid_sds[[2]]^2

  if(f_stat > 1){
    p1 = 1 - pf(f_stat, df1=df_list[[1]], df2=df_list[[2]])
    p2 = pf(1/f_stat, df1=df_list[[1]], df2=df_list[[2]])
    pval = p1 + p2
  }else{
    p1 = pf(f_stat, df1=df_list[[1]], df2=df_list[[2]])
    p2 = 1 - pf(1/f_stat, df1=df_list[[1]], df2=df_list[[2]])
    pval = p1 + p2
  }


  # return results ----

  ret = c(Model1=resid_sds[[1]], Model2=resid_sds[[2]], f=f_stat,
          df1=df_list[[1]], df2=df_list[[2]], p=pval)
  return(ret)
}
