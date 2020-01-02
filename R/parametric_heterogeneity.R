#'Test coefficient and residual heterogeneity across models.
#'
#'An umbrella function to assess heterogeneity across functions.
#'
#'\code{parametric_heterogeneity} is used to run and format results from
#'\code{\link{coefficient_manova}}, \code{\link{coefficient_anova}}, and
#'\code{\link{residual_levene}}, the last for \code{lm} models only. Please see
#'those functions for more detail.
#'
#'@param model_list A list of regression models.
#'
#'@return Data frame containing name of items tested for heterogeneity,
#'  F-statistics for homogeneity tests, numerator degrees of freedom,
#'  denominator degrees of freedom, and the associated p-value.
#'
#'@seealso
#'  \itemize{
#'    \item \code{\link{coefficient_manova}}
#'    \item \code{\link{coefficient_anova}}
#'    \item \code{\link{residual_levene}}
#'  }
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
#'  parametric_heterogeneity(model_list = mList)
#'
#'@export


parametric_heterogeneity = function(model_list){


  # check assumptions -------------------------------------

  model_list_checks(model_list)


  # test covariate vectors --------------------------------

  manova_results = coefficient_manova(model_list = model_list)


  # coefficient-wise tests --------------------------------

  anova_results = coefficient_anova(model_list = model_list)


  # test residual SD --------------------------------------

  if(family(model_list[[1]])$family == 'gaussian'){
    levene_results = residual_levene(model_list = model_list)
  }


  # combine, format results -------------------------------

  res_stats = c('f', 'df1', 'df2', 'p')

  if(family(model_list[[1]])$family == 'gaussian'){
    results0 = rbind(manova_results[res_stats],
                     anova_results[, res_stats],
                     levene_results[res_stats])

    results = data.frame(test = c('Coefficient vector',
                                  anova_results$B,
                                  'Residuals'),
                         results0)
  }else{
    message('Residual test only appropriate for normally distributed errors so not performed.')
    results0 = rbind(manova_results[res_stats],
                     anova_results[, res_stats])

    results = data.frame(test = c('Coefficient vector',
                                  anova_results$B),
                         results0)
  }


  # return results ----------------------------------------

  return(results)
}
