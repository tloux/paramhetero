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
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
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
#'N = 500
#'
#'  m = rep(1:4, each=N)
#'
#'  x1 = rnorm(n=N*4)
#'  x2 = rnorm(n=N*4)
#'  x3 = rnorm(n=N*4)
#'
#'  y = x1 + x2 + x3 + rnorm(n=N*4)
#'
#'  dat = data.frame(m, x1, x2, x3, y)
#'
#'  m1 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==1)
#'  m2 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==2)
#'  m3 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==3)
#'  m4 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==4)
#'
#'  mList = list(m1, m2, m3, m4)
#'
#'  parametric_heterogeneity(model_list = mList,
#'                           model_names = c('Group A', 'Group B',
#'                                           'Group C', 'Group D'))
#'
#'@export


parametric_heterogeneity = function(model_list, model_names = NULL){

  # covariate vector
  manova_results = coefficient_manova(model_list = model_list)

  # coefficient-wise tests
  anova_results = coefficient_anova(model_list = model_list,
                                    model_names = model_names)

  # residual SD
  if(class(model_list[[1]]) == 'lm'){
    levene_results = residual_levene(model_list = model_list)
  }

  # combine, format results
  res_stats = c('f', 'df1', 'df2', 'p')

  results0 = rbind(manova_results[res_stats],
                   anova_results[, res_stats],
                   levene_results[res_stats])

  results = data.frame(test = c('Coefficient vector',
                                anova_results$Coefficient,
                                'Residuals'),
                       results0)

  return(results)
}
