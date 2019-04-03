#' Compare single regression coefficient across models.
#'
#' Compare the regression coefficients of a predictor across models.
#'
#' Both \code{coef_anova} and  \code{coef_welch} compare the coefficient of
#' a predictor across multiple models, testing the null hypothesis that all
#' parameters are equal. \code{coef_anova} assumes heterogeneity across models,
#' while \code{coef_welch} allows for heterogeneity in the natural variation.
#' Currently, only \code{coef_welch} is implemented as it is a slightly more
#' generally applicable test. An option for \code{coef_anova} may appear in
#' later versions.
#'
#' For example, given three models
#'
#' \deqn{y_i = \beta_{10} + \beta_{11}x_{i1} + \beta_{12}x_{i2} + \epsilon_{1i}}{yi = b10 + b11 xi1 + b12 xi2 + e1i}
#'
#' \deqn{y_i = \beta_{20} + \beta_{21}x_{i1} + \beta_{22}x_{i2} + \epsilon_{2i}}{yi = b20 + b21 xi1 + b22 xi2 + e2i}
#'
#' \deqn{y_i = \beta_{30} + \beta_{31}x_{i1} + \beta_{32}x_{i2} + \epsilon_{3i}}{yi = b30 + b31 xi1 + b32 xi2 + e3i}
#'
#' Would test hypotheses \eqn{H_0: \beta_{11} = \beta_{21} = \beta_{31}}{b11 = b21 = b31} against
#' the alternative that not all three parameters for \eqn{x_1}{x1} are equal. (Or
#' similarly for the parameters of \eqn{x_2}{x2}.)
#'
#' This is the workhorse function for \code{\link{coefficient_anova}}.
#'
#' @param bs Vector of regression coefficients.
#' @param vars Vector of variances of estimates in \code{bs}.
#' @param ns Vector of sample sizes for each model.
#' @param p Single integer, number of shared predictors in models.
#'
#' @return Vector containing the resulting F statistic, numerator degrees of
#'   freedom, denominator degrees of freedom, and the p-value for the test of
#'   equality.
#'
#' @references Liu, H. "Comparing Welch's ANOVA, a Kruskal-Wallis Test and
#'   Traditional ANOVA in Case of Heterogeneity of Variance." Master's Thesis.
#'   Available at \url{https://scholarscompass.vcu.edu/etd/3985/}.
#'
#' @export


coef_welch = function(bs, vars, ns, p){


  # basic stat calculations ---------------------------------

  # number of models
  M = length(bs)

  # model degrees of freedom
  dfs = ns - p

  # group weights
  ws = ns / vars


  # mstr ----------------------------------------------------

  overall_coef = weighted.mean(x=bs, w=ws)

  sstr_terms = sapply(1:M, function(m){
    ws[m] * (bs[m] - overall_coef)^2
  })

  sstr = sum(sstr_terms)

  mstr = sstr / (M - 1)


  # "mse" ---------------------------------------------------

  Lambda = 3 * sum((1 - ws/sum(ws))^2 / dfs) / (M^2 - 1)


  # test stat, p-value --------------------------------------

  fstat = mstr / (1 + 2 * Lambda * (M - 2) / 3)

  dfnum = M - 1
  dfdenom = 1/Lambda

  pval = 1 -pf(fstat, df1=dfnum, df2=dfdenom)


  # return output -----------------------------------------

  ret = c(f=fstat, df1=dfnum, df2=dfdenom, p=pval)
  return(ret)

}



#'@rdname coef_welch
coef_anova = function(bs, vars, ns, p){


  # basic stat calculations -------------------------------

  # number of models
  M = length(bs)

  # model degrees of freedom
  dfs = ns - p - 1


  # mstr --------------------------------------------------

  b_bar = weighted.mean(x=bs, w=ns)

  sstr_terms = sapply(1:M, function(m){
    ns[m] * (bs[m] - b_bar)^2
  })

  sstr = sum(sstr_terms)

  mstr = sstr / (M - 1)


  # mse ---------------------------------------------------

  sse_terms = sapply(1:M, function(m){
    dfs[m] * vars[m]
  })

  sse = sum(sse_terms)

  mse = sse / sum(dfs)


  # test stat, p-value --------------------------------------

  fstat = mstr / mse

  dfnum = M - 1
  dfdenom = sum(dfs)

  pval = 1 -pf(fstat, df1=dfnum, df2=dfdenom)



  # return output -----------------------------------------

  ret = c(f=fstat, df1=dfnum, df2=dfdenom, p=pval)
  return(ret)

}
