#'Extract residuals from multiple models.
#'
#'Extracts residuals from multiple models and returns as a data frame.
#'
#'This function is used within \code{residual_plots} and is not likely to be of
#'much use to end users.
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'
#'@return A data frame of residuals and their corresponding model names.
#'
#'@examples
#'# Simulate data
#'
#'N = 50
#'
#'m = rep(1:4, each=N)
#'
#'x1 = rnorm(n=N*4)
#'x2 = rnorm(n=N*4)
#'x3 = rnorm(n=N*4)
#'
#'y = x1 + x2 + x3 + rnorm(n=N*4)
#'
#'dat = data.frame(m, x1, x2, x3, y)
#'
#'m1 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==1)
#'m2 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==2)
#'m3 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==3)
#'m4 = lm(y ~ x1 + x2 + x3, data=dat, subset=m==4)
#'
#'mList = list(m1, m2, m3, m4)
#'
#'get_resid_df(model_list = mList)
#'
#'@export

get_resid_df = function(model_list, model_names=NULL){

  resid_list = lapply(1:length(model_list), function(i){

    mname = ifelse(is.null(model_names), i, model_names[i])
    resids = residuals(model_list[[i]])

    data.frame(Model = mname, Residuals = resids)
  })

  resid_df = do.call(rbind, resid_list)

  return(resid_df)
}
