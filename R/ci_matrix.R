#'Create confidence intervals for all coefficients.
#'
#'Create confidence intervals for all coefficients across models, excluding the
#'intercept.
#'
#'This function is called from \code{\link{coefficient_forestplot}} and might
#'not be of direct use for most users. Confidence intervals are obtained through
#'\code{confint} in the \code{stats} package. Intercepts are excluded from the
#'analysis as they don't relate to interactions or "effect modification," but
#'would be most analogous to an average effect.
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'@param levels At most two confidence levels.
#'
#'@return A data frame of model coefficients with their respective confidence
#'  intervals.
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
#'  paramhetero:::ci_matrix(model_list = mList, levels = 0.95)
#'
#'@importFrom stats coef confint


ci_matrix = function(model_list, model_names = NULL, levels = c(0.95, 0.50)){


  # check assumptions -------------------------------------

  model_list_checks(model_list)

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }

  if(!(length(levels) <= 2)) stop('Can compute at most 2 confidence levels.')


  # format model summary data -----------------------------

  if(is.null(model_names)) model_names = paste('Model', 1:length(model_list))

  levels <- sort(levels, decreasing = TRUE)
  varnames = names(get_coefs(model_list[[1]]))
  ests = list()
  modlist0 <- sapply(model_names, function(n){
    rep(n, length(names(get_coefs(model_list[[1]]))))})
  modlist0 <- unname(modlist0)
  repmodlist <- sapply(modlist0, function(m){rbind(m)})


  for(m in 1:length(model_list)){

    if(m == 1){
      ci1 <- confint(object = model_list[[1]], parm = varnames,
                     level = levels[1])
      ests <-  get_coefs(model_list[[m]])[varnames]
    }else{
      ci1 <- rbind(ci1, confint(object = model_list[[m]], parm = varnames,
                                level = levels[1]))
      ests <- append(ests, get_coefs(model_list[[m]])[varnames])
    }

    if(length(levels) == 1 && m == length(model_list)){
      plotdat <- data.frame(Model = repmodlist,
                            Variable = varnames,
                            Estimate = unname(ests),
                            ci1_lo = ci1[, 1], ci1_hi = ci1[, 2])
    }else{
      if(m == 1){
        ci2 = confint(object = model_list[[1]], parm = varnames,
                      level = levels[2])
      }else{
        ci2 = rbind(ci2, confint(object = model_list[[m]], parm = varnames,
                                 level = levels[2]))
      }

      if(m == length(model_list)){
        plotdat <- data.frame(Model = repmodlist,
                              Variable = varnames,
                              Estimate = unname(ests),
                              ci1_lo = ci1[, 1], ci1_hi = ci1[, 2],
                              ci2_lo = ci2[, 1], ci2_hi = ci2[, 2])
      }
    }
  }


  # return results ----------------------------------------

  return(plotdat)
}
