#'Create forest plot of model coefficients with confidence intervals
#'
#'Create a ggplot forest plot of model coefficients with confidence intervals.
#'
#'The forest plot groups variables along the axis determined by the \code{horiz}
#'parameter and colors the data by model. If \code{model_names = NULL}, the
#'default, models are numbered sequentially in the order they appear in
#'\code{model_list} (Model 1, Model 2, Model 3, etc.).
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models.
#'@param varlabs A vector of labels for model variables.
#'@param conflevel Confidence level for intervals.
#'@param horiz Toggle whether confidence intervals are displayed horizontally or
#'  vertically. Default is set to \code{TRUE}.
#'
#'@return A ggplot object to compare model coefficient estimates with their
#'  corresponding confidence interval(s), grouped by coefficient.
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
#'  coefficient_forestplot(model_list = mList,
#'                         model_names =c('Northeast', 'South',
#'                                        'North Central', 'West'),
#'                         horiz = FALSE)
#'
#'@importFrom stats confint coef
#'@importFrom ggplot2 ggplot aes geom_pointrange position_dodge2 scale_y_discrete position_dodge scale_colour_discrete
#'
#'@export


coefficient_forestplot = function(model_list, model_names = NULL,
                                  varlabs = NULL,
                                  conflevel=0.95, horiz = TRUE){

  # check assumptions ----

  model_list_checks(model_list)

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }else{
    model_names = paste('Model', 1:length(model_list))
  }

  # the following is just from R CMD check will not affect code
  Variable = NULL
  Estimate = NULL
  Model = NULL
  ci_lo = NULL
  ci_hi = NULL


  # get confidence intervals ----

  ci_list = lapply(1:length(model_list), function(m){

    if(class(model_list[[m]])[1] == 'svyolr'){
      n_zeta = length(model_list[[m]]$lev) - 1
      last_coef = length(coef(model_list[[m]])) - n_zeta
      tmp = as.data.frame(confint(model_list[[m]],
                                  level=conflevel)[1:last_coef, ])
      tmp$Estimate = coef(model_list[[m]])[1:last_coef]
      tmp$Variable = rownames(tmp)
      tmp$Model = model_names[m]

    }else{
      tmp = as.data.frame(confint(model_list[[m]], level=conflevel))

      tmp$Estimate = coef(model_list[[m]])
      tmp$Variable = rownames(tmp)
      tmp$Model = model_names[m]
    }

    tmp = tmp[tmp$Variable != '(Intercept)', ]
    rownames(tmp) = NULL

    return(tmp)
  })
  confints = do.call(rbind, ci_list)
  names(confints)[1:2] = c('ci_lo', 'ci_hi')


  # create plot ----

  mydodge = 0.2 * (length(model_list) - 1)

  if(is.null(varlabs)) varlabs = unique(confints$Variable)

  if(horiz){
    fplot = ggplot(data = confints, aes(x = Estimate, y = Variable, colour = Model)) +
      geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi),
                      position = position_dodge2(width = mydodge, reverse = TRUE)) +
      scale_y_discrete(limits = rev(unique(confints$Variable)),
                       labels = rev(varlabs))

  }else{
    fplot = ggplot(data = confints, aes(x = Variable, y = Estimate, colour = Model)) +
      geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                      position = position_dodge(width = mydodge)) +
      scale_x_discrete(limits = unique(confints$Variable),
                       labels = varlabs)
  }


  # return plot ----

  return(fplot)
}
