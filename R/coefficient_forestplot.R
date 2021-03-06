#'Create forest plot of model coefficients with confidence intervals.
#'
#'Create a ggplot forest plot of model coefficients with confidence intervals.
#'
#'The forest plot groups variables along the axis determined by the \code{horiz}
#'parameter and colors the data by model. If \code{model_names = NULL}, the
#'default, models are numbered sequentially in the order they appear in
#'\code{model_list} (Model 1, Model 2, Model 3, etc.). If two confidence levels
#'are provided the shorter interval is drawn with a thicker line.
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'@param levels A list of levels of confidence for confidence intervals (max 2
#'  levels).
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
#'@export


coefficient_forestplot = function(model_list, model_names = NULL,
                                  levels = c(0.95, 0.50), horiz = TRUE){

  # check assumptions -------------------------------------

  model_list_checks(model_list)

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }


  # the following is just from R CMD check will not affect code
  Variable = NULL
  Estimate = NULL
  Model = NULL
  ci1_lo = NULL
  ci1_hi = NULL
  ci2_lo = NULL
  ci2_hi = NULL


  # get confidence intervals ------------------------------

  confint_matrix = ci_matrix(model_list=model_list, model_names=model_names,
                             levels = levels)


  # create plot -------------------------------------------

  mydodge = 0.2 * (length(model_list) - 1)

  fplot = ggplot2::ggplot(data = confint_matrix,
                          ggplot2::aes(x = Variable, y = Estimate,
                                       colour = Model)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = ci1_lo, ymax = ci1_hi),
                             fatten=5,
                             position = ggplot2::position_dodge(width = mydodge)) +
    ggplot2::xlab('') +
    ggplot2::ylab('Coefficient estimates')

  if(ncol(confint_matrix) == 7){
    fplot = fplot +
      ggplot2::geom_linerange(ggplot2::aes(ymin = ci2_lo, ymax = ci2_hi),
                              size = 1.5,
                              position = ggplot2::position_dodge(width = mydodge))
  }

  if(horiz){
    fplot = fplot +
      ggplot2::scale_x_discrete(limits = rev(unique(confint_matrix$Variable))) +
      ggplot2::scale_colour_discrete(breaks = rev(sort(confint_matrix$Model))) +
      ggplot2::coord_flip()
  }else{
    fplot = fplot +
      ggplot2::scale_x_discrete(limits = confint_matrix$Variable[confint_matrix$Model == unique(confint_matrix$Model)[1]])
  }


  # return plot -------------------------------------------

  return(fplot)
}
