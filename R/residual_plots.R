#'Plot residuals.
#'
#'Plots residuals of multiple models as density plots and boxplots.
#'
#'\code{residual_plots} plots density plots and boxplots in a two row, single
#'column format. \code{residual_density} and \code{residual_boxplots} plot the
#'component pieces. If \code{model_names = NULL}, the default, models are
#'numbered sequentially in the order they appear in \code{model_list} (Model 1,
#'Model 2, Model 3, etc.).
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'@param bw Bandwidth for density plots (default is \code{"nrd0"}).
#'@param thm A ggplot2 theme for the output plots.
#'
#'@return A residual plot for inputted regression models.
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
#'  residual_plots(model_list = mList, thm=ggplot2::theme_minimal())
#'
#'@export


residual_plots = function(model_list, model_names=NULL, bw='nrd0', thm=NULL){

  # check assumptions -------------------------------------

  model_list_checks(model_list)

  if(class(model_list[[1]])[1] == 'svyglm'){
    warning('Visualization does not use survey weights.')
  }

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }


  # create plots ------------------------------------------

  resid_dens = residual_density(model_list, model_names, bw=bw) +
    ggplot2::guides(color=FALSE, fill=FALSE)

  resid_box = residual_boxplot(model_list, model_names) +
    ggplot2::guides(color='legend', fill='legend') +
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                   legend.position='bottom')

  if(!is.null(thm)){
    resid_dens = resid_dens + thm
    resid_box = resid_box + thm +
      ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                     legend.position='bottom')
  }

  ggpubr::ggarrange(resid_dens, resid_box, heights = c(2, 1),
                    align = 'v', ncol = 1, nrow = 2)
}


#'@export
#'@rdname residual_plots
residual_density = function(model_list, model_names=NULL, bw='nrd0'){


  # check assumptions -------------------------------------

  model_list_checks(model_list)

  if(class(model_list[[1]])[1] == 'svyglm'){
    warning('Visualization does not use survey weights.')
  }

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }


  # the following is just from R CMD check will not affect code
  Residuals = NULL
  Model = NULL


  # create plot -------------------------------------------

  resid_df = make_resid_df(model_list, model_names)

  ggplot2::ggplot(data=resid_df, ggplot2::aes(x=Residuals, color=Model, fill=Model)) +
    ggplot2::geom_density(alpha=0.3, position='identity', bw=bw) +
    ggplot2::ylab('') +
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())
}


#'@export
#'@rdname residual_plots
residual_boxplot = function(model_list, model_names=NULL){

  # check assumptions -------------------------------------

  model_list_checks(model_list)

  if(class(model_list[[1]])[1] == 'svyglm'){
    warning('Visualization does not use survey weights.')
  }

  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }


  # the following is just from R CMD check will not affect code
  Residuals = NULL
  Model = NULL


  # create plot -------------------------------------------

  resid_df = make_resid_df(model_list, model_names)

  ggplot2::ggplot(data=resid_df, ggplot2::aes(x=Model, y=Residuals,
                                              color=Model, fill=Model)) +
    ggplot2::geom_boxplot(alpha=0.3) +
    ggplot2::xlab('') +
    ggplot2::guides(color=FALSE, fill=FALSE) +
    ggplot2::theme(axis.ticks.y=ggplot2::element_blank()) +
    ggplot2::coord_flip()
}
