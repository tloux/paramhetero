#'Plot residuals.
#'
#'Plots residuals of multiple models as a histogram and a boxplot.
#'
#'\code{residual_plots} plots histograms and boxplots in a two row, single
#'column format. \code{residual_histogram} and \code{residual_boxplots} plot the
#'component pieces. If \code{model_names = NULL}, the default, models are
#'numbered sequentially in the order they appear in \code{model_list} (Model 1,
#'Model 2, Model 3, etc.).
#'
#'@param model_list A list of regression models.
#'@param model_names A list of names for the regression models (default is
#'  \code{NULL}).
#'@param thm A ggplot2 theme for the output plots.
#'
#'@return A residual plot for inputted regression models.
#'
#'@examples
#'  # Simulate data
#'
#'  N = 500
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
#'  residual_plots(model_list = mList, thm=theme_minimal())
#'
#'@export


residual_plots = function(model_list, model_names=NULL, thm=NULL){

  resid_hist = residual_histogram(model_list, model_names) +
    guides(color=FALSE, fill=FALSE)

  resid_box = residual_boxplot(model_list, model_names) +
    guides(color='legend', fill='legend') +
    theme(axis.text.y=element_blank(), legend.position='bottom')

  if(!is.null(thm)){
    resid_hist = resid_hist + thm
    resid_box = resid_box + thm +
      theme(axis.text.y=element_blank(), legend.position='bottom')
  }

  ggarrange(resid_hist, resid_box, heights = c(2, 1),
            align = 'v', ncol = 1, nrow = 2)
}


#'@rdname residual_plots
residual_histogram = function(model_list, model_names=NULL){

  resid_df = get_resid_df(model_list, model_names)

  ggplot(data=resid_df, aes(x=Residuals, color=Model, fill=Model)) +
    geom_density(alpha=0.3, position='identity') +
    ylab('') +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
}


#'@rdname residual_plots
residual_boxplot = function(model_list, model_names=NULL){

  resid_df = get_resid_df(model_list, model_names)

  ggplot(data=resid_df, aes(x=Model, y=Residuals, color=Model, fill=Model)) +
    geom_boxplot(alpha=0.3) +
    xlab('') +
    guides(color=FALSE, fill=FALSE) +
    theme(axis.ticks.y=element_blank()) +
    coord_flip()
}
