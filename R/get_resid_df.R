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
#'  get_resid_df(model_list = mList)
#'
#'@export

get_resid_df = function(model_list, model_names=NULL){

  # check assumptions -------------------------------------
  
  model_list_checks(model_list)
  
  if(!is.null(model_names)){
    model_names_checks(model_list, model_names)
  }
  
  
  # format residuals into data frame ----------------------
  
  resid_list = lapply(1:length(model_list), function(i){

    mname = ifelse(is.null(model_names), i, model_names[i])
    resids = residuals(model_list[[i]])

    data.frame(Model = mname, Residuals = resids)
  })

  resid_df = do.call(rbind, resid_list)
  
  
  # return results ----------------------------------------
  
  return(resid_df)
}
