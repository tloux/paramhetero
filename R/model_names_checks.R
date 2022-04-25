model_names_checks = function(model_list, model_names){
  
  # check model names same length as model list ----
  
  if(!is.null(model_names)){
    if(length(model_names) != length(model_list)){
      stop('model_list and model_names not same length.')
    }
  }
  
  
  # check model names are unique ----
  
  if (!is.null(model_names)){
    if(length(unique(model_names)) != length(model_names)){
      stop('model_names not unique.')
    }
  }
  
}
