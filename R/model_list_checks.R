
model_list_checks = function(model_list){


  # check model form --------------------------------------

  model_classes = lapply(model_list, class)

  # all same class

  if(length(unique(model_classes)) != 1){
    stop('All models must be the same class - either lm or glm.')
  }

  # class lm or glm

  if(!(model_classes[[1]][1] %in% c('lm', 'glm'))){
    stop('All models must be the same class - either lm or glm.')
  }

  # if glm, same family and link function

  if(model_classes[[1]][1] == 'glm'){

    fams = sapply(model_list, function(m) m$family$family)
    links = sapply(model_list, function(m) m$family$link)

    if(any(fams != fams[1]) | any(links != links[1])){
      stop('All models must be same family with same link function')
    }
  }


  # check all models have same covariates in same order ---

  for(j in 2:length(model_list)){
    for(k in 1:(length(model_list) - 1)){
      if(!all(names(get_coefs(model_list[[j]])) == names(get_coefs(model_list[[k]]))))
      {stop('Models must share same predictors in same order.')}
    }
  }

}




model_names_checks = function(model_list, model_names){


  # check model names same length as model list -----------

  if(!is.null(model_names)){
    if(length(model_names) != length(model_list)){
      stop('model_list and model_names not same length.')
    }
  }


  # check model names are unique --------------------------

  if (!is.null(model_names)){
    if(length(unique(model_names)) != length(model_names)){
      stop('model_names not unique.')
    }
  }

}

