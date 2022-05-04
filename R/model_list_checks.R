
model_list_checks = function(model_list){


  # check model form ----

  models_allowed = c('lm', 'glm', 'svyglm', 'svyolr')

  allowed_char = paste(models_allowed, collapse=', ')
  model_class_error = paste0('All models must be the same class, one of ',
                             allowed_char, '.')

  model_classes = lapply(model_list, class)

  # all same class

  if(length(unique(model_classes)) != 1){
    stop(model_class_error)
  }

  # class one of allowed classes

  if(!(model_classes[[1]][1] %in% models_allowed)){
    stop(model_class_error)
  }

  # same family and link function

  if(model_classes[[1]][1] != 'svyolr'){
    fams = sapply(model_list, function(m) stats::family(m)$family)
    links = sapply(model_list, function(m) stats::family(m)$link)

    if(any(fams != fams[1]) | any(links != links[1])){
      stop('All models must be same family with same link function')
    }
  }


  # check same covariates in same order ----

  for(j in 1:(length(model_list) - 1)){
    for(k in (j + 1):length(model_list)){
      if(!all(names(get_coefs(model_list[[j]])) == names(get_coefs(model_list[[k]]))))
      {stop('Models must share same predictors in same order.')}
    }
  }

}
