
model_list_checks = function(model_list){


  # check model form ----

  model_classes = lapply(model_list, class)

  # all same class

  if(length(unique(model_classes)) != 1){
    stop('All models must be the same class, one of lm, glm.')
  }

  # class one of lm, glm

  if(!(model_classes[[1]][1] %in% c('lm', 'glm'))){
    stop('All models must be the same class, one of lm, glm.')
  }

  # same family and link function

  fams = sapply(model_list, function(m) stats::family(m)$family)
  links = sapply(model_list, function(m) stats::family(m)$link)

  if(any(fams != fams[1]) | any(links != links[1])){
    stop('All models must be same family with same link function')
  }


  # check same covariates in same order ----

  for(j in 1:(length(model_list) - 1)){
    for(k in (j + 1):length(model_list)){
      if(!all(names(get_coefs(model_list[[j]])) == names(get_coefs(model_list[[k]]))))
      {stop('Models must share same predictors in same order.')}
    }
  }

}
