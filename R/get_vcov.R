#'@importFrom stats coef vcov
#'@importFrom lme4 fixef



get_vcov = function(model){
  UseMethod('get_vcov')
}


get_vcov.default = function(model){

  int_index = grep(pattern='(I|i)ntercept', x=names(coef(model)))

  if(length(int_index) > 0){
    return(vcov(model)[-int_index, -int_index])
  }else{
    return(vcov(model))
  }
}


get_vcov.lmerMod = function(model){

  int_index = grep(pattern='(I|i)ntercept', x=names(fixef(model)))

  if(length(int_index) > 0){
    return(vcov(model)[-int_index, -int_index])
  }else{
    return(vcov(model))
  }
}


get_vcov.glmerMod = function(model){

  int_index = grep(pattern='(I|i)ntercept', x=names(fixef(model)))

  if(length(int_index) > 0){
    return(vcov(model)[-int_index, -int_index])
  }else{
    return(vcov(model))
  }
}


get_vcov.svyolr = function(model){

  n_zeta = length(model$lev) - 1
  last_coef = length(coef(model)) - n_zeta

  pred_vcov = survey:::vcov(model)[1:last_coef, 1:last_coef]
  return(pred_vcov)
}
