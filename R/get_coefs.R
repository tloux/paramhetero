#'@importFrom stats coef
#'@importFrom lme4 fixef

get_coefs = function(model){
  UseMethod('get_coefs')
}


get_coefs.default = function(model){

  int_index = grep(pattern='(I|i)ntercept', x=names(coef(model)))

  if(length(int_index) > 0){
    return(coef(model)[-int_index])
  }else{
    return(coef(model))
  }
}


get_coefs.lmerMod = function(model){

  int_index = grep(pattern='(I|i)ntercept', x=names(fixef(model)))

  if(length(int_index > 0)){
    return(fixef(model)[-int_index])
  }else{
    return(fixef(model))
  }
}


get_coefs.glmerMod = function(model){

  int_index = grep(pattern='(I|i)ntercept', x=names(fixef(model)))

  if(length(int_index > 0)){
    return(fixef(model)[-int_index])
  }else{
    return(fixef(model))
  }
}


get_coefs.svyolr = function(model){

  n_zeta = length(model$lev) - 1
  last_coef = length(coef(model)) - n_zeta

  pred_coefs = coef(model)[1:last_coef]
  return(pred_coefs)
}
