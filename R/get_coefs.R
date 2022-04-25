#'@importFrom stats coef

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
