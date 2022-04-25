#'@importFrom stats coef vcov

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
