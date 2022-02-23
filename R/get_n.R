#'@importFrom stats fitted.values

get_n = function(model){
  UseMethod('get_n')
}


get_n.default = function(model){
  return(length(fitted.values(model)))
}


get_n.svyolr = function(model){
  return(nrow(fitted.values(model)))
}
