
get_resid = function(model){
  UseMethod('get_resid')
}


get_resid.default = function(model){
  residuals(model)
}
