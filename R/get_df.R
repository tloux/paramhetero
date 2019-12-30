#'@importFrom stats df.residual

get_df = function(model){
  UseMethod('get_df')
}


get_df.default = function(model){
  df.residual(model)
}
