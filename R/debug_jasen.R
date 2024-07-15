#' jjj
#'
#' @return j
#' @export
#'
#' @examples
#' jj
debug_jasen <- function(){
  print(nrow(bg_pop))
  print(nrow(bg_shapes))
  print(nrow(county_pop))
  print(nrow(county_shapes))
  print(nrow(df_bg))
  print(nrow(df_county))
  print(nrow(df_state))
  print(nrow(df_tract))
  print(nrow(state_pop))
  print(nrow(statefp))
  print(nrow(tract_pop))
  print(nrow(tract_shapes))

  return(nrow(df_county))
}
