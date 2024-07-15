#' Title
#'
#' @param states vector of states
#'
#' @return vector of FIPS for the states
#' @export
#'
#' @examples
#' ...
convert_state_names_FIPS <- function(states){

  new_states <- convert_state_names(states)

  FIPS <- df_state$STATEFP[df_state$NAME %in% new_states]

  return(FIPS)
}
