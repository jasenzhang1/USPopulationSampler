#' Title
#'
#' @param states a vector containing state names, presumably poorly formatted in the following ways:
#'               - their two letter abbreviation
#'               - poor capitalization of the state names
#'
#' @return vector of FIPS for the respective states
#' @export
#'
#' @examples
#' convert_state_names_FIPS(c('nj', 'pa', 'NY'))
#'
convert_state_names_FIPS <- function(states){

  # required datasets: df_state
  # ---------------------------------------------

  df_state <- load_zenodo('df_state.rda')

  # ---------------------------------------------

  new_states <- convert_state_names(states)

  FIPS <- df_state$STATEFP[df_state$NAME %in% new_states]

  return(FIPS)
}
