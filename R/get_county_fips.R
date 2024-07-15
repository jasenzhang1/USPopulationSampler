#' Title
#'
#' @param states vector of states we want to know the county names of
#'
#' @return a vector of countys of the states we selected and their FIPS
#' @export
#'
#' @examples
#' get_county_names('CA', 'oregon', 'nj')
get_county_fips <- function(states){

  new_states <- convert_state_names(states)

  county_names_df <- county_convert %>% dplyr::filter(STATE_NAME %in% new_states)

  return(county_names_df$FIPS)
}
