#' Obtaining FIPS codes for counties
#'
#' @param states vector of states we want to know the county names of
#'
#' @return a vector of countys of the states we selected and their FIPS
#' @export
#'
#' @examples
#' get_county_fips(c('CA', 'oregon', 'nj'))
get_county_fips <- function(states){

  # required datasets: county_convert

  new_states <- convert_state_names(states)

  county_names_df <- county_convert %>% dplyr::filter(get('STATE_NAME') %in% new_states)

  return(county_names_df$FIPS)
}
