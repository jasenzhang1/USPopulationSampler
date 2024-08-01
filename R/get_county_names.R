#' Obtaining a list of counties for a state or several states.
#'
#' @param states vector of states we want to know the county names of
#'
#' @return a vector of countys of the states we selected
#' @export
#'
#' @examples
#' get_county_names(c('CA', 'oregon', 'nj'))
get_county_names <- function(states){

  new_states <- convert_state_names(states)

  county_names_df <- county_convert %>% dplyr::filter(get('STATE_NAME') %in% new_states)

  return(county_names_df$NAME.x)
}
