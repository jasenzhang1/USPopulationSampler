#' Title
#'
#' @param FIPS
#'
#' @return temp
#' @export
#'
#' @examples
#' obtain_shapes_parallel('00000')
obtain_shapes_parallel <- function(temp_FIPS){
  state_FIPS <- substr(temp_FIPS, 1, 2)
  county_FIPS <- substr(temp_FIPS, 3, 5)

  if(state_FIPS == '36'){
    the_year <- 2019
  } else{
    the_year <- 2020
  }
  child_df <- merge_shape_pop(tigris::tracts(state_FIPS, county_FIPS, year = the_year))

  # %>%
  #   merge_shape_pop()

  #return(state_FIPS)
  return(child_df)
}
