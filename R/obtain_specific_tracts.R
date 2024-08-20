#' Title
#'
#' @param temp_FIPS temp
#' @param df temp
#'
#' @return temp
#' @export
#'
#' @examples
#' obtain_specific
obtain_specific_tracts <- function(temp_FIPS, df){

  state_FIPS <- substr(temp_FIPS, 1, 2)
  county_FIPS <- substr(temp_FIPS, 3, 5)
  df2 <- df %>% filter(STATEFP == state_FIPS) %>%
    filter(COUNTYFP == county_FIPS)

  return(df2)
}
