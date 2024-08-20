#' Title
#'
#' @param FIPS
#'
#' @return temp
#' @export
#'
#' @examples
#' obtain_shapes('00000')
obtain_shapes <- function(FIPS_vec){

  dfs <- list()
  list_index <- 1

  for(i in 1:length(FIPS_vec)){
    print(i)

    temp_FIPS <- FIPS_vec[i]

    state_FIPS <- substr(temp_FIPS, 1, 2)
    county_FIPS <- substr(temp_FIPS, 3, 5)

    if(state_FIPS == '36'){
      the_year <- 2019
    } else{
      the_year <- 2020
    }

    child_df <- tigris::tracts(state_FIPS, county_FIPS, year = the_year) %>%
      merge_shape_pop()

    dfs[[list_index]] <- child_df
    list_index <- list_index + 1
  }

  #return(state_FIPS)
  return(dfs)
}
