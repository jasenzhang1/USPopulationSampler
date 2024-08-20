#' Title
#'
#' @param FIPS
#'
#' @return temp
#' @export
#'
#' @examples
#' obtain_shapes_v2('36')
obtain_shapes_v2 <- function(relevant_states){

  #v2: obtain all tracts for all states, hopefully it should be faster

  dfs <- list()
  list_index <- 1

  for(i in 1:length(relevant_states)){
    print(i)

    state_FIPS <- relevant_states[i]


    child_df <- tigris::tracts(state_FIPS, year = 2020) %>%
      merge_shape_pop()

    dfs[[list_index]] <- child_df
    list_index <- list_index + 1
  }


  return(dfs)
}
