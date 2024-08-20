#' Title
#'
#' @param FIPS
#'
#' @return temp
#' @export
#'
#' @examples
#' obtain_shapes_parallel_v2('00000')
obtain_shapes_parallel_v2 <- function(state_FIPS){

  # v2: states only

  child_df <- tigris::tracts(state_FIPS, year = 2020) %>%
    merge_shape_pop()

  return(child_df)
}
