#' Title
#'
#' @param shape_df
#'
#' @return dataframe with pop column
#' @export
#'
#' @examples
#' merge_shape_pop(tracts('CA'))
merge_shape_pop <- function(shape_df){

  # if its a block group dataframe

  if('BLKGRPCE' %in% colnames(shape_df)){
    df <- shape_df %>% dplyr::left_join(bg_pop, by = c('STATEFP', 'COUNTYFP', 'TRACTCE', 'BLKGRPCE')) %>%
      select('STATEFP', 'COUNTYFP', 'TRACTCE', 'BLKGRPCE', 'geometry', 'pop') %>%
      filter(!is.na(pop)) %>%
      filter(pop > 0)
  }

  # if its a tract dataframe
  else if('TRACTCE' %in% colnames(shape_df)){
    df <- shape_df %>% dplyr::left_join(tract_pop, by = c('STATEFP', 'COUNTYFP', 'TRACTCE')) %>%
      select('STATEFP', 'COUNTYFP', 'TRACTCE', 'geometry', 'pop') %>%
      filter(!is.na(pop)) %>%
      filter(pop > 0)

    df$pop[is.na(df$pop)] <- 0

  # if its a county dataframe
  } else if('COUNTYFP' %in% colnames(shape_df)){
    df <- shape_df %>% dplyr::left_join(county_pop, by = c('STATEFP', 'COUNTYFP')) %>%
      select('STATEFP', 'COUNTYFP', 'geometry', 'pop') %>%
      filter(!is.na(pop)) %>%
      filter(pop > 0)

  # if its a state dataframe
  } else{
    df <- shape_df %>% dplyr::left_join(state_pop, by = 'STATEFP') %>%
      select('STATEFP', 'geometry', 'pop') %>%
      filter(!is.na(pop)) %>%
      filter(pop > 0)
  }

  return(df)
}
