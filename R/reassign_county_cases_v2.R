#' Title
#'
#' @param temp_FIPS a string for the FIPS
#' @param N a number for number of FIPS
#' @param all_dates a vector of date objects
#' @param tract_df temp
#'
#' @return child_df2, a dataframe of 3 columns and N rows. The three columns describe FIPS,
#' @export
#'
#' @examples
#' reassign_county_cases_v2(covid_march_2020)
reassign_county_cases_v2 <- function(temp_FIPS, N, all_dates, tract_df){

  # v2: Now, we do the ambitious task of reassigning county cases into tracts
  #     as well as sampling within the tract
  #     because all of these are parallelizable

  # index <- 3
  # temp_FIPS <- cases_FIPS$FIPS[index]
  # N <- cases_FIPS$cases[index]
  # all_dates <- cases_FIPS$dates[index]
  # tract_df <- tracts_yay2[index]


  all_dates2 <- all_dates %>% unlist() %>% unname() %>%
    as.Date(origin = "1970-01-01")



  # 3.1 obtain information about the county

  state_FIPS <- substr(temp_FIPS, 1, 2)
  county_FIPS <- substr(temp_FIPS, 3, 5)


  # 3.2 tract shapes and populations
  # child_df <- tracts(state_FIPS, county_FIPS, year = 2020) %>%
  #   merge_shape_pop()
  child_df <- tract_df


  # 3.3 reassign cases to tracts

  divided_pop <- sample(1:nrow(child_df),
                        N,
                        replace = T,
                        prob = child_df$pop/sum(child_df$pop)) %>%
    table() %>%
    as.data.frame()
  colnames(divided_pop) <- c('ID', 'sampled_pop')

  divided_pop$ID <- divided_pop$ID %>% as.character() %>% as.numeric()

  # 3.4 append these (tract_FIPS, cases) datapoints to a new dataframe

  child_df$ID <- 1:nrow(child_df)

  child_df2 <- child_df %>% left_join(divided_pop, by = 'ID') %>%
    select(c('sampled_pop', 'geometry')) %>%
    filter(! is.na(sampled_pop))

  # 3.5 add dates to the new dataframe

  date_partitions <- split(sample(all_dates2), cut(seq_along(all_dates2), cumsum(c(0, child_df2$sampled_pop)), labels = FALSE))

  child_df2$dates <- date_partitions




  return(child_df2)
}
