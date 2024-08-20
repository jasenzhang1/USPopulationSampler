#' Title
#'
#' @param temp_FIPS a string for the FIPS
#' @param N a number for number of FIPS
#' @param all_dates a vector of date objects
#'
#' @return child_df2, a dataframe of 3 columns and N rows. The three columns describe FIPS,
#' @export
#'
#' @examples
#' reassign_county_cases_v2_troubleshoot(covid_march_2020)
reassign_county_cases_v2_troubleshoot <- function(temp_FIPS, N, all_dates){

  # v2: Now, we do the ambitious task of reassigning county cases into tracts
  #     as well as sampling within the tract
  #     because all of these are parallelizable

  # troubleshoot: do a for loop instead of parallelizing


  all_dates2 <- all_dates %>% unlist() %>% unname() %>%
    as.Date(origin = "1970-01-01")

  # 3.1 obtain information about the county

  state_FIPS <- substr(temp_FIPS, 1, 2)
  county_FIPS <- substr(temp_FIPS, 3, 5)


  # 3.2 tract shapes and populations
  child_df <- tracts(state_FIPS, county_FIPS, year = 2020) %>%
    merge_shape_pop()

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


  # # 3.6 now, we have a (N = number of relevant tracts) x 3 dataframe with
  #
  # sampled_points <- parallel::mcmapply(samp, pts$sampled_pop, pts$geometry, mc.cores = as.integer(nc))
  #
  # N_i <- sum(pts$sampled_pop)
  #
  # sampled_points2 <- unlist(sampled_points) %>%
  #   matrix(nrow = N_i, ncol = 2, byrow = T) %>%
  #   as.data.frame()
  #
  # # unpack the dates
  #
  # the_dates <- unlist(pts$dates) %>% unname() %>% as.Date(origin = "1970-01-01")
  #
  # colnames(sampled_points2) <- c('x','y')
  # sampled_points2$date <- the_dates

  return(child_df2)
}
