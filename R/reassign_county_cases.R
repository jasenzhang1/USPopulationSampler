#' Title
#'
#' @param cases_FIPS
#'
#' @return temp
#' @export
#'
#' @examples
#' reassign_county_cases(covid_march_2020)
reassign_county_cases <- function(cases_FIPS, temp_df, used_metros){

  pts <- data.frame()

  # 1.1 unpack metro data

  metro_fips <- unname(unlist(metropolitan_FIPS$`36998`))
  metro_df <- data.frame()

  for(metro_ID in used_metros){
    metro_regions <- metropolitan_FIPS[metro_ID] %>% unname() %>% unlist()

    the_dates <- temp_df %>% filter(FIPS == metro_ID) %>% pull(new_date)
    the_counts <- temp_df %>% filter(FIPS == metro_ID) %>% pull(cases)

    # shuffle all the dates
    all_dates <- rep(the_dates,the_counts) %>% sample()

    # obtain covid cases for the metro regions

    metro_covid <- cases_FIPS %>% filter(FIPS %in% metro_regions)

    date_partitions <- split(all_dates, cut(seq_along(all_dates), cumsum(c(0, metro_covid$cases)), labels = FALSE))


    metro_covid$dates <- date_partitions

    metro_covid$parent <- rep(metro_ID, nrow(metro_covid))

    metro_df <- rbind(metro_df, metro_covid)
  }

  # 3.0 for loop

  for(i in 1:nrow(cases_FIPS)){ # for each county,


    print(i)

    # 3.1 obtain information about the county
    N <- cases_FIPS[i, 2]
    temp_FIPS <- cases_FIPS[i, 1]
    state_FIPS <- substr(temp_FIPS, 1, 2)
    county_FIPS <- substr(temp_FIPS, 3, 5)

    # 3.1 date information about the county
    if(temp_FIPS %in% metro_fips){ # if the FIPS is a derived FIPS

      all_dates <- metro_df %>% filter(FIPS == temp_FIPS) %>% pull(dates) %>% unlist() %>% unname()


    } else{

      the_dates <- temp_df %>% filter(FIPS == temp_FIPS) %>% pull(new_date)
      the_counts <- temp_df %>% filter(FIPS == temp_FIPS) %>% pull(cases)

      # shuffle all the dates
      all_dates <- rep(the_dates,the_counts) %>% sample()
    }


    # 3.2 tract information about the county
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

    # 3.5 add dates to the new datafrmae

    date_partitions <- split(all_dates, cut(seq_along(all_dates), cumsum(c(0, child_df2$sampled_pop)), labels = FALSE))

    child_df2$dates <- date_partitions



    # 3.6 append

    pts <- rbind(pts, child_df2)

  }
  return(pts)
}
