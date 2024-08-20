#' Title
#'
#' @param cases_FIPS dataframe of FIPS with their cases
#'
#' @return dataframe
#' @export
#'
#' @examples
#' split_metropolis_FIPS(covid_march_2020)
split_metropolis_FIPS <- function(cases_FIPS){

  cases_FIPS2 <- data.frame()

  used_metros <- c()

  # 2. Let's break down cases in each county into cases in each tract

  for(i in 1:nrow(cases_FIPS)){


    N <- cases_FIPS[i, 2]
    temp_FIPS <- cases_FIPS[i, 1]


    # 2.1 check the edge case of a FIPS code that's a city

    if(temp_FIPS %in% names(metropolitan_FIPS)){
      print(temp_FIPS)

      used_metros <- c(used_metros, temp_FIPS)

      child_FIPS <- metropolitan_FIPS[temp_FIPS] %>% unname() %>% unlist()

      state_FIPS <- substr(child_FIPS[1], 1, 2)

      # 2.2 divide up the population from the city FIPS into the child FIPS

      child_df <- counties(state_FIPS, year = 2020) %>%
        filter(GEOID %in% child_FIPS) %>%
        merge_shape_pop()

      divided_pop <- sample(1:nrow(child_df),
                            N,
                            replace = T,
                            prob = child_df$pop/sum(child_df$pop)) %>%
        factor(level = 1:nrow(child_df)) %>%
        table() %>%
        as.data.frame()

      colnames(divided_pop) <- c('FIPS', 'cases')
      divided_pop$FIPS <- paste0(child_df$STATEFP,
                                 child_df$COUNTYFP)


      # 2.3 append the new (FIPS, cases) dataframe onto the existing one

      cases_FIPS2 <- rbind(cases_FIPS2, divided_pop)


    } else{ # do nothing and append
      cases_FIPS2 <- rbind(cases_FIPS2, cases_FIPS[i,])
    }

  }

  # 2.4 for the metropolises with 0 sampled ppl

  cases_FIPS3 <- cases_FIPS2 %>% filter(cases > 0)

  return(list(cases_FIPS3, used_metros))
}
