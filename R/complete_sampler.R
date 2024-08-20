#' Title
#'
#' @param covid_df dataframe of covid cases from NYT
#' @param nc number of cores
#'
#' @return a dataframe of sampled points with their dates
#' @export
#'
#' @examples
#' complete_sampler(covid_march_2020)
#'
complete_sampler <- function(covid_df, nc = 1){

  # 1.1) group county cases, melt the dates

  covid_df2 <- covid_df %>%
    # First, calculate total cases per county
    group_by(FIPS) %>%
    mutate(total_cases = sum(cases)) %>%
    # Uncount dates according to the number of cases on each date
    uncount(cases) %>%
    # Summarize the data to create the desired output structure
    summarize(
      total_cases = first(total_cases),
      dates = list(new_date)
    ) %>%
    ungroup()

    colnames(covid_df2) <- c('FIPS', 'cases', 'dates')

    covid_df3 <- covid_df2 %>% arrange(desc(cases))

    # 1.2) deal with FIPS that are cities that bundle actual counties

    wow <- split_metropolis_FIPS_v2(covid_df3)

    cases_FIPS <- wow[[1]]
    used_metros <- wow[[2]]

    # 2) Now, we reassign cases in each county into tracts


    # 2.1) We first extract the tracts (we can't do it parallel idk why)

    relevant_counties <- cases_FIPS$FIPS
    relevant_states <- unique(substr(cases_FIPS$FIPS,1,2))


    # tracts_parallel_county <- parallel::mclapply(relevant_counties[1:40],
    #                                              obtain_shapes_parallel,
    #                                              mc.cores = as.integer(nc))
    #
    # tracts_county <- obtain_shapes(relevant_counties[1:40])
    #
    # tracts_state_parallel <- do.call(rbind, parallel::mclapply(relevant_states,
    #                                                            obtain_shapes_parallel_v2,
    #                                                            mc.cores = as.integer(nc)))


    tracts_state <- do.call(rbind, obtain_shapes_v2(relevant_states))

    # in the case that we extract tract info from states, we need to construct a list with
    # tract information for each county


    tracts_from_state <- parallel::mclapply(relevant_counties,
                                            function(x) obtain_specific_tracts(x, tracts_state),
                                            mc.cores = as.integer(nc))


    # 2.2) now, the tracts are part of the input in mcmapply

    pts <- parallel::mcmapply(reassign_county_cases_v2,
                              cases_FIPS$FIPS,
                              cases_FIPS$cases,
                              cases_FIPS$dates,
                              tracts_from_state,
                              mc.cores = as.integer(nc)) %>%
      t() %>%
      as.data.frame()

    # 2.3) we need to unpack the pts dataframe

    pop_vec <- unlist(pts$sampled_pop, recursive = F)
    geo_vec <- unlist(pts$geometry, recursive = F)
    dat_vec <- unlist(pts$dates, recursive = F)

    # 3) simulate points


    sampled_points_with_dates <- points_to_sf_v2(pop_vec, geo_vec, dat_vec, nc)

    return(sampled_points_with_dates)
}
