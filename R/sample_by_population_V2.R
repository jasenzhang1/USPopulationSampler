#' Sampling from a collection shapes, each with a specific population
#'
#' @param N number of samples
#' @param area_fips a dataframe with population (pop) and shape (geometry) columns
#' @param level granularity
#'
#' @return a list of sampled locations
#' @export
#'
#' @examples
#' df <- data.frame(pop = c(1,1,1), geometry = shapes)
#' sample_by_population(1000, df)
sample_by_population_V2 <- function(N, area_fips, level){

  # V2 - USING VECTORIZATION
  #
  # we give the function:
  #   - the dataframe of populations and shapes
  #
  # we get:
  #   - a dataframe of sampled points by population
  #   - a graph visualization of the sampling
  #
  # input:
  #   - N (number): number of subcounties we want to sample
  #   - df (data.frame): dataframe of the population (pop) and shape (geometry) files

  # 1) PREPARE THE RELEVANT DATASET

  if(level == 1){ # if the user wants to sample at the state pop level

    df_sample <- df_state %>% dplyr::filter(STATEFP %in% area_fips)

  } else if(level %in% c(2,3,4)){ # if the user wants to sample at the county/tract/bg level
    # we allow the user to supply a combination of counties and states

    state_fips <- c()
    county_fips <- c()

    for(i in 1:length(area_fips)){
      if(length(area_fips[i]) == 5){ # length of 5 means its a county

        county_fips <- c(county_fips, area_fips[i])

      } else{ # it is a state

        state_fips <- c(state_fips, area_fips[i])

      }
    }

    if(level == 2){
      df_sample <- df_county %>%
        dplyr::filter(FIPS %in% county_fips | STATEFP %in% state_fips)
    } else if(level == 3){
      df_sample <- df_tract %>%
        dplyr::filter(FIPS %in% county_fips | STATEFP %in% state_fips)
    } else{
      df_sample <- df_bg %>%
        dplyr::filter(FIPS %in% county_fips | STATEFP %in% state_fips)
    }

  } else{
    return(NA)
  }

  # 2) SAMPLE

  sampled_points_sf <- sample_by_population(N, df_sample)

  return(sampled_points_sf)
}
