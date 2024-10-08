#' Sampling in the United States by 2020 Census data
#'
#' @param N number of points to draw
#' @param area_fips vector of FIPS (2 characters for states, 5 characters for counties)
#' @param level integer specifying granularity of population measurements. (1 = state, 2 = county, 3 = tract, 4 = BG)
#'
#' @return sampled points
#' @export
#'
#' @examples
#' US_pop_sampler(50, '06', 2)
US_pop_sampler <- function(N, area_fips, level){

  # required datasets: df_state, df_county, df_tract, df_bg

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

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # 1) PREPARE THE RELEVANT DATASET

  if(level == 1){ # if the user wants to sample at the state pop level

    df_sample <- load_zenodo('df_state.rda') %>%
      dplyr::filter(get('STATEFP') %in% area_fips)

  } else if(level %in% c(2,3,4)){ # if the user wants to sample at the county/tract/bg level
    # we allow the user to supply a combination of counties and states

    state_fips <- c()                 # store statefps
    county_fips_cities <- c()         # store city fips
    county_fips_no_cities <- c()      # store non-city fips
    cities_unpacked <- c()            # store the counties that are contained in the cities

    for(i in 1:length(area_fips)){
      if(nchar(area_fips[i]) == 5){ # length of 5 means its a county

        if(area_fips[i] %in% names(metropolitan_FIPS)){ #if we supplied it a city

          temp <- metropolitan_FIPS[area_fips[i]] %>%
            unname() %>%
            unlist()
          cities_unpacked <- c(cities_unpacked, temp)
          county_fips_cities <- c(county_fips_cities, area_fips[i])
        } else{ # if we didn't supply it a city
          county_fips_no_cities <- c(county_fips_no_cities, area_fips[i])
        }

      } else{ # it is a state

        state_fips <- c(state_fips, area_fips[i])

      }
    }

    # ----------------------------------------------------------

    if(level == 2){ # if we only care about the county level

      fips_lv2 <- c(county_fips_cities, county_fips_no_cities)

      df_sample <- load_zenodo('df_county.rda') %>%
        dplyr::filter(get('FIPS') %in% fips_lv2 | get('STATEFP') %in% state_fips)

    } else if(level == 3){ # tract level

      fips_lv3 <- c(cities_unpacked, county_fips_no_cities)

      df_sample <- load_zenodo('df_tract.rda') %>%
        dplyr::filter(get('FIPS') %in% fips_lv3 | get('STATEFP') %in% state_fips)
    } else{ # bg level
      df_sample <- load_zenodo('df_bg.rda') %>%
        dplyr::filter(get('FIPS') %in% fips_lv3 | get('STATEFP') %in% state_fips)
    }

  } else{ # no fips provided, return NA
    return(NA)
  }

  # 2) SAMPLE

  sampled_points_sf <- sampler_helper(N, df_sample)

  return(sampled_points_sf)
}
