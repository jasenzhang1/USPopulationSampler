#' Title
#'
#' @param area_fips vector of either 2-digit or 5-digit FIPS codes for states or counties respectively
#' @param level integer indicating the level of granularity (2 = counties, 3 = tracts, 4 = block groups)
#'
#' @return number of regions
#' @export
#'
#' @examples
#' num_regions('06', 3)
num_regions <- function(area_fips, level){


  # 1) PREPARE THE RELEVANT DATASET

  if(level == 1){ # if the user wants to sample at the state pop level

    N <- load_zenodo('df_state.rda') %>%
      dplyr::filter(get('STATEFP') %in% area_fips) %>%
      nrow()

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

      N <- load_zenodo('df_county.rda') %>%
        dplyr::filter(get('FIPS') %in% fips_lv2 | get('STATEFP') %in% state_fips) %>%
        nrow()

    } else if(level == 3){ # tract level

      fips_lv3 <- c(cities_unpacked, county_fips_no_cities)

      N <- load_zenodo('df_tract.rda') %>%
        dplyr::filter(get('FIPS') %in% fips_lv3 | get('STATEFP') %in% state_fips) %>%
        nrow()

    } else{ # bg level
      N <- load_zenodo('df_bg.rda') %>%
        dplyr::filter(get('FIPS') %in% fips_lv3 | get('STATEFP') %in% state_fips) %>%
        nrow()
    }

  } else{ # no fips provided, return NA
    return(NA)
  }

  # 2) SAMPLE


  return(N)
}
