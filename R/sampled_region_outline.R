#' Title
#'
#' @param area_fips vector of fips
#' @param level the level of granularity
#'
#' @return shape
#' @export
#'
#' @examples
#' sampled_region_outline(c('06', '14'))
sampled_region_outline <- function(area_fips, level){
  state_fips <- c()
  county_fips <- c()

  for(i in 1:length(area_fips)){
    if(length(area_fips[i]) == 5){ # length of 5 means its a county

      county_fips <- c(county_fips, area_fips[i])

    } else{ # it is a state

      state_fips <- c(state_fips, area_fips[i])

    }
  }

  df_sample <- df_county %>%
    dplyr::filter(FIPS %in% county_fips | STATEFP %in% state_fips)

  if(level == 1){ #if we want state borders
    the_states <- unique(df_sample$STATEFP)
    index <- 1
    for(i in the_states){

      if(index == 1){
        geos <- sf::st_sfc(sf::st_union(df_sample$geometry[df_sample$STATEFP == i])) %>% data.frame()
      } else{
        geos <- rbind(geos, sf::st_sfc(sf::st_union(df_sample$geometry[df_sample$STATEFP == i])))
      }
      index <- index + 1
    }


  } else{
    geos <- sf::st_union(df_sample$geometry)
  }


  return(geos)
}
