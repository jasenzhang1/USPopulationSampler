#' Title
#'
#' @param area_fips vector of fips
#' @param level the level of granularity depicted in the plot (1 = state, 2 = county)
#'
#' @return shape
#' @export
#'
#' @examples
#' sampled_region_outline(c('06', '32'), 2)
#'
sampled_region_outline <- function(area_fips, level){

  # required datasets: df_state, df_county, df_tracts, df_bg

  state_fips <- c()
  county_fips <- c()


  # 1) sort county by states

  for(i in 1:length(area_fips)){
    if(nchar(area_fips[i]) == 5){ # length of 5 means its a county

      county_fips <- c(county_fips, area_fips[i])

    } else{ # it is a state

      state_fips <- c(state_fips, area_fips[i])

    }
  }


  # 2) collect shapes


  if(level == 1){ #if we want state borders

    df_sample <- load_zenodo('df_county.rda') %>%
      dplyr::filter(get('FIPS') %in% county_fips | get('STATEFP') %in% state_fips)

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


  } else if (level == 2) { #if we want county borders

    df_sample <- load_zenodo('df_county.rda') %>%
      dplyr::filter(get('FIPS') %in% county_fips | get('STATEFP') %in% state_fips)

    the_counties <- unique(df_sample$COUNTYFP)

    index <- 1

    for(i in the_counties){

      if(index == 1){
        geos <- sf::st_sfc(sf::st_union(df_sample$geometry[df_sample$COUNTYFP == i])) %>% data.frame()
      } else{
        geos <- rbind(geos, sf::st_sfc(sf::st_union(df_sample$geometry[df_sample$COUNTYFP == i])))
      }
      index <- index + 1
    }
  } else if (level == 3) { # we want tract borders

    # select unique tracts

    df_sample <- load_zenodo('df_tract.rda') %>%
      dplyr::filter(get('FIPS') %in% county_fips | get('STATEFP') %in% state_fips)

    geos <- df_sample$geometry


  } else if (level == 4){

    # select unique block groups

    df_sample <- load_zenodo('df_bg.rda') %>%
      dplyr::filter(get('FIPS') %in% county_fips | get('STATEFP') %in% state_fips)

    geos <- df_sample$geometry

  } else{ # if we want to lump everything together
    df_sample <- load_zenodo('df_county.rda') %>%
      dplyr::filter(get('FIPS') %in% county_fips | get('STATEFP') %in% state_fips)

    geos <- sf::st_union(df_sample$geometry) %>% sf::st_sfc()
  }


  return(geos)
}
