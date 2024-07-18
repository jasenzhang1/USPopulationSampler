city_fips <- function(FIPS){
  if (FIPS == '36998') { #new york city
    return(c('36005', '36047', '36061', '36081', '36085'))
    # new york, bronx, kings, queens, richmond county
  }
}
