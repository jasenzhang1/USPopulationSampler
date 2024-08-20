#' Title
#'
#' @param pts v2: a 3 column dataframe (sampled_pop, geometry, dates)
#'            sampled_pop[1] is a list, we must unlist() it
#'            geometry[1] is a list, we must unlist() it and convert it into a matrix
#'            dates[1] is a list, we must unlist() it, unname() it, and convert it from 5 digit integer to date using unix
#'
#'
#' @param nc
#'
#' @return a set of points in sf form, ready to plot
#' @export
#'
#' @examples
points_to_sf_v2 <- function(pop_vec, geo_vec, dat_vec, nc = 1L){

  sampled_points <- parallel::mcmapply(samp,
                                       pop_vec,
                                       geo_vec,
                                       mc.cores = as.integer(nc))

  N_i <- sum(pop_vec)

  sampled_points2 <- unlist(sampled_points) %>%
    matrix(nrow = N_i, ncol = 2, byrow = T) %>%
    as.data.frame()

  # unpack the dates

  the_dates <- unlist(dat_vec) %>% unname() %>% as.Date(origin = "1970-01-01")

  colnames(sampled_points2) <- c('x','y')
  sampled_points2$date <- the_dates

  return(sampled_points2)
}
