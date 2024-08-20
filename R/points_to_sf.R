#' Title
#'
#' @param pts a 2-column dataframe with the number of points in the first column
#'            and the respective multi-polygon to be sampled from in the
#'            second column
#'
#' @param nc
#'
#' @return a set of points in sf form, ready to plot
#' @export
#'
#' @examples
points_to_sf <- function(pts, nc = 1L){

  sampled_points <- parallel::mcmapply(samp, pts$sampled_pop, pts$geometry, mc.cores = as.integer(nc))

  N_i <- sum(pts$sampled_pop)

  sampled_points2 <- unlist(sampled_points) %>%
    matrix(nrow = N_i, ncol = 2, byrow = T) %>%
    as.data.frame()

  # unpack the dates

  the_dates <- unlist(pts$dates) %>% unname() %>% as.Date(origin = "1970-01-01")

  colnames(sampled_points2) <- c('x','y')
  sampled_points2$date <- the_dates

  return(sampled_points2)
}
