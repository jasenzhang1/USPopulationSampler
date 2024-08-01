#' Title
#'
#' @param freq integer denoting how many samples
#' @param geo sf object denoting the area to be randomly sampled
#'
#' @return a list of sampled points
#' @export
#'
#' @examples
#' samp(3, los_angeles$geometry[2])
samp <- function(freq, geo){
  return(sf::st_sample(geo, size = freq, type = "random"))
}
