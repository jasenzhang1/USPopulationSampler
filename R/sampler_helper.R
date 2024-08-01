#' Helper function to uniformly draw points in a shape proportional to their population
#'
#' @param N number of samples
#' @param df a dataframe with population (pop) and shape (geometry) columns
#'
#' @return a list of sampled points
#' @export
#'
#' @examples
#' sampler_helper(10, los_angeles)
sampler_helper <- function(N, df){

  # required datasets: none

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

  N_int <- as.integer(N)

  samp_vec <- sample(1:nrow(df), N_int, replace = T, prob = df$pop/sum(df$pop)) %>%
    table() %>%
    as.data.frame() # sample N tracts

  points <- data.frame()

  for (i in 1:nrow(samp_vec)) { # for each tract

    index <- as.numeric(as.character(samp_vec[i,1]))

    N_i <- samp_vec[i,2]

    shape_i <- df[index, 'geometry']
    sampled_points <- sf::st_sample(shape_i, size = N_i, type = "random")
    point_df <- unlist(sampled_points) %>% matrix(nrow = N_i, ncol = 2, byrow = T)
    points <- rbind(points, point_df)
  }

  colnames(points) <- c('x', 'y')

  sampled_points_sf <- sf::st_as_sf(points, coords = c('x', 'y'))

  return(sampled_points_sf)
}
