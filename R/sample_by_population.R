#' Sampling from a collection shapes, each with a specific population
#'
#' @param N number of samples
#' @param df a dataframe with population (pop) and shape (geometry) columns
#'
#' @return a list of sampled locations
#' @export
#'
#' @examples
#' df <- data.frame(pop = c(1,1,1), geometry = shapes)
#' sample_by_population(1000, df)
sample_by_population <- function(N, df){

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

  samp_vec <- sample(df$pop, N, replace = T, prob =  df$pop/sum(df$pop)) # sample N tracts

  points <- data.frame()

  for(i in 1:nrow(samp_vec)){ # for each tract
    if(samp_vec[i,1] > 0){    # if it has at least one sample
      N_tract <- samp_vec[i,1]
      tract_shape <- df[i, 'geometry']
      sampled_points <- sf::st_sample(tract_shape, size = N_tract, type = "random")
      point_df <- unlist(sampled_points) %>% matrix(nrow = N_tract, ncol = 2, byrow = T)
      points <- rbind(points, point_df)
    }
  }



  colnames(points) <- c('x', 'y')

  sampled_points_sf <- sf::st_as_sf(points, coords = c('x', 'y'))

  return(sampled_points_sf)
}
