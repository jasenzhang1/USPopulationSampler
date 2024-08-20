#' Helper function to uniformly draw points in a shape proportional to their population
#'
#' @param N number of samples
#' @param df a dataframe with population (pop) and shape (geometry) columns
#' @param nc number of cores (default number is one)
#'
#' @return a list of sampled points
#' @export
#'
#' @examples
#' sampler_helper_parallel(los_angeles, 10, 4)
sampler_helper_parallel <- function(df, N, nc = 1){

  # required datasets: none

  N_int <- as.integer(N)

  samp_vec <- sample(1:nrow(df), N_int, replace = T, prob = df$pop/sum(df$pop)) %>%
    table() %>%
    as.data.frame() # sample N tracts

  indices <- as.character(samp_vec$.) %>% as.numeric()
  shapes <- df[indices, 'geometry']


  # how many cores
  if(Sys.info()["sysname"] == 'Windows'){
    points <- parallel::mcmapply(samp, samp_vec$Freq, shapes, mc.cores = 1L)
  } else{
    points <- parallel::mcmapply(samp, samp_vec$Freq, shapes, mc.cores = as.integer(nc))
  }


  if(length(points) == N){
    points2 <- sf::st_sfc(points) %>%
      sf::st_sf()
  } else{
    points2 <- sf::st_sfc(unlist(points, recursive = FALSE)) %>%
      sf::st_sf()
  }




  return(points2)
}




