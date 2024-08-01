#' Helper function to uniformly draw points in a shape proportional to their population
#'
#' @param N number of samples
#' @param df a dataframe with population (pop) and shape (geometry) columns
#'
#' @return a list of sampled points
#' @export
#'
#' @examples
#' sampler_helper_parallel(10, los_angeles)
sampler_helper_parallel <- function(N, df){

  # required datasets: none

  N_int <- as.integer(N)

  samp_vec <- sample(1:nrow(df), N_int, replace = T, prob = df$pop/sum(df$pop)) %>%
    table() %>%
    as.data.frame() # sample N tracts

  indices <- as.character(samp_vec$.) %>% as.numeric()
  shapes <- df[indices, 'geometry']

  start_time <- Sys.time()
  points <- mapply(samp, samp_vec$Freq, shapes)
  end_time <- Sys.time()
  print(end_time - start_time)

  start_time <- Sys.time()

  # how many cores
  if(Sys.info()["sysname"] == 'Windows'){
    points <- parallel::mcmapply(samp, samp_vec$Freq, shapes, mc.cores = 1L)
  } else{
    points <- parallel::mcmapply(samp, samp_vec$Freq, shapes, mc.cores = 2L)
  }

  end_time <- Sys.time()
  print(end_time - start_time)

  points2 <- sf::st_sfc(unlist(points, recursive = FALSE)) %>%
    sf::st_sf()

  return(points2)
}
