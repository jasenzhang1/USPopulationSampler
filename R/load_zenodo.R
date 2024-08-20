#' Title
#'
#' @param data name of the rda file to download from zenodo
#' @param ID_number number corresponding to the zenodo database. It will vary by the dataset version
#'
#' @return the dataset
#' @export
#'
#' @examples load_zenodo('df_state.rda')
load_zenodo <- function(data, ID_number = 12981327){

  url <- paste('https://zenodo.org/records', ID_number, sep = '/')
  url <- paste(url, 'files', sep = '/')
  url <- paste(url, data, sep = '/')

  utils::download.file(url, data,
                       method = 'auto',
                       quiet = T)

  #load(destfile)

  env <- new.env()

  # Load the dataset into the new environment
  load(data, envir = env)

  # Get the name of the loaded dataset
  dataset_name <- ls(env)[1]

  # Return the dataset
  return(get(dataset_name, envir = env))

}
