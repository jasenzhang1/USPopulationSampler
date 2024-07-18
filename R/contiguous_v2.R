#' Title
#'
#' @param vec dataframe with STATEFP as a column
#'
#' @return a dataframe with just information from the contiguous US
#' @export
#'
#' @examples
#' df_contiguous <- contiguous(df)
#'
contiguous_v2 <- function(vec){

  contiguous_state_fp <- c('01', '04', '05', '06', '08', '09')
  contiguous_state_fp <- c(contiguous_state_fp,
                           as.character(c(10:13, 16:42, 44:51, 53:56)))

  contiguous_state_names <- statefp$NAME[statefp$STATEFP %in% contiguous_state_fp]

  if(is.na(as.numeric(vec[1]))){ # if user supplies names
    return(vec %in% contiguous_state_names)
  } else{
    return(vec %in% contiguous_state_fp)
  }

}
