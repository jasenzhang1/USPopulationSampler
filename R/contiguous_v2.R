#' Title
#'
#' @param vec vector of STATEFP values
#'
#' @return return a vector of boolean values that describes if each entry supplied in `vec` is a contiguous state.
#' @export
#'
#' @examples
#' df_contiguous <- contiguous_v2(c('01', '02', '03', '04', '05'))
#'
contiguous_v2 <- function(vec){


  contiguous_state_fp <- c('01', '04', '05', '06', '08', '09')
  contiguous_state_fp <- c(contiguous_state_fp,
                           as.character(c(10:13, 16:42, 44:51, 53:56)))

  return(vec %in% contiguous_state_fp)

}
