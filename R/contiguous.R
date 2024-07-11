#' Title
#'
#' @param df dataframe with STATEFP as a column
#'
#' @return a dataframe with just information from the contiguous US
#' @export
#'
#' @examples
#' df_contiguous <- contiguous(df)
#'
contiguous <- function(df){
  contiguous_state_fp <- c('01', '04', '05', '06', '08', '09')
  contiguous_state_fp <- c(contiguous_state_fp,
                           as.character(c(10:13, 16:42, 44:51, 53:56)))

  df2 <- df[df$STATEFP %in% contiguous_state_fp,]

  return(df2)
}
