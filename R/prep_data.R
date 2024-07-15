#' Title
#'
#' @param shape_names string of county names or state names
#' @param level level of sampling granularity
#'
#' @return a dataframe of the relevant regions
#' @export
#'
#' @examples
#' prep_data('california', 3)
#'
prep_data <- function(shape_names, level){
  if(level == 1) { #state
    df <- df_state
  } else if(level == 2) { # county
    df <- df_county
  } else if (level == 3) { # tract
    df <- df_tract
  } else if (level == 4) { # bg
    df <- df_bg
  } else{
    return(0)
  }




}
