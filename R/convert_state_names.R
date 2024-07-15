#' Title
#'
#' @param states string of states
#'
#' @return a string of properly formatted states
#' @export
#'
#' @examples convert_state_names(c('nj', 'pa', 'NY'))
convert_state_names <- function(states){
  new_states <- states


  for (i in 1:length(states)) {
    if (nchar(states[i]) == 2) { # if we were supplied a 2 letter abbreviation

      upper_state <- toupper(states[i])
      temp_state <- state_convert$NAME[state_convert$STUSPS == upper_state]


    } else{ # if we were supplied the actual name
      state_split <- unlist(strsplit(states[i], " "))

      state_split_upper <- sapply(state_split, capitalize_first) # capitalize the first letter

      temp_state <- paste(state_split_upper, collapse = " ")

    }
    new_states[i] <- temp_state
  }
  return(new_states)
}
