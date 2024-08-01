#' Title
#'
#' @param states a vector containing state names, presumably poorly formatted in the following ways:
#'               - their two letter abbreviation
#'               - poor capitalization of the state names
#'
#' @return a string of states formatted by their full names with proper capitalization
#' @export
#'
#' @examples convert_state_names(c('nj', 'pa', 'NY'))
convert_state_names <- function(states){

  # required datasets: state_convert

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
