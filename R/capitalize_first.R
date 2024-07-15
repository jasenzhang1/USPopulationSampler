capitalize_first <- function(s) {
  if (nchar(s) > 0) {
    return(paste0(toupper(substring(s, 1, 1)), tolower(substring(s, 2))))
  } else {
    return(s)
  }
}
