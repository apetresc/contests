raindrops <- function(number) {
  result <- ""
  if (number %% 3 == 0) {
    result <- paste0(result, "Pling")
  }
  if (number %% 5 == 0) {
    result <- paste0(result, "Plang")
  }
  if (number %% 7 == 0) {
    result <- paste0(result, "Plong")
  }
  if (result == "") {
    return(as.character(number))
  } else {
    return(result)
  }
}
