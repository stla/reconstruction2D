isNothing <- function(x) {
  is.na(x) || is.null(x)
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x > 0
}

isPositiveNumber <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x > 0
}
