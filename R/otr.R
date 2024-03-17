otr <- function(points, masses = NULL, steps = NA, np = NA, tolerance = NA) {
  stopifnot(ncol(points) == 2)
  if(is.null(masses)) {
    masses <- rep(1, nrow(points))
  } else {
    stopifnot(length(masses) == nrow(points))
  }
  if(sum(c(isNothing(steps), isNothing(np), isNothing(tolerance))) != 2L) {
    stop("")
  }
  if(isNothing(steps)) {
    steps <- 0L
  }
  if(isNothing(np)) {
    np <- 0L
  }
  if(isNothing(tolerance)) {
    tolerance <- 1
  }
  runOTR(
    t(points), masses,
    as.integer(steps), as.integer(np), as.double(tolerance)
  )
}
