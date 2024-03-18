#' 2D reconstruction
#' @description Reconstruction of a 2D shape from a set of points.
#'
#' @param points matrix of points, with two columns
#' @param masses numeric vector of point masses; if \code{NULL}, all masses are
#'   set to \code{1}
#' @param steps number of edge collapse operators to be performed; if given
#'   (i.e. not \code{NA}), then \code{np} and \code{tolerance} must be \code{NA}
#' @param np number of vertices to reach in the output; if given (i.e. not
#'   \code{NA}), then \code{steps} and \code{tolerance} must be \code{NA}
#' @param tolerance tolerance on the Wasserstein distance; if given
#'   (i.e. not \code{NA}), then \code{steps} and \code{np} must be \code{NA}
#'
#' @return A list containing two matrices: \code{vertices}, the isolated
#'   vertices, and \code{segments}.
#' @export
#'
#' @note Only one of \code{steps}, \code{np} and \code{tolerance} must be given.
#'
#' @details See \href{https://doc.cgal.org/latest/Optimal_transportation_reconstruction_2/index.html}{Optimal Transportation Curve Reconstruction}.
#'
#' @examples
#' # reconstruct the pentagram
#' pts <- noisyPentagram()
#' shape <- otr(pts, np = 10)
#' segs <- shape[["segments"]]
#' # plot
#' opar <- par(mar = c(0, 0, 0, 0))
#' plot(
#'   pts, type = "p", pch = ".", asp = 1, cex = 4,
#'   xlab = NA, ylab = NA, axes = FALSE, col = "blue"
#' )
#' segments(
#'   segs[, "x0"], segs[, "y0"], segs[, "x1"], segs[, "y1"],
#'   lwd = 3, col = "black"
#' )
#' par(opar)
otr <- function(points, masses = NULL, steps = NA, np = NA, tolerance = NA) {
  stopifnot(ncol(points) == 2)
  storage.mode(points) <- "double"
  if(anyNA(points)) {
    stop("Missing points coordinates are not allowed.")
  }
  if(is.null(masses)) {
    masses <- rep(1, nrow(points))
  } else {
    stopifnot(length(masses) == nrow(points))
    storage.mode(masses) <- "double"
    if(anyNA(masses)) {
      stop("Missing masses are not allowed.")
    }
  }
  if(sum(c(isNothing(steps), isNothing(np), isNothing(tolerance))) != 2L) {
    stop(
      "You must provide one and only one argument among `steps`, `np`, ",
      "and `tolerance`."
    )
  }
  if(isNothing(steps)) {
    steps <- 0L
  } else {
    stopifnot(isPositiveInteger(steps))
  }
  if(isNothing(np)) {
    np <- 0L
  } else {
    stopifnot(isPositiveInteger(np))
  }
  if(isNothing(tolerance)) {
    tolerance <- 1
  } else {
    stopifnot(isPositiveNumber(tolerance))
  }
  runOTR(
    t(points), masses,
    as.integer(steps), as.integer(np), as.double(tolerance)
  )
}
