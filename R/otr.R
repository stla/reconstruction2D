#' 2D reconstruction
#' @description Reconstruction of a 2D shape from a set of points.
#'
#' @param points matrix of points, with two columns
#' @param masses numeric vector of point masses; if \code{NULL}, all masses are
#'   set to \code{1}
#' @param steps number of steps
#' @param np number of vertices to reach
#' @param tolerance Wasserstein tolerance
#'
#' @return A list containing two matrices: \code{vertices}, the isolated
#'   vertices, and \code{segments}.
#' @export
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
