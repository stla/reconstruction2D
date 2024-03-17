#' Noisy pentagram
#' @description Generates a noisy pentagram.
#'
#' @param sd standard deviation of the noise
#' @param n number of points per edge
#'
#' @return A matrix with two columns, one point per row.
#' @export
#' @importFrom stats rnorm
#'
#' @examples
#' opar <- par(mar = c(0, 0, 0, 0))
#' plot(
#'   noisyPentagram(), type = "p", pch = ".", asp = 1, cex = 4,
#'   xlab = NA, ylab = NA, axes = FALSE
#' )
#' par(opar)
noisyPentagram <- function(sd = 0.01, n = 20L) {
  rho <- sqrt((5 - sqrt(5))/10)
  vs1 <- t(vapply(0L:4L, function(i) {
    rho * c(cospi(2*i/5 + 1/10), sinpi(2*i/5 + 1/10))
  }, numeric(2L)))
  R <- sqrt((25 - 11*sqrt(5))/10)
  vs2 <- t(vapply(0L:4L, function(i) {
    R * c(cospi(2*i/5 + 1/10 + 1/5), sinpi(2*i/5 + 1/10 + 1/5))
  }, numeric(2L)))
  vs <- matrix(NA_real_, nrow = 10L, ncol = 2L)
  vs[c(1L, 3L, 5L, 7L, 9L), ]  <- vs1
  vs[c(2L, 4L, 6L, 8L, 10L), ] <- vs2
  pts <- matrix(nrow = 0L, ncol = 2L)
  s_ <- seq(0, 1, length.out = n)
  for(i in 1L:10L) {
    v1 <- vs[i, ]
    j <- ifelse(i < 10L, i+1L, 1L)
    v2 <- vs[j, ]
    u <- v2 - v1
    for(s in s_) {
      pts <- rbind(pts, v1 + s*u + rnorm(2L, 0, sd))
    }
  }
  pts
}

#' Noisy decagram
#' @description Generates a noisy decagram.
#'
#' @param sd standard deviation of the noise
#' @param n number of points per edge
#'
#' @return A matrix with two columns, one point per row.
#' @export
#' @importFrom stats rnorm
#'
#' @examples
#' opar <- par(mar = c(0, 0, 0, 0))
#' plot(
#'   noisyDecagram(), type = "p", pch = ".", asp = 1, cex = 4,
#'   xlab = NA, ylab = NA, axes = FALSE
#' )
#' par(opar)
noisyDecagram <- function(sd = 0.015, n = 20L) {
  R <- (sqrt(5) - 1)/2 # circumradius
  vs1 <- vapply(0:9, function(i) {
    R * c(cospi(2*i/10), sinpi(2*i/10))
  }, numeric(2L))
  r <- sqrt(5 - 2*sqrt(5))/2 # inradius
  rho <- sqrt(r^2 + (5/2 - sqrt(5))^2)
  vs2 <- vapply(0:9, function(i) {
    rho * c(cospi(2*i/10 + 1/10), sinpi(2*i/10 + 1/10))
  }, numeric(2L))
  vs <- matrix(NA_real_, nrow = 20L, ncol = 2L)
  vs[seq(1L, 19L, by = 2L), ] <- t(vs1)
  vs[seq(2L, 20L, by = 2L), ] <- t(vs2)
  pts <- matrix(nrow = 0L, ncol = 2L)
  s_ <- seq(0, 1, length.out = n)
  for(i in 1L:20L) {
    v1 <- vs[i, ]
    j <- ifelse(i < 20L, i+1L, 1L)
    v2 <- vs[j, ]
    u <- v2 - v1
    for(s in s_) {
      pts <- rbind(pts, v1 + s*u + rnorm(2L, 0, sd))
    }
  }
  pts
}
