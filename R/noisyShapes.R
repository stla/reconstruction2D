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
