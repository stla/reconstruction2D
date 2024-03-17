rho <- sqrt((5 - sqrt(5))/10)
vs1 <- t(vapply(0:4, function(i){
  c(rho*cos(2*i*pi/5 + pi/10), rho*sin(2*i*pi/5 + pi/10))
}, numeric(2L)))
R <- sqrt((25 - 11*sqrt(5))/10)
vs2 <- t(vapply(0:4, function(i){
  c(R*cos(2*i*pi/5 + pi/10 + pi/5), R*sin(2*i*pi/5 + pi/10 + pi/5))
}, numeric(2L)))

vs <- matrix(NA_real_, nrow = 10, ncol = 2)
vs[c(1, 3, 5, 7, 9), ] <- vs1
vs[c(2, 4, 6, 8, 10), ] <- vs2

pts <- matrix(nrow = 0, ncol = 2)
s_ <- seq(0, 1, len = 20L)
for(i in 1L:10L) {
  v1 <- vs[i, ]
  j <- ifelse(i < 10L, i+1L, 1L)
  v2 <- vs[j, ]
  u <- v2 - v1
  for(s in s_) {
    pts <- rbind(pts, v1 + s*u + rnorm(2, 0, 0.01))
  }
}
masses <- rep(1, nrow(pts))

steps <- 0L
np <- 10L
tolerance <- 0.5
otr <- reconstruction2D:::runOTR(t(pts), masses, steps, np, tolerance)
segs <- otr$segments

plot(NULL, xlim = c(-0.55, 0.55), ylim = c(-0.55, 0.55), asp = 1)
points(pts, pch = ".", cex = 3, col = "blue")
segments(
  segs[, "x0"], segs[, "y0"], segs[, "x1"], segs[, "y1"],
  lwd = 2, col = "black"
)
