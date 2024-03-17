
n <- 100
pts <- uniformly::runif_on_cube(n, d = 2)
masses <- rep(1, n)

steps <- 0L
np <- 4L
tolerance <- 0.5
otr <- reconstruction2D:::runOTR(t(pts), masses, steps, np, tolerance)
segs <- otr$segments

plot(NULL, xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), asp = 1)
segments(segs[, "x0"], segs[, "y0"], segs[, "x1"], segs[, "y1"])
