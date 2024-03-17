library(imager)
library(dplyr)

cuts <- seq(0, 600, len = 100)

img <- load.image("picture.png") %>%
  #grayscale() %>%
  threshold("auto")

dat <- img %>%
  as.cimg() %>%
  as.data.frame()  %>%
  mutate(mass = 1-value) %>%
  select(x, y, mass) %>%
  mutate(
    ix = cuts[findInterval(x, cuts, all.inside = TRUE)],
    iy = cuts[findInterval(y, cuts, all.inside = TRUE)]
  ) %>%
  filter(mass != 0) %>%
  group_by(ix, iy) %>%
  summarize(w = n())

pts <- as.matrix(dat[, c("ix", "iy")])
masses <- dat$w


steps <- 0L
np <- 15L
tolerance <- 4
otr <- reconstruction2D:::runOTR(t(pts), masses, steps, np, tolerance)
segs <- otr$segments

# plot(NULL, xlim = c(0, 600), ylim = c(0, 600), asp = 1)
# points(pts, pch = ".")
plot(img)
segments(
  segs[, "x0"], segs[, "y0"], segs[, "x1"], segs[, "y1"],
  lwd = 3, col = "blue"
)
