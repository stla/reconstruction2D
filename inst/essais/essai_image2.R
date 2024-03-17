library(imager)
library(dplyr)

img <- load.image("Frankenstein.jpg") %>%
  #grayscale() %>%
  threshold("75%")

dat <- img %>%
  as.cimg() %>%
  as.data.frame()  %>%
  mutate(mass = 1 - value) %>%
  sample_n(8000, weight = mass) %>%
  select(x, y, mass) %>%
  filter(mass != 0)


pts <- as.matrix(dat[, c("x", "y")])
masses <- dat$mass


steps <- 0L
np <- 30L
tolerance <- 11
otr <- reconstruction2D:::runOTR(t(pts), masses, steps, np, tolerance)
segs <- otr$segments

# plot(NULL, xlim = c(0, 600), ylim = c(0, 600), asp = 1)
# points(pts, pch = ".")
plot(img)
segments(
  segs[, "x0"], segs[, "y0"], segs[, "x1"], segs[, "y1"],
  lwd = 3, col = "blue"
)
