% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/noisyShapes.R
\name{noisyPentagram}
\alias{noisyPentagram}
\title{Noisy pentagram}
\usage{
noisyPentagram(sd = 0.01, n = 20L)
}
\arguments{
\item{sd}{standard deviation of the noise}

\item{n}{number of points per edge}
}
\value{
A matrix with two columns, one point per row.
}
\description{
Generates a noisy pentagram.
}
\examples{
opar <- par(mar = c(0, 0, 0, 0))
plot(
  noisyPentagram(), type = "p", pch = ".", asp = 1, cex = 4,
  xlab = NA, ylab = NA, axes = FALSE
)
par(opar)
}
