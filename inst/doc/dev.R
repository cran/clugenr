## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  dpi = 120
)

## ----setup, include=FALSE-----------------------------------------------------
# Keep examples reproducible in newer R versions and set seed
RNGversion("3.6.0")
set.seed(12345)

## ---- out.width = "100%", dpi = 200-------------------------------------------
library(clugenr)
x <- clugen(2, 5, 800, c(-1, 1), 0.6, c(4, 6), 5, 0.4, 0.5)
plot(x$points, col = x$clusters, xlab = "x", ylab = "y", asp = 1)

