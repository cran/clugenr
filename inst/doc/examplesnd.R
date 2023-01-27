## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.asp = 0.5,
  dpi = 200
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
# Load the clugenr library
library(clugenr)

# Load functions for plotting examples
source("plot_examples_1d.R", local = knitr::knit_global())
source("plot_examples_nd.R", local = knitr::knit_global())

# Keep examples reproducible in newer R versions
RNGversion("3.6.0")

## -----------------------------------------------------------------------------
seed <- 222

## -----------------------------------------------------------------------------
# Custom proj_dist_fn: point projections placed using the Weibull distribution
proj_wbull <- function(len, n) rweibull(n, shape = 1.5, scale = len / 2) - len / 2

## -----------------------------------------------------------------------------
e79 <- clugen(1, 3, 2000, 1, 0, 10, 6, 1.5, 0, seed = seed)
e80 <- clugen(1, 3, 2000, 1, 0, 10, 6, 1.5, 0, seed = seed, proj_dist_fn = "unif")
e81 <- clugen(1, 3, 2000, 1, 0, 10, 6, 1.5, 0, seed = seed, proj_dist_fn = proj_wbull)

## -----------------------------------------------------------------------------
plot_examples_1d(list(e = e79, t = "e79: proj_dist_fn='norm'"),
                 list(e = e80, t = "e80: proj_dist_fn='unif'"),
                 list(e = e81, t = "e81: custom proj_dist_fn (Weibull)"),
                 pmargin = 0.07,
                 ymax = 0.575)

## -----------------------------------------------------------------------------
nd <- 5
seed <- 321

## -----------------------------------------------------------------------------
e82 <- clugen(nd, 6, 1500, c(1, 1, 0.5, 0, 0), pi / 16, rep.int(30, nd), 30, 4, 3,
              seed = seed)

## ---- fig.asp = 1-------------------------------------------------------------
plot_examples_nd(e82, "e82: 5D with optional parameters set to defaults")

## -----------------------------------------------------------------------------
nd <- 5
seed <- 123

## -----------------------------------------------------------------------------
e83 <- clugen(nd, 6, 1500, c(0.1, 0.3, 0.5, 0.3, 0.1), pi / 12, rep.int(30, nd),
              35, 5, 3.5, seed = seed,
              proj_dist_fn = "unif", point_dist_fn = "n")

## ---- fig.asp = 1-------------------------------------------------------------
plot_examples_nd(e83, "e83: 5D with proj_dist_fn='unif' and point_dist_fn='n'")

