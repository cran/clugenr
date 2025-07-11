## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.asp = 0.5,
  dpi = 120
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
e085 <- clugen(1, 3, 2000, 1, 0, 10, 6, 1.5, 0, seed = seed)
e086 <- clugen(1, 3, 2000, 1, 0, 10, 6, 1.5, 0, seed = seed, proj_dist_fn = "unif")
e087 <- clugen(1, 3, 2000, 1, 0, 10, 6, 1.5, 0, seed = seed, proj_dist_fn = proj_wbull)

## -----------------------------------------------------------------------------
plot_examples_1d(list(e = e085, t = "e085: proj_dist_fn='norm'"),
                 list(e = e086, t = "e086: proj_dist_fn='unif'"),
                 list(e = e087, t = "e087: custom proj_dist_fn (Weibull)"),
                 pmargin = 0.07,
                 ymax = 0.725)

## -----------------------------------------------------------------------------
nd <- 5
seed <- 321

## -----------------------------------------------------------------------------
e088 <- clugen(nd, 6, 1500, c(1, 1, 0.5, 0, 0), pi / 16, rep.int(30, nd), 30, 4, 3,
               seed = seed)

## ----fig.asp = 1--------------------------------------------------------------
plot_examples_nd(e088, "e088: 5D with optional parameters set to defaults")

## -----------------------------------------------------------------------------
nd <- 5
seed <- 123

## -----------------------------------------------------------------------------
e089 <- clugen(nd, 6, 1500, c(0.1, 0.3, 0.5, 0.3, 0.1), pi / 12, rep.int(30, nd),
               35, 5, 3.5, seed = seed,
               proj_dist_fn = "unif", point_dist_fn = "n")

## ----fig.asp = 1--------------------------------------------------------------
plot_examples_nd(e089, "e089: 5D with proj_dist_fn='unif' and point_dist_fn='n'")

