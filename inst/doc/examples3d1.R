## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.asp = 0.4,
  fig.width = 8
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(clugenr)    # The clugenr library
options(rgl.useNULL = TRUE)  # Create RGL plots in systems without displays (CI)
library(rgl)
setupKnitr(autoprint = TRUE) # Render RGL plots directly on generated page

# Load helper functions for plotting examples
source("plot_examples_3d.R", local = knitr::knit_global())

# Keep examples reproducible in newer R versions
RNGversion("3.6.0")

## -----------------------------------------------------------------------------
seed <- 123

## -----------------------------------------------------------------------------
e043 <- clugen(3, 4, 500, c(1, 0, 0), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e044 <- clugen(3, 4, 500, c(1, 1, 1), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e045 <- clugen(3, 4, 500, c(0, 0, 1), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e043, t = "e043: direction = [1, 0, 0]"),
                 list(e = e044, t = "e044: direction = [1, 1, 1]"),
                 list(e = e045, t = "e045: direction = [0, 0, 1]"))

## -----------------------------------------------------------------------------
seed <- 123

## -----------------------------------------------------------------------------
# Custom angle_deltas function: arbitrarily rotate some clusters by 90 degrees
angdel_90 <- function(nclu, astd) sample(c(0, pi / 2), nclu, replace = TRUE)

## -----------------------------------------------------------------------------
e046 <- clugen(3, 6, 1000, c(1, 0, 0), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e047 <- clugen(3, 6, 1000, c(1, 0, 0), pi / 8, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e048 <- clugen(3, 6, 1000, c(1, 0, 0), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed,
               angle_deltas_fn = angdel_90)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e046, t = "e046: angle_disp = 0"),
                 list(e = e047, t = "e047: angle_disp = π / 8"),
                 list(e = e048, t = "e048: custom angle_deltas function"))

## -----------------------------------------------------------------------------
seed <- 123

## -----------------------------------------------------------------------------
# Define a main direction for each cluster
dirs <- matrix(c(1, 1, 1,
                 0, 0, 1,
                 1, 0, 0,
                 0, 1, 0,
                 -1, 1, 1),
               nrow = 5, byrow = TRUE)

## -----------------------------------------------------------------------------
e049 <- clugen(3, 5, 1000, dirs, 0, rep.int(0, 3), 20, 0, 0.25, seed = seed)
e050 <- clugen(3, 5, 1000, dirs, pi / 12, rep.int(0, 3), 20, 0, 0.25, seed = seed)
e051 <- clugen(3, 5, 1000, dirs, pi / 4, rep.int(0, 3), 20, 0, 0.25, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e049, t = "e049: angle_disp = 0"),
                 list(e = e050, t = "e050: angle_disp = π / 12"),
                 list(e = e051, t = "e051: angle_disp = π / 4"))

## -----------------------------------------------------------------------------
seed <- 123

## -----------------------------------------------------------------------------
e052 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 0, 0, 0.5,
               seed = seed, point_dist_fn = "n")
e053 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 10, 0, 0.5,
               seed = seed, point_dist_fn = "n")
e054 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 30, 0, 0.5,
               seed = seed, point_dist_fn = "n")

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e052, t = "e052: llength = 0"),
                 list(e = e053, t = "e053: llength = 10"),
                 list(e = e054, t = "e054: llength = 30"))

## -----------------------------------------------------------------------------
# Custom llengths function: line lengths tend to grow for each new cluster
llen_grow <- function(nclu, llen, llenstd) {
  llen * (0:(nclu - 1) + rnorm(nclu, sd = llenstd))
}

## -----------------------------------------------------------------------------
e055 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 15,  0.0, 0.5,
               seed = seed, point_dist_fn = "n")
e056 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 15, 10.0, 0.5,
               seed = seed, point_dist_fn = "n")
e057 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 10,  0.1, 0.5,
               seed = seed, point_dist_fn = "n", llengths_fn = llen_grow)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e055, t = "e055: llength_disp = 0.0"),
                 list(e = e056, t = "e056: llength_disp = 10.0"),
                 list(e = e057, t = "e057: custom llengths function"))

## -----------------------------------------------------------------------------
seed <- 321

## -----------------------------------------------------------------------------
e058 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(30, 10, 10), 25, 4, 3, seed = seed)
e059 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 30, 10), 25, 4, 3, seed = seed)
e060 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 30), 25, 4, 3, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e058, t = "e058: cluster_sep = [30, 10, 10]"),
                 list(e = e059, t = "e059: cluster_sep = [10, 30, 10]"),
                 list(e = e060, t = "e060: cluster_sep = [10, 10, 30]"))

## -----------------------------------------------------------------------------
seed <- 321

## -----------------------------------------------------------------------------
# Custom clucenters function: places clusters in a diagonal
centers_diag <- function(nclu, csep, coff) {
  matrix(1, nrow = nclu, ncol = length(csep)) * (1:nclu * max(csep)) +
    rep(coff, each = nclu)
}

## -----------------------------------------------------------------------------
e061 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 10), 12, 3, 2.5, seed = seed)
e062 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 10), 12, 3, 2.5, seed = seed,
               cluster_offset = c(20, -20, 20))
e063 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 10), 12, 3, 2.5, seed = seed,
               cluster_offset = c(-50, -50, -50), clucenters_fn = centers_diag)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e061, t = "e061: default"),
                 list(e = e062, t = "e062: cluster_offset = [20, -20, 20]"),
                 list(e = e063, t = "e063: custom clucenters function"))

