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
e40 <- clugen(3, 4, 500, c(1, 0, 0), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e41 <- clugen(3, 4, 500, c(1, 1, 1), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e42 <- clugen(3, 4, 500, c(0, 0, 1), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e40, t = "e40: direction = [1, 0, 0]"),
                 list(e = e41, t = "e41: direction = [1, 1, 1]"),
                 list(e = e42, t = "e42: direction = [0, 0, 1]"))

## -----------------------------------------------------------------------------
seed <- 123

## -----------------------------------------------------------------------------
# Custom angle_deltas function: arbitrarily rotate some clusters by 90 degrees
angdel_90 <- function(nclu, astd) sample(c(0, pi / 2), nclu, replace = TRUE)

## -----------------------------------------------------------------------------
e43 <- clugen(3, 6, 1000, c(1, 0, 0), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e44 <- clugen(3, 6, 1000, c(1, 0, 0), pi / 8, c(10, 10, 10), 15, 1.5, 0.5, seed = seed)
e45 <- clugen(3, 6, 1000, c(1, 0, 0), 0, c(10, 10, 10), 15, 1.5, 0.5, seed = seed,
              angle_deltas_fn = angdel_90)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e43, t = "e43: angle_disp = 0"),
                 list(e = e44, t = "e44: angle_disp = π / 8"),
                 list(e = e45, t = "e45: custom angle_deltas function"))

## -----------------------------------------------------------------------------
seed <- 123

## -----------------------------------------------------------------------------
e46 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 0, 0, 0.5,
              seed = seed, point_dist_fn = "n")
e47 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 10, 0, 0.5,
              seed = seed, point_dist_fn = "n")
e48 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 30, 0, 0.5,
              seed = seed, point_dist_fn = "n")

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e46, t = "e46: llength = 0"),
                 list(e = e47, t = "e47: llength = 10"),
                 list(e = e48, t = "e48: llength = 30"))

## -----------------------------------------------------------------------------
# Custom llengths function: line lengths tend to grow for each new cluster
llen_grow <- function(nclu, llen, llenstd) {
  llen * (0:(nclu - 1) + rnorm(nclu, sd = llenstd))
}

## -----------------------------------------------------------------------------
e49 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 15,  0.0, 0.5,
              seed = seed, point_dist_fn = "n")
e50 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 15, 10.0, 0.5,
              seed = seed, point_dist_fn = "n")
e51 <- clugen(3, 5, 800, c(1, 0, 0), pi / 10, c(10, 10, 10), 10,  0.1, 0.5,
              seed = seed, point_dist_fn = "n", llengths_fn = llen_grow)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e49, t = "e49: llength_disp = 0.0"),
                 list(e = e50, t = "e50: llength_disp = 10.0"),
                 list(e = e51, t = "e51: custom llengths function"))

## -----------------------------------------------------------------------------
seed <- 321

## -----------------------------------------------------------------------------
e52 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(30, 10, 10), 25, 4, 3, seed = seed)
e53 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 30, 10), 25, 4, 3, seed = seed)
e54 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 30), 25, 4, 3, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e52, t = "e52: cluster_sep = [30, 10, 10]"),
                 list(e = e53, t = "e53: cluster_sep = [10, 30, 10]"),
                 list(e = e54, t = "e54: cluster_sep = [10, 10, 30]"))

## -----------------------------------------------------------------------------
seed <- 321

## -----------------------------------------------------------------------------
# Custom clucenters function: places clusters in a diagonal
centers_diag <- function(nclu, csep, coff) {
  matrix(1, nrow = nclu, ncol = length(csep)) * (1:nclu * max(csep)) +
    rep(coff, each = nclu)
}

## -----------------------------------------------------------------------------
e55 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 10), 12, 3, 2.5, seed = seed)
e56 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 10), 12, 3, 2.5, seed = seed,
              cluster_offset = c(20, -20, 20))
e57 <- clugen(3, 8, 1000, c(1, 1, 1), pi / 4, c(10, 10, 10), 12, 3, 2.5, seed = seed,
              cluster_offset = c(-50, -50, -50), clucenters_fn = centers_diag)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e55, t = "e55: default"),
                 list(e = e56, t = "e56: cluster_offset = [20, -20, 20]"),
                 list(e = e57, t = "e57: custom clucenters function"))

## -----------------------------------------------------------------------------
seed <- 456

## -----------------------------------------------------------------------------
e58 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 0.0, seed = seed)
e59 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 1.0, seed = seed)
e60 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 3.0, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e58, t = "e58: lateral_disp = 0"),
                 list(e = e59, t = "e59: lateral_disp = 1"),
                 list(e = e60, t = "e60: lateral_disp = 3"))

## -----------------------------------------------------------------------------
e61 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 0.0, seed = seed,
              proj_dist_fn = "unif")
e62 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 1.0, seed = seed,
              proj_dist_fn = "unif")
e63 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 3.0, seed = seed,
              proj_dist_fn = "unif")

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e61, t = "e61: lateral_disp = 0"),
                 list(e = e62, t = "e62: lateral_disp = 1"),
                 list(e = e63, t = "e63: lateral_disp = 3"))

## -----------------------------------------------------------------------------
# Custom proj_dist_fn: point projections placed using the Beta distribution
proj_beta <- function(len, n) len * rbeta(n, 0.1, 0.1) - len / 2

## -----------------------------------------------------------------------------
e64 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 0.0, seed = seed,
              proj_dist_fn = proj_beta)
e65 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 1.0, seed = seed,
              proj_dist_fn = proj_beta)
e66 <- clugen(3, 4, 1000, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 3.0, seed = seed,
              proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e64, t = "e64: lateral_disp = 0"),
                 list(e = e65, t = "e65: lateral_disp = 1"),
                 list(e = e66, t = "e66: lateral_disp = 3"))

## -----------------------------------------------------------------------------
seed <- 12321

## -----------------------------------------------------------------------------
# Custom proj_dist_fn: point projections placed using the Beta distribution
proj_beta <- function(len, n) len * rbeta(n, 0.1, 0.1) - len / 2

## -----------------------------------------------------------------------------
e67 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed)
e68 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
             proj_dist_fn = "unif")
e69 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
             proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e67, t = "e67: proj_dist_fn = 'norm' (default)"),
                 list(e = e68, t = "e68: proj_dist_fn = 'unif'"),
                 list(e = e69, t = "e69: custom proj_dist_fn (Beta dist.)"))

## -----------------------------------------------------------------------------
e70 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
              point_dist_fn = "n")
e71 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
              point_dist_fn = "n", proj_dist_fn = "unif")
e72 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
              point_dist_fn = "n", proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e70, t = "e70: proj_dist_fn = 'norm' (default)"),
                 list(e = e71, t = "e71: proj_dist_fn = 'unif'"),
                 list(e = e72, t = "e72: custom proj_dist_fn (Beta dist.)"))

## -----------------------------------------------------------------------------
# Custom point_dist_fn: final points placed using the Exponential distribution
clupoints_n_1_exp <- function(projs, lat_std, len, clu_dir, clu_ctr) {
    dist_exp <- function(npts, lstd) lstd * rexp(npts, rate = 2 / lstd)
    clupoints_n_1_template(projs, lat_std, clu_dir, dist_exp)
}

## -----------------------------------------------------------------------------
e73 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
              point_dist_fn = clupoints_n_1_exp)
e74 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
              point_dist_fn = clupoints_n_1_exp, proj_dist_fn = "unif")
e75 <- clugen(3, 5, 1500, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
              point_dist_fn = clupoints_n_1_exp, proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e73, t = "e73: proj_dist_fn = 'norm' (default)"),
                 list(e = e74, t = "e74: proj_dist_fn = 'unif'"),
                 list(e = e75, t = "e75: custom proj_dist_fn (Beta dist.)"))

## -----------------------------------------------------------------------------
seed <- 87

## -----------------------------------------------------------------------------
# Custom clucenters_fn (all): yields fixed positions for the clusters
centers_fixed <- function(nclu, csep, coff) {
  matrix(c(-csep[1], -csep[2], -csep[3], csep[1], -csep[2], -csep[3],
           -csep[1], csep[2], csep[3], csep[1], csep[2], csep[3]),
         nrow = nclu, byrow = TRUE)
}

# Custom clusizes_fn (e77): cluster sizes determined via the uniform distribution,
# no correction for total points
clusizes_unif <- function(nclu, npts, ae) sample(2 * npts / nclu, nclu, replace = TRUE)

# Custom clusizes_fn (e78): clusters all have the same size, no correction for
# total points
clusizes_equal <- function(nclu, npts, ae) npts %/% nclu * rep.int(1, nclu)

## -----------------------------------------------------------------------------
e76 <- clugen(3, 4, 1500, c(1, 1, 1), pi, c(20, 20, 20), 0, 0, 5, seed = seed,
              point_dist_fn = "n",
              clucenters_fn = centers_fixed)
e77 <- clugen(3, 4, 1500, c(1, 1, 1), pi, c(20, 20, 20), 0, 0, 5, seed = seed,
              clusizes_fn = clusizes_unif, point_dist_fn = "n",
              clucenters_fn = centers_fixed)
e78 <- clugen(3, 4, 1500, c(1, 1, 1), pi, c(20, 20, 20), 0, 0, 5, seed = seed,
              clusizes_fn = clusizes_equal, point_dist_fn = "n",
              clucenters_fn = centers_fixed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e76, t = "e76: normal dist. (default)"),
                 list(e = e77, t = "e77: unif. dist. (custom)"),
                 list(e = e78, t = "e78: equal size (custom)"))

