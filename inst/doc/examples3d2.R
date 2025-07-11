## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.asp = 0.4,
  fig.width = 8.766
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
seed <- 456

## -----------------------------------------------------------------------------
e064 <- clugen(3, 4, 300, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 0.0, seed = seed)
e065 <- clugen(3, 4, 300, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 1.0, seed = seed)
e066 <- clugen(3, 4, 300, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 3.0, seed = seed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e064, t = "e064: lateral_disp = 0"),
                 list(e = e065, t = "e065: lateral_disp = 1"),
                 list(e = e066, t = "e066: lateral_disp = 3"))

## -----------------------------------------------------------------------------
e067 <- clugen(3, 4, 300, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 0.0, seed = seed,
               proj_dist_fn = "unif")
e068 <- clugen(3, 4, 300, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 1.0, seed = seed,
               proj_dist_fn = "unif")
e069 <- clugen(3, 4, 300, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 3.0, seed = seed,
               proj_dist_fn = "unif")

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e067, t = "e067: lateral_disp = 0"),
                 list(e = e068, t = "e068: lateral_disp = 1"),
                 list(e = e069, t = "e069: lateral_disp = 3"))

## -----------------------------------------------------------------------------
# Custom proj_dist_fn: point projections placed using the Beta distribution
proj_beta <- function(len, n) len * rbeta(n, 0.1, 0.1) - len / 2

## -----------------------------------------------------------------------------
e070 <- clugen(3, 4, 400, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 0.0, seed = seed,
               proj_dist_fn = proj_beta)
e071 <- clugen(3, 4, 400, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 1.0, seed = seed,
               proj_dist_fn = proj_beta)
e072 <- clugen(3, 4, 400, c(1, 0, 0), pi / 2, c(20, 20, 20), 13, 2, 3.0, seed = seed,
               proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e070, t = "e070: lateral_disp = 0"),
                 list(e = e071, t = "e071: lateral_disp = 1"),
                 list(e = e072, t = "e072: lateral_disp = 3"))

## -----------------------------------------------------------------------------
seed <- 12321

## -----------------------------------------------------------------------------
# Custom proj_dist_fn: point projections placed using the Beta distribution
proj_beta <- function(len, n) len * rbeta(n, 0.1, 0.1) - len / 2

## -----------------------------------------------------------------------------
e073 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed)
e074 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               proj_dist_fn = "unif")
e075 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e073, t = "e073: proj_dist_fn = 'norm' (default)"),
                 list(e = e074, t = "e074: proj_dist_fn = 'unif'"),
                 list(e = e075, t = "e075: custom proj_dist_fn (Beta dist.)"))

## -----------------------------------------------------------------------------
e076 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               point_dist_fn = "n")
e077 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               point_dist_fn = "n", proj_dist_fn = "unif")
e078 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               point_dist_fn = "n", proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e076, t = "e076: proj_dist_fn = 'norm' (default)"),
                 list(e = e077, t = "e077: proj_dist_fn = 'unif'"),
                 list(e = e078, t = "e078: custom proj_dist_fn (Beta dist.)"))

## -----------------------------------------------------------------------------
# Custom point_dist_fn: final points placed using the Exponential distribution
clupoints_n_1_exp <- function(projs, lat_std, len, clu_dir, clu_ctr) {
  dist_exp <- function(npts, lstd) lstd * rexp(npts, rate = 2 / lstd)
  clupoints_n_1_template(projs, lat_std, clu_dir, dist_exp)
}

## -----------------------------------------------------------------------------
e079 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               point_dist_fn = clupoints_n_1_exp)
e080 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               point_dist_fn = clupoints_n_1_exp, proj_dist_fn = "unif")
e081 <- clugen(3, 5, 400, c(1, 0, 0), pi / 3, c(20, 20, 20), 22, 3, 2, seed = seed,
               point_dist_fn = clupoints_n_1_exp, proj_dist_fn = proj_beta)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e079, t = "e079: proj_dist_fn = 'norm' (default)"),
                 list(e = e080, t = "e080: proj_dist_fn = 'unif'"),
                 list(e = e081, t = "e081: custom proj_dist_fn (Beta dist.)"))

## -----------------------------------------------------------------------------
seed <- 87

## -----------------------------------------------------------------------------
# Custom clucenters_fn (all): yields fixed positions for the clusters
centers_fixed <- function(nclu, csep, coff) {
  matrix(c(-csep[1], -csep[2], -csep[3], csep[1], -csep[2], -csep[3],
           -csep[1], csep[2], csep[3], csep[1], csep[2], csep[3]),
         nrow = nclu, byrow = TRUE)
}

# Custom clusizes_fn (e083): cluster sizes determined via the uniform distribution,
# no correction for total points
clusizes_unif <- function(nclu, npts, ae) sample(2 * npts / nclu, nclu, replace = TRUE)

# Custom clusizes_fn (e084): clusters all have the same size, no correction for
# total points
clusizes_equal <- function(nclu, npts, ae) npts %/% nclu * rep.int(1, nclu)

## -----------------------------------------------------------------------------
e082 <- clugen(3, 4, 400, c(1, 1, 1), pi, c(20, 20, 20), 0, 0, 5, seed = seed,
               point_dist_fn = "n",
               clucenters_fn = centers_fixed)
e083 <- clugen(3, 4, 400, c(1, 1, 1), pi, c(20, 20, 20), 0, 0, 5, seed = seed,
               clusizes_fn = clusizes_unif, point_dist_fn = "n",
               clucenters_fn = centers_fixed)
e084 <- clugen(3, 4, 400, c(1, 1, 1), pi, c(20, 20, 20), 0, 0, 5, seed = seed,
               clusizes_fn = clusizes_equal, point_dist_fn = "n",
               clucenters_fn = centers_fixed)

## -----------------------------------------------------------------------------
plot_examples_3d(list(e = e082, t = "e082: normal dist. (default)"),
                 list(e = e083, t = "e083: unif. dist. (custom)"),
                 list(e = e084, t = "e084: equal size (custom)"))

