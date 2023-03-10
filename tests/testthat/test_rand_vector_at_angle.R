# Copyright (c) 2020-2023 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

# Create parameter combinations to test
targs <- expand.grid(seed = seeds, nd = num_dims)

# Loop through all parameter combinations
for (i in seq.int(1, nrow(targs))) {

  # Get current parameters
  seed <- targs[i, "seed"]
  nd <- targs[i, "nd"]

  # Set seed
  set.seed(seed)

  # Create combination of seed-depending parameters (line directions + centers)
  tsargs <- expand.grid(u = asplit(get_unitvecs(nvec, nd), 1),
                        a = c(get_angles(nang), pi / 2))

  # Loop through line directions and line centers
  for (j in seq.int(1, nrow(tsargs))) {

    # Get current vector and angle
    u <- tsargs[j, "u"][[1]]
    a <- tsargs[j, "a"]

    # Determine test name for current parameter set
    test_desc <- paste0("rand_vector_at_angle: nd=", nd, ", a=", a,
                        ", u=[", paste(u, collapse = ", "), "]")

    # Perform tests for current parameter set
    test_that(test_desc, {

      # Check that the function runs without warnings
      expect_warning(r <- rand_vector_at_angle(u, a), regexp = NA)

      # Check that returned vector has the correct length
      expect_equal(length(r), nd)

      # Check that returned vector has norm == 1
      expect_equal(norm(r, "2"), 1)

      # Check that vectors u and r have an angle of a between them
      if (nd > 1 && abs(a) < pi / 2) {
        expect_equal(angle_btw(u, r), abs(a))
      }

      # Check corner case where angle == pi / 2 and vector length > 1
      if (nd > 1 && a == pi / 2) {
        v <- stats::runif(1, 2, 100) * u
        expect_warning(r <- rand_vector_at_angle(v, a), regexp = NA)
        expect_equal(angle_btw(v, r), pi / 2)
      }
    })
  }
}
