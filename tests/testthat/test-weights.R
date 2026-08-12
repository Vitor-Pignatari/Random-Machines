# Weight/probability transforms + the normalization pipeline in R/weights.R.

# ---- Pure transforms: orientation, length, domain guards -------------------

test_that("weight transforms have the documented monotonic orientation", {
  x <- seq(0.1, 0.9, by = 0.1)
  # maximize-oriented: increasing in x (best model sits at x = 1)
  expect_true(all(diff(logit_weights(x))      > 0))
  expect_true(all(diff(inv_sq_gap_weights(x)) > 0))
  # minimize-oriented: decreasing in x (best model sits at x = 0)
  expect_true(all(diff(inv_logit_weights(x))         < 0))
  expect_true(all(diff(inv_sq_weights(x))            < 0))
  expect_true(all(diff(softmax_weights(x, beta = 1)) < 0))
})

test_that("transforms stay finite at the min-max endpoints (eps domain guard)", {
  ends <- c(0, 1)                       # what .minmax() produces at the extremes
  for (f in list(logit_weights, inv_logit_weights, inv_sq_gap_weights,
                 inv_sq_weights, function(x) softmax_weights(x, beta = 0.5))) {
    w <- f(ends)
    expect_length(w, 2)
    expect_true(all(is.finite(w)))
  }
})

test_that("transforms are pure (do not normalize internally)", {
  # A pure transform need not sum to 1: these deliberately do not.
  expect_false(isTRUE(all.equal(sum(inv_sq_gap_weights(c(0.2, 0.5, 0.9))), 1)))
  expect_false(isTRUE(all.equal(sum(inv_sq_weights(c(0.2, 0.5, 0.9))),     1)))
})

test_that("softmax beta sharpens the weighting", {
  x <- c(0, 0.5, 1)
  spread <- function(b) diff(range(softmax_weights(x, beta = b)))
  expect_gt(spread(4), spread(0.5))     # larger beta -> more concentrated
})

# ---- Normalization helpers --------------------------------------------------

test_that(".minmax scales to [0, 1] and handles a constant vector", {
  expect_equal(.minmax(c(2, 4, 6)), c(0, 0.5, 1))
  expect_equal(.minmax(c(5, 5, 5)), rep(0.5, 3))   # no spread -> mid-interval
})

test_that(".to_simplex sums to 1, shifts negatives, and falls back to uniform", {
  s <- .to_simplex(c(-1, 0, 1))
  expect_equal(sum(s), 1)
  expect_true(all(s >= 0))
  expect_equal(.to_simplex(c(0, 0, 0)), rep(1 / 3, 3))   # uniform fallback
})

test_that(".normalize_weights sums to 1 and neutralises non-finite weights", {
  expect_equal(sum(.normalize_weights(c(1, 2, 3, 4))), 1)
  w <- .normalize_weights(c(Inf, 5, 10, -3))
  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1)
  expect_equal(w[1], 0)                                    # Inf neutralised
  expect_equal(.normalize_weights(c(0, 0, 0)), rep(1 / 3, 3))
})

# ---- Selection grid ---------------------------------------------------------

test_that(".default_weight_fns pairs each cell with its transforms", {
  expect_identical(.default_weight_fns("binary", FALSE)$lambda, logit_weights)
  expect_identical(.default_weight_fns("binary", FALSE)$omega,  inv_sq_gap_weights)
  expect_identical(.default_weight_fns("binary", TRUE)$lambda,  inv_logit_weights)
  expect_identical(.default_weight_fns("binary", TRUE)$omega,   inv_sq_weights)
  expect_identical(.default_weight_fns("regression", FALSE)$lambda, softmax_weights)
  expect_identical(.default_weight_fns("regression", FALSE)$omega,  softmax_weights)
})

test_that(".default_metric follows the task/prob grid (built-in metrics)", {
  expect_identical(.default_metric("binary", FALSE),     .metric_accuracy)
  expect_identical(.default_metric("binary", TRUE),      .metric_brier)
  expect_identical(.default_metric("regression", FALSE), .metric_rmse)
  # built-in metrics carry an orientation for the validity check
  expect_identical(.metric_direction(.metric_accuracy), "maximize")
  expect_identical(.metric_direction(.metric_brier),    "minimize")
  expect_identical(.metric_direction(.metric_rmse),     "minimize")
})

# ---- Pipeline: lambdaCalc (simplex) / omegaCalc (min-max) --------------------

test_that("lambdaCalc projects onto the simplex; omegaCalc min-max scales", {
  specs   <- .build_specs(iris_binary(), Species ~ ., task = "binary")
  metrics <- c(0.6, 0.75, 0.9)

  lam <- lambdaCalc(specs, metrics)
  expect_length(lam, 3)
  expect_equal(sum(lam), 1)             # hard restriction
  expect_true(all(lam >= 0))

  om <- omegaCalc(specs, metrics)
  expect_length(om, 3)
  expect_equal(range(om), c(0, 1))      # min-max scaled to [0, 1]
})

# ---- Orientation validity ---------------------------------------------------

test_that("a metric/weight-fn orientation mismatch is rejected at build time", {
  # accuracy is maximize; inv_logit_weights is minimize -> incompatible pair.
  expect_error(
    .build_specs(iris_binary(), Species ~ ., task = "binary",
                 lambdaMetric = .metric_accuracy,
                 lambdaFunction = inv_logit_weights),
    "orient"
  )
})

# ---- Argument threading (beta may differ per stage) -------------------------

test_that("lambdaArgs / omegaArgs thread pre-bound arguments (e.g. beta)", {
  specs <- .build_specs(mtcars, mpg ~ ., task = "regression",
                        lambdaArgs = list(beta = 1),
                        omegaArgs  = list(beta = 3))
  expect_identical(specs@lambdaArgs, list(beta = 1))
  expect_identical(specs@omegaArgs,  list(beta = 3))
  # different beta -> different omega vs lambda weighting for the same metrics
  m <- .minmax(c(1, 2, 4))
  expect_false(isTRUE(all.equal(
    do.call(specs@omegaFunction,  c(list(m), specs@omegaArgs)),
    do.call(specs@lambdaFunction, c(list(m), specs@lambdaArgs))
  )))
})
