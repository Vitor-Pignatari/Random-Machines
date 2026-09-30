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

test_that("transforms stay finite at the [0, 1] endpoints (eps domain guard)", {
  ends <- c(0, 1)                       # a perfect and a worst-possible score
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

test_that(".to_simplex normalizes, zeroes negatives/non-finites, and falls back to uniform", {
  # all-positive raw weights: plain division by the sum
  expect_equal(.to_simplex(c(2, 1, 1)), c(0.5, 0.25, 0.25))
  # a negative raw weight (a below-chance kernel under the logit transform) is
  # zeroed, not shifted: the ratios among the remaining kernels stay faithful
  # to Eq. (8) of Ara et al. (2021)
  expect_equal(.to_simplex(c(2, 1, -1, 0.5)), c(2, 1, 0, 0.5) / 3.5)
  # nothing positive -> uniform fallback
  expect_equal(.to_simplex(c(0, 0, 0)), rep(1 / 3, 3))
  expect_equal(.to_simplex(c(-1, -2)), rep(1 / 2, 2))
  # non-finite entries (a misbehaving user-supplied transform) are neutralised;
  # .to_simplex also renormalizes the stored omegas at predict time
  w <- .to_simplex(c(Inf, 5, 10, -3))
  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1)
  expect_equal(w[1], 0)                                    # Inf neutralised
})

# ---- Selection grid ---------------------------------------------------------

test_that(".task_defaults pairs each cell's metric with matching transforms", {
  expect_identical(.task_defaults("binary", FALSE)$metric, .metric_accuracy)
  expect_identical(.task_defaults("binary", FALSE)$lambda, logit_weights)
  expect_identical(.task_defaults("binary", FALSE)$omega,  inv_sq_gap_weights)
  expect_identical(.task_defaults("binary", TRUE)$metric,  .metric_brier)
  expect_identical(.task_defaults("binary", TRUE)$lambda,  inv_logit_weights)
  expect_identical(.task_defaults("binary", TRUE)$omega,   inv_sq_weights)
  expect_identical(.task_defaults("regression", FALSE)$metric, .metric_rmse)
  expect_identical(.task_defaults("regression", FALSE)$lambda, softmax_weights)
  expect_identical(.task_defaults("regression", FALSE)$omega,  softmax_weights)
})

# lambdaCalc()/omegaCalc() project onto the simplex: the paper-fidelity tests
# below check their exact normalized outputs.

# ---- Regression paper fidelity (Ara et al. 2022, ESWA 202:117107) -----------

test_that("softmax_weights standardizes by sd and defaults to beta = 2", {
  d <- c(19, 21, 24, 30)
  expect_equal(softmax_weights(d), exp(-2 * d / sd(d)))
  # scale invariance: the units of y must not change the weights
  expect_equal(softmax_weights(d * 100), softmax_weights(d))
  expect_equal(softmax_weights(d / 100), softmax_weights(d))
  # degenerate spread (sd = 0 or NA): equal, finite weights
  expect_true(all(is.finite(softmax_weights(rep(3, 4)))))
  expect_equal(diff(range(softmax_weights(rep(3, 4)))), 0)
  expect_true(is.finite(softmax_weights(5)))
})

test_that("regression lambdas match paper Eq. (1)", {
  specs <- .build_specs(mtcars, mpg ~ ., task = "regression")
  d     <- c(19, 21, 24, 30)
  ref   <- exp(-2 * d / sd(d)) / sum(exp(-2 * d / sd(d)))
  expect_equal(lambdaCalc(specs, d), ref)
  expect_equal(lambdaCalc(specs, d * 100), lambdaCalc(specs, d))
})

test_that("regression omegas are normalized standardized softmax (Eq. (2))", {
  specs <- .build_specs(mtcars, mpg ~ ., task = "regression")
  D     <- c(3.2, 3.5, 4.0, 5.0)
  ref   <- exp(-2 * D / sd(D)) / sum(exp(-2 * D / sd(D)))
  expect_equal(omegaCalc(specs, D), ref)
})

# ---- Binary paper fidelity (Ara et al. 2021, J. Data Sci. 19(3):409-428) ----

test_that("binary lambdas match paper Eq. (8) when all kernels beat chance", {
  specs <- .build_specs(iris_binary(), Species ~ ., task = "binary")
  acc   <- c(0.9, 0.8, 0.7, 0.6)
  logit <- log(acc / (1 - acc))
  expect_equal(lambdaCalc(specs, acc), logit / sum(logit))
})

test_that("a below-chance kernel gets selection probability 0 (Eq. (8) guard)", {
  specs <- .build_specs(iris_binary(), Species ~ ., task = "binary")
  acc   <- c(0.9, 0.8, 0.4)
  lam   <- lambdaCalc(specs, acc)
  expect_equal(lam[3], 0)
  pos <- log(acc[1:2] / (1 - acc[1:2]))
  expect_equal(lam[1:2], pos / sum(pos))   # survivors keep Eq. (8) ratios
})

test_that("binary omegas are normalized 1/(1 - acc)^2 (Eq. (9))", {
  specs <- .build_specs(iris_binary(), Species ~ ., task = "binary")
  acc   <- c(0.9, 0.8, 0.7)
  ref   <- (1 / (1 - acc)^2) / sum(1 / (1 - acc)^2)
  expect_equal(omegaCalc(specs, acc), ref)
})

# ---- Chance level (multiclass extension; binary unchanged) ------------------

test_that("chance-centered logit: default 0.5 is the paper's transform", {
  acc <- c(0.9, 0.8, 0.7, 0.6)
  expect_equal(logit_weights(acc), log(acc / (1 - acc)))
  expect_equal(inv_logit_weights(1 - acc), log(acc / (1 - acc)))
  # a chance-level score maps to exactly 0
  expect_equal(logit_weights(0.2, chance = 0.2), 0)
  expect_equal(inv_logit_weights(2 / 3, chance = 2 / 3), 0)
})

test_that("5-class lambdas discriminate kernels that are below .5 but above chance", {
  specs <- .build_specs(.make_k_class(5), y ~ ., task = "multiclass")
  lam   <- lambdaCalc(specs, c(0.45, 0.40, 0.35, 0.30))  # chance = 0.2
  expect_true(all(diff(lam) < 0))                        # ordered, not uniform
  expect_equal(sum(lam), 1)
  # a below-chance kernel is still zeroed
  expect_equal(lambdaCalc(specs, c(0.45, 0.15))[2], 0)
})

test_that("3-class probabilistic lambdas keep better-than-chance Brier kernels", {
  specs <- .build_specs(iris, Species ~ ., task = "multiclass", prob = TRUE)
  # uniform prediction scores 2/3; all four kernels beat it but exceed 0.5
  lam <- lambdaCalc(specs, c(0.55, 0.60, 0.62, 0.64))
  expect_true(all(lam > 0))
  expect_true(all(diff(lam) < 0))
  # at or above chance -> zero
  expect_equal(lambdaCalc(specs, c(0.3, 0.7))[2], 0)
})

test_that("inv_sq_weights keeps the ordering of Brier scores above 1", {
  w <- inv_sq_weights(c(0.5, 1.2, 1.8))
  expect_true(all(is.finite(w)))
  expect_true(all(diff(w) < 0))
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

test_that(".metric_direction: attribute wins, bare metrics are probed, else NA", {
  bare_acc <- function(truth, estimate) mean(truth == estimate)
  bare_err <- function(truth, estimate) mean(truth != estimate)
  truth <- factor(c(1, 2, 1, 2))
  good  <- factor(c(1, 2, 2, 2))   # 3/4 correct
  bad   <- factor(c(2, 1, 1, 1))   # 1/4 correct
  # empirical inference from the good/bad probe
  expect_identical(.metric_direction(bare_acc, truth, good, bad), "maximize")
  expect_identical(.metric_direction(bare_err, truth, good, bad), "minimize")
  # an explicit attribute overrides the probe
  expect_identical(.metric_direction(.metric_rmse, truth, good, bad), "minimize")
  # no attribute and no probe -> NA (orientation check skipped)
  expect_identical(.metric_direction(bare_acc), NA_character_)
})

test_that("a misoriented bare metric is caught by the empirical probe", {
  # error rate carries no direction attribute; the probe infers "minimize",
  # which clashes with the maximize-oriented hard-classification defaults.
  err_rate <- function(truth, estimate) mean(truth != estimate)
  expect_error(
    .build_specs(iris_binary(), Species ~ ., task = "binary",
                 lambdaMetric = err_rate),
    "orient"
  )
  # the same bare metric paired with a minimize-oriented function is accepted
  specs <- .build_specs(iris_binary(), Species ~ ., task = "binary",
                        lambdaMetric = err_rate,
                        lambdaFunction = inv_logit_weights)
  expect_s4_class(specs, "ArgSpecsBinary")
})

# ---- Argument threading (beta may differ per stage) -------------------------

test_that("lambdaArgs / omegaArgs thread pre-bound arguments (e.g. beta)", {
  specs <- .build_specs(mtcars, mpg ~ ., task = "regression",
                        lambdaArgs = list(beta = 1),
                        omegaArgs  = list(beta = 3))
  expect_identical(specs@lambdaArgs, list(beta = 1))
  expect_identical(specs@omegaArgs,  list(beta = 3))
  # different beta -> different omega vs lambda weighting for the same metrics
  m <- c(1, 2, 4)
  expect_false(isTRUE(all.equal(
    do.call(specs@omegaFunction,  c(list(m), specs@omegaArgs)),
    do.call(specs@lambdaFunction, c(list(m), specs@lambdaArgs))
  )))
})
