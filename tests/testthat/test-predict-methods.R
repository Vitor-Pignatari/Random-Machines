# predict() for BootOmegas / RandomMachines. Ensembles are built via the shared
# build_ensemble() helper (see helper-fixtures.R). Per Decision K1, BootOmegas no
# longer stores specs, so predict(BootOmegas, ...) takes `specs`.

test_that("BootOmegas holds finite (no longer Inf) omegas", {
  d <- iris_binary()
  specs <- .build_specs(d, Species ~ ., task = "binary", prob = FALSE, B = 15)
  bo <- build_ensemble(specs, d)

  expect_s4_class(bo, "BootOmegas")
  expect_true(all(is.finite(bo@bootOmegas)))
})

test_that("predict(BootOmegas) does weighted majority vote for binary", {
  set.seed(1)
  sp <- shuffle_split(iris_binary(), 70)
  tr <- sp$train; te <- sp$test

  specs <- .build_specs(tr, Species ~ ., task = "binary", prob = FALSE, B = 15)
  bo <- build_ensemble(specs, tr)

  pred <- predict(bo, te, specs = specs)
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(te))
  expect_setequal(levels(pred), levels(tr$Species))
})

test_that("predict(BootOmegas) averages probabilities for binary prob", {
  set.seed(2)
  sp <- shuffle_split(iris_binary(), 70)
  tr <- sp$train; te <- sp$test

  specs <- .build_specs(tr, Species ~ ., task = "binary", prob = TRUE, B = 15)
  bo <- build_ensemble(specs, tr)

  pred <- predict(bo, te, specs = specs)
  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(te), 2))
  expect_setequal(colnames(pred), levels(tr$Species))
  expect_true(all(abs(rowSums(pred) - 1) < 1e-6))  # weighted average stays a distribution
})

test_that("predict(BootOmegas) handles multiclass vote and probability average", {
  set.seed(3)
  sp <- shuffle_split(iris, 110)
  tr <- sp$train; te <- sp$test

  specs_vote <- .build_specs(tr, Species ~ ., task = "multiclass", prob = FALSE, B = 12)
  vote <- build_ensemble(specs_vote, tr)
  pv <- predict(vote, te, specs = specs_vote)
  expect_s3_class(pv, "factor")
  expect_length(pv, nrow(te))

  specs_prob <- .build_specs(tr, Species ~ ., task = "multiclass", prob = TRUE, B = 12)
  prob <- build_ensemble(specs_prob, tr)
  pp <- predict(prob, te, specs = specs_prob)
  expect_true(is.matrix(pp))
  expect_equal(dim(pp), c(nrow(te), 3))
  expect_true(all(abs(rowSums(pp) - 1) < 1e-6))
})

test_that("predict(BootOmegas) does a weighted mean for regression", {
  set.seed(4)
  d  <- mtcars[sample(nrow(mtcars)), ]
  tr <- d[1:24, ]; te <- d[25:32, ]

  specs <- .build_specs(
    tr, mpg ~ ., task = "regression", B = 15,
    lambdaMetric = .metric_rmse,
    omegaMetric  = .metric_rmse
  )
  bo <- build_ensemble(specs, tr)

  pred <- predict(bo, te, specs = specs)
  expect_type(pred, "double")
  expect_length(pred, nrow(te))
})

test_that("predict(BootOmegas) guards bad newdata", {
  d <- iris_binary()
  specs <- .build_specs(d, Species ~ Sepal.Length + Sepal.Width,
                        task = "binary", prob = FALSE, B = 10)
  bo <- build_ensemble(specs, d)

  expect_error(predict(bo, iris[0, ], specs = specs), "non-empty data.frame")
  expect_error(predict(bo, iris["Petal.Length"], specs = specs), "missing predictor")
})
