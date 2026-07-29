# predict() for BootOmegas / RandomMachines. Ensembles are built via the shared
# build_ensemble() helper (see helper-fixtures.R).

test_that("BootOmegas carries specs and finite (no longer Inf) omegas", {
  d <- iris_binary()
  specs <- random_machines(d, Species ~ ., task = "binary", prob = FALSE, B = 15)
  bo <- build_ensemble(specs, d, c(0.34, 0.33, 0.33))

  expect_s4_class(bo@specs, "ArgSpecsBinary")
  expect_true(all(is.finite(bo@bootOmegas)))
})

test_that("predict(BootOmegas) does weighted majority vote for binary", {
  set.seed(1)
  d  <- iris_binary()
  d  <- d[sample(nrow(d)), ]
  tr <- d[1:70, ]; te <- d[71:100, ]

  specs <- random_machines(tr, Species ~ ., task = "binary", prob = FALSE, B = 15)
  bo <- build_ensemble(specs, tr, c(0.34, 0.33, 0.33))

  pred <- predict(bo, te)
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(te))
  expect_setequal(levels(pred), levels(tr$Species))
})

test_that("predict(BootOmegas) averages probabilities for binary prob", {
  set.seed(2)
  d  <- iris_binary()
  d  <- d[sample(nrow(d)), ]
  tr <- d[1:70, ]; te <- d[71:100, ]

  specs <- random_machines(tr, Species ~ ., task = "binary", prob = TRUE, B = 15)
  bo <- build_ensemble(specs, tr, c(0.34, 0.33, 0.33))

  pred <- predict(bo, te)
  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(te), 2))
  expect_setequal(colnames(pred), levels(tr$Species))
  expect_true(all(abs(rowSums(pred) - 1) < 1e-6))  # weighted average stays a distribution
})

test_that("predict(BootOmegas) handles multiclass vote and probability average", {
  set.seed(3)
  d  <- iris[sample(nrow(iris)), ]
  tr <- d[1:110, ]; te <- d[111:150, ]

  vote <- build_ensemble(
    random_machines(tr, Species ~ ., task = "multiclass", prob = FALSE, B = 12),
    tr, c(0.34, 0.33, 0.33)
  )
  pv <- predict(vote, te)
  expect_s3_class(pv, "factor")
  expect_length(pv, nrow(te))

  prob <- build_ensemble(
    random_machines(tr, Species ~ ., task = "multiclass", prob = TRUE, B = 12),
    tr, c(0.34, 0.33, 0.33)
  )
  pp <- predict(prob, te)
  expect_true(is.matrix(pp))
  expect_equal(dim(pp), c(nrow(te), 3))
  expect_true(all(abs(rowSums(pp) - 1) < 1e-6))
})

test_that("predict(BootOmegas) does a weighted mean for regression", {
  set.seed(4)
  d  <- mtcars[sample(nrow(mtcars)), ]
  tr <- d[1:24, ]; te <- d[25:32, ]

  specs <- random_machines(
    tr, mpg ~ ., task = "regression", B = 15,
    lambdaMetric = yardstick::rmse_vec,
    omegaMetric  = yardstick::rmse_vec
  )
  bo <- build_ensemble(specs, tr, c(0.34, 0.33, 0.33))

  pred <- predict(bo, te)
  expect_type(pred, "double")
  expect_length(pred, nrow(te))
})

test_that("predict(BootOmegas) guards bad newdata", {
  d <- iris_binary()
  specs <- random_machines(d, Species ~ Sepal.Length + Sepal.Width,
                           task = "binary", prob = FALSE, B = 10)
  bo <- build_ensemble(specs, d, c(0.34, 0.33, 0.33))

  expect_error(predict(bo, iris[0, ]), "non-empty data.frame")
  expect_error(predict(bo, iris["Petal.Length"]), "missing predictor")
})
