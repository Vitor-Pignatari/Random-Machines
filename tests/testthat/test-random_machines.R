# .build_specs() is the internal spec builder; random_machines() is the public
# verb that builds *and* fits. The spec stores its training data, not a symbol.

test_that(".build_specs() dispatches on task, stores data, validates the response", {
  df    <- iris_binary()
  specs <- .build_specs(df, formula = Species ~ ., task = "binary")
  expect_s4_class(specs, "ArgSpecsBinary")
  expect_identical(specs@task, "binary")
  # `data` is a self-contained data.frame, so the spec survives saveRDS/reload
  expect_s3_class(specs@data, "data.frame")
  expect_equal(nrow(specs@data), nrow(df))
  expect_true("Species" %in% names(specs@data))

  expect_s4_class(.build_specs(iris, Species ~ ., task = "multiclass"),
                  "ArgSpecsMultiClass")
  # a task/response mismatch is rejected by subclass validity
  expect_error(.build_specs(iris, Species ~ ., task = "regression"), "not compatible")
})

# ---- Paper defaults (Ara et al. 2021; Ara et al. 2022) ----------------------
# Both articles fix the hyperparameters at: four kernels (linear, polynomial
# d = 2, gaussian, laplacian), gamma = 1, C = 1, epsilon = 0.1, B = 100.

test_that("default kernel set and hyperparameters follow the RM papers", {
  specs <- .build_specs(iris_binary(), Species ~ ., task = "binary")

  expect_identical(specs@kernels, c("rbf", "laplace", "poly2", "linear"))
  expect_identical(specs@B, 100L)

  for (k in specs@kernels) {
    expect_equal(specs@args[[k]]$C, 1)
    expect_equal(specs@args[[k]]$epsilon, 0.1)
  }
  expect_equal(kernlab::kpar(specs@args$rbf$kernel)$sigma, 1)
  expect_equal(kernlab::kpar(specs@args$laplace$kernel)$sigma, 1)
  pk <- kernlab::kpar(specs@args$poly2$kernel)
  expect_equal(pk$degree, 2)
  expect_equal(pk$scale, 1)
  expect_equal(pk$offset, 0)
  expect_s4_class(specs@args$linear$kernel, "vanillakernel")
})

test_that("data and formula are required (no toy-data defaults)", {
  expect_error(.build_specs(task = "binary"))
  expect_error(random_machines(task = "binary"))
})

test_that("lambda stage defaults to a single 75/25 holdout (papers' Algorithm 1)", {
  expect_identical(eval(formals(random_machines)$K), 1)
  expect_identical(eval(formals(RandomMachines)$K), 1)

  set.seed(42)
  rm <- random_machines(iris_binary(), Species ~ ., task = "binary", B = 5,
                        store.resamples = TRUE)  # keep the resamples to inspect
  tr <- rm@kernelSamples@data$train
  expect_identical(ncol(tr), 1L)                       # one split, not K folds
  expect_equal(mean(tr[, 1]), 0.75, tolerance = 0.02)  # ~75% training rows
  # store.resamples = TRUE retains both resample payloads
  expect_named(rm@kernelSamples@data, c("train", "test"))
  expect_named(rm@bootSamples@bootData, c("train", "test"))
  expect_identical(ncol(rm@bootSamples@bootData$train), 5L)  # one column per replicate
})

test_that("store.resamples = FALSE (default) clears the resample matrices", {
  set.seed(43)
  df <- iris_binary()
  rm <- random_machines(df, Species ~ ., task = "binary", B = 5)

  # diagnostic payloads cleared; predict() only needs specs + bootOmegas
  expect_length(rm@kernelSamples@data, 0)
  expect_length(rm@bootSamples@bootData, 0)
  # the stratification vector is not retained either (response lives in specs@data)
  expect_false("y" %in% names(rm@kernelSamples@splitargs))

  pred <- predict(rm, df)
  expect_length(pred, nrow(df))
})

test_that("formulas with transformed terms fit and predict", {
  set.seed(6)
  rm <- random_machines(mtcars, log(mpg) ~ log(hp) + wt, task = "regression", B = 5)
  # the spec keeps the raw variables, so the formula can be re-evaluated
  expect_true(all(c("mpg", "hp", "wt") %in% names(rm@specs@data)))
  pred <- predict(rm, mtcars)
  expect_length(pred, nrow(mtcars))
  expect_true(all(is.finite(pred)))
  # predictions are on the model's (log) scale
  expect_lt(abs(mean(pred) - mean(log(mtcars$mpg))), 0.5)
})

test_that("predictions keep the training response's level order", {
  set.seed(7)
  d <- data.frame(x1 = c(rnorm(30, 0), rnorm(30, 4)), x2 = rnorm(60),
                  y  = factor(rep(c("low", "high"), each = 30),
                              levels = c("low", "high")))
  hard <- predict(random_machines(d, y ~ ., task = "binary", B = 5), d)
  expect_identical(levels(hard), c("low", "high"))
  prob <- predict(random_machines(d, y ~ ., task = "binary", prob = TRUE, B = 5), d)
  expect_identical(colnames(prob), c("low", "high"))
})

test_that("multiclass defaults bind the class-count chance level", {
  k3 <- .build_specs(iris, Species ~ ., task = "multiclass")
  expect_equal(k3@lambdaArgs$chance, 1 / 3)
  k3p <- .build_specs(iris, Species ~ ., task = "multiclass", prob = TRUE)
  expect_equal(k3p@lambdaArgs$chance, 2 / 3)
  # binary keeps the paper's transform unchanged (chance 0.5 is the default)
  expect_identical(.build_specs(iris_binary(), Species ~ ., task = "binary")@lambdaArgs,
                   list())
  # a user-supplied chance wins
  own <- .build_specs(iris, Species ~ ., task = "multiclass",
                      lambdaArgs = list(chance = 0.4))
  expect_equal(own@lambdaArgs$chance, 0.4)
})
