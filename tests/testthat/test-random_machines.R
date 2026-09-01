# .build_specs() is the internal spec builder; random_machines() is the public
# verb that builds *and* fits (Decision B1). data is stored as a resolved model
# frame, not a symbol (Decision A2).

test_that(".build_specs() builds the correct ArgSpecs subclass", {

  df <- iris_binary()

  specs <- .build_specs(
    data    = df,
    formula = Species ~ .,
    task    = "binary"
  )

  expect_s4_class(specs, "ArgSpecsBinary")
  expect_identical(specs@task, "binary")
})

test_that(".build_specs() stores the resolved model frame (A2), not a symbol", {

  df <- iris_binary()

  specs <- .build_specs(df, formula = Species ~ ., task = "binary")

  # `data` is a self-contained data.frame -- survives saveRDS/reload.
  expect_s3_class(specs@data, "data.frame")
  expect_equal(nrow(specs@data), nrow(df))
  expect_true("Species" %in% names(specs@data))
})

test_that(".build_specs() dispatches on task and validates the response", {

  expect_s4_class(
    .build_specs(iris, formula = Species ~ ., task = "multiclass"),
    "ArgSpecsMultiClass"
  )

  # a task/response mismatch is rejected by subclass validity
  expect_error(
    .build_specs(iris, formula = Species ~ ., task = "regression"),
    "not compatible"
  )
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
  rm <- random_machines(iris_binary(), Species ~ ., task = "binary", B = 5)
  tr <- rm@kernelSamples@data$train
  expect_identical(ncol(tr), 1L)                       # one split, not K folds
  expect_equal(mean(tr[, 1]), 0.75, tolerance = 0.02)  # ~75% training rows
})

test_that("random_machines() builds and fits in one call (B1)", {

  set.seed(1)
  df <- iris_binary()

  rm <- random_machines(df, Species ~ ., task = "binary", B = 12, K = 4)

  expect_s4_class(rm, "RandomMachines")
  expect_s4_class(rm@specs, "ArgSpecsBinary")

  pred <- predict(rm, df)
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(df))
})
