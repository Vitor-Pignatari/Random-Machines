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
