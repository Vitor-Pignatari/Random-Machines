test_that("random_machines() builds the correct ArgSpecs subclass", {

  df <- iris_binary()

  specs <- random_machines(
    data    = df,
    formula = Species ~ .,
    task    = "binary"
  )

  expect_s4_class(specs, "ArgSpecsBinary")
  expect_identical(specs@task, "binary")
})

test_that("random_machines() enquotes `data` as a symbol", {

  df <- iris_binary()

  specs <- random_machines(df, formula = Species ~ ., task = "binary")

  # `data` is stored as the name `df`, not the data frame itself.
  expect_true(is.name(specs@data))
  expect_identical(deparse(specs@data), "df")

  # ...and it re-evaluates to the original frame on demand.
  expect_equal(
    nrow(eval(specs@data, envir = environment(specs@formula))),
    nrow(df)
  )
})

test_that("random_machines() dispatches on task and validates the response", {

  # task drives which subclass is built
  expect_s4_class(
    random_machines(iris, formula = Species ~ ., task = "multiclass"),
    "ArgSpecsMultiClass"
  )

  # a task/response mismatch is rejected by ArgSpecs validity
  expect_error(
    random_machines(iris, formula = Species ~ ., task = "regression"),
    "not compatible"
  )
})
