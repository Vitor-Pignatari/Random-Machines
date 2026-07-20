test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


model = 1

models <- list(
  "rbf"      = list(x = Species ~ ., kernel = rbfdot(10)),
  "laplace"  = list(x = Species ~ ., kernel = rbfdot(10)),
  "linear"   = list(x = Species ~ ., kernel = rbfdot(10))
)

##
fold <- sample(1:nrow(iris), size = 30)
##

# em loop pra cada fold:
args <- models[[model]]
args[["data"]] <- iris[fold, ]
model_fold <- do.call(kernlab::ksvm, args)
prd <- predict(model_fold, iris[-fold, ])

