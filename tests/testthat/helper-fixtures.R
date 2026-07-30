# Shared test fixtures, auto-sourced by testthat before every test file.
# Keep RNG-dependent shuffling *inside* the tests (so seeds stay local); these
# helpers only build deterministic data frames and reusable ensemble scaffolding.

# The two-class slice of iris used by every binary-classification test.
iris_binary <- function() {
  droplevels(iris[iris$Species %in% c("setosa", "versicolor"), ])
}

# Fit the first kernel of a spec on the full data, to exercise a real kernlab
# model (used by the svmPredict dispatch tests).
fit_first <- function(specs, data) {
  calls <- .call_builder(specs)
  eval(rlang::call_modify(calls[[1]], data = data, fit = FALSE))
}

# Build a BootOmegas ensemble for a given spec + training data.
build_ensemble <- function(specs, data, lambdas) {
  svmcalls <- .call_builder(specs)
  boot <- BootSamples(
    trainData = data,
    bootFun   = simple_bs,
    bootArgs  = list(indexes = seq_len(nrow(data)), B = specs@B)
  )
  BootOmegas(specs, bootData = boot, svmcalls = svmcalls, lambdas = lambdas)
}
