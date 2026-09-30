# Shared test fixtures, auto-sourced by testthat before every test file.
# Keep RNG-dependent shuffling *inside* the tests (so seeds stay local); these
# helpers only build deterministic data frames and reusable ensemble scaffolding.

# A two-class slice of iris (default setosa/versicolor, the easy pair).
iris_pair <- function(a = "setosa", b = "versicolor") {
  droplevels(iris[iris$Species %in% c(a, b), ])
}
iris_binary <- function() iris_pair()

# A deterministic k-class data frame (10 rows per class), for specs that need
# a class count other than iris's three.
.make_k_class <- function(k) {
  data.frame(x = seq_len(10 * k), y = factor(rep(letters[seq_len(k)], each = 10)))
}

# Shuffle a data frame and split into train (first `n_train` rows) / test (rest).
# Call after set.seed() in the test to keep the split reproducible.
shuffle_split <- function(df, n_train) {
  df <- df[sample(nrow(df)), , drop = FALSE]
  list(train = df[seq_len(n_train), , drop = FALSE],
       test  = df[-seq_len(n_train), , drop = FALSE])
}

# A KernelSamples over K cross-validation folds of `df` (stratified when `y` is
# given). Used to set up the stage-1 fitting tests.
kernel_samples <- function(df, K, y = NULL) {
  KernelSamples(splitfun  = kfold_cv,
                splitargs = list(n = nrow(df), K = K, y = y))
}

# Fit the first kernel of a spec on the full data, to exercise a real kernlab
# model (used by the svmPredict dispatch tests).
fit_first <- function(specs, data) {
  calls <- .call_builder(specs)
  eval(rlang::call_modify(calls[[1]], data = data, fit = FALSE))
}

# B bootstrap resamples of every row of `data`.
boot_samples <- function(data, B) {
  BootSamples(trainData = data,
              bootArgs  = list(indexes = seq_len(nrow(data)), B = B))
}

# Build a BootOmegas ensemble for a given spec + training data. `lambdas`
# defaults to uniform kernel-selection weights.
build_ensemble <- function(specs, data,
                           lambdas = rep(1 / length(specs@kernels),
                                         length(specs@kernels))) {
  BootOmegas(specs, bootData = boot_samples(data, specs@B),
             svmcalls = .call_builder(specs), lambdas = lambdas)
}
