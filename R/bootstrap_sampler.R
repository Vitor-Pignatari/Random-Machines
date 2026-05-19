# require(dplyr)
# require(purrr)

# =============================  Bootstrap =============================

#' Create multiple bootstrap samples from a database, reparting it with training and test samples
#'
#' @param data the database that will be used to create the bootstrap samples
#' @param n number of bootstrap samples that will be created
#'
#' @return a list
#' @importFrom purrr map
#'
#' @examples
#'
sample_bootstrap <- function(data, n = 100) {
  n_rows <- nrow(data)
  map(1:n, function(i) {
    indices <- sample(1:n_rows, size = n_rows, replace = TRUE)
    list(train = indices, test = setdiff(1:n_rows, indices))
  })
}
