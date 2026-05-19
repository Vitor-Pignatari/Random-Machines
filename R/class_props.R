#' Compute per-partition class proportions, preserving zero-count classes using
#' tidyr::complete. Optionally return a wide matrix of class proportions.
#'
#' @param df input data (must include partition and y)
#' @param part tidyselect for partition column
#' @param y tidyselect for class column
#' @param wide if TRUE, pivot to wide class-proportion columns
#'
#' @return
#' @importFrom dplyr mutate count group_by select
#' @importFrom tidyr complete pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
class_props <- function(df, part = partition, y = y, wide = FALSE) {
  # save the levels of y (to include classes with zero in the partition)
  y_levels <- levels(factor(dplyr::pull(df, {{ y }})))

  out <- df %>%
    mutate({{ y }} := factor({{ y }}, levels = y_levels)) %>%
    count({{ part }}, {{ y }}, name = "n") %>%
    group_by({{ part }}) %>%
    complete({{ y }}, fill = list(n = 0)) %>%   # ensure classes with n=0
    mutate(total = sum(n), prop = n / total) %>%
    ungroup()

  if (wide) {
    out <- out %>%
      select(-total, -n) %>%
      pivot_wider(names_from = {{ y }}, values_from = prop, values_fill = 0)
  }
  out
}
