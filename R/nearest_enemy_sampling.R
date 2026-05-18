

#' Sample rows with probability proportional to an exponential transform of the
# negative nearest-enemy distance. Smaller distances (closer to other-class
# points) receive larger weights after scaling.
#'
#' @param data data.frame with features and y
#' @param y_var name of class column
#' @param final_sample_size number of rows to sample
#' @param alpha temperature/slope for the exponential scaling
#'
#' @return a Nearest-Enemy sampled data.frame
#' @import dplyr sample_n
#'
#' @examples
nearest_enemy_sampling <- function(data, y_var, final_sample_size, alpha = 1) {
  data$nearest_enemy_distance <- nearest_enemy_distance(data, y_var) * (-1)

  sampling_weight <- scale_exponential(
    data$nearest_enemy_distance,
    alpha = alpha
  )

  # data %>%
  #   sample_n(final_sample_size, weight = sampling_weight)

  sample_n(data, final_sample_size, weight = sampling_weight)
}
