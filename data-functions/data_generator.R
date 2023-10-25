#' Binary Data Generation Function
#'
#' This function simulates a dataset containing two independent variables (`X^1` and `X^2`) and
#' a binary response variable (`y`), using specified parameters for each variable.
#'
#' @param n Number of observations in the dataset.
#' @param b1 Coefficient for the first independent variable (`X^1`).
#' @param b2 Coefficient for the second independent variable (`X^2`).
#' @param x1_mean Mean value for the first independent variable (`X^1`).
#' @param x1_mean_coef Coefficient to adjust the mean of `X^1`.
#' @param x1_std Standard deviation for the first independent variable (`X^1`).
#' @param x2_mean Mean value for the second independent variable (`X^2`).
#' @param x2_std Standard deviation for the second independent variable (`X^2`).
#' @param e_mean Mean of the error term.
#' @param e_std Standard deviation of the error term.
#' 
#' @return A data frame with columns `X^1`, `X^2`, and `y`.
#' @export
data_gen_bin <- function(n = 100,
                         b1 = 1,
                         b2 = -0.5,
                         x1_mean = 100,
                         x1_mean_coef = 1,
                         x1_std = 25,
                         x2_mean = 200,
                         x2_std = 40,
                         e_mean = 10,
                         e_std = 5
                         ) {
  x1 = rnorm(n, x1_mean * x1_mean_coef, x1_std)
  x2 = rnorm(n, x2_mean, x2_std)
  e = rnorm(n, e_mean, e_std)
  pi = exp(b1 * x1 + b2 * x2 + e) / (1 + exp(b1 * x1 + b2 * x2 + e))
  y = rbinom(n, 1, prob = pi)
  stream_data <- data.frame(`X^1` = x1, `X^2` = x2, y = y)
  return(stream_data)
}
