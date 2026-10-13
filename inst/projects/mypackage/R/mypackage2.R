#' Summarise a cleaned data list
#'
#' @param data A list as returned by \code{\link{clean_data}}.
#' @return A list with elements \code{mean}, \code{sd}, and \code{n}.
#' @export
summarise_data <- function(data) {
  list(mean = mean(data$raw), sd = sd(data$raw), n = data$n)
}

#' Produce a formatted text report from raw values
#'
#' Chains \code{\link{load_data}}, \code{\link{clean_data}}, and
#' \code{\link{summarise_data}} into a single human-readable string.
#'
#' @param values A numeric vector of raw values.
#' @return A character scalar of the form \code{"n=N, mean=X.XX, sd=X.XX"}.
#' @export
report <- function(values) {
  s <- summarise_data(clean_data(load_data(values)))
  sprintf("n=%d, mean=%.2f, sd=%.2f", s$n, s$mean, s$sd)
}
