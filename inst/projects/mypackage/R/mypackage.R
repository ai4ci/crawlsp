#' Load raw data into a simple list structure
#'
#' @param values A numeric vector of raw values.
#' @return A list with elements \code{raw} (the input vector) and \code{n}
#'   (its length).
#' @export
load_data <- function(values) {
  list(raw = values, n = length(values))
}

#' Remove missing values from a data list
#'
#' @param data A list as returned by \code{\link{load_data}}.
#' @return A list of the same shape with \code{NA} values removed and
#'   \code{n} updated accordingly.
#' @export
clean_data <- function(data) {
  cleaned <- data$raw[!is.na(data$raw)]
  list(raw = cleaned, n = length(cleaned))
}
