suppressPackageStartupMessages({
  library(magrittr)
  library(mockery)
  library(purrr)
  library(fs)
})

# a hack to make withr::defer_parent to work, see https://github.com/r-lib/withr/issues/123
defer <- withr::defer

expect_equivalent <- function(x, y) {
  testthat::expect_equal(x, y, ignore_attr = TRUE)
}
