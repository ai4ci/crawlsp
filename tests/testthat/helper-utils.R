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


symbol_range <- function(symbol) {
  if (!is.null(symbol$location)) {
    return(symbol$location$range)
  }
  symbol$range
}

test_language_client <- function(...) {
  if (nzchar(Sys.getenv("R_LANGSVR_LOG"))) {
    rcommand = sprintf(
      "options(languageserver.formatting_style = NULL); languageserver::run(debug = '%s')",
      normalizePath(Sys.getenv("R_LANGSVR_LOG"), "/", mustWork = FALSE)
    )
  } else {
    rcommand = "options(languageserver.formatting_style = NULL); languageserver::run()"
  }
  args = c("--no-echo", "-e", rcommand)
  client = language_client(
    cmd = file.path(R.home("bin"), "R"),
    args = args,
    ...
  )
  withr::defer_parent({
    client %>% respond("shutdown", NULL, retry = FALSE)
    if (client$process$is_alive()) {
      if (identical(Sys.getenv("R_COVR"), "true")) {
        client$process$wait()
      } else {
        client$process$wait(1000)
        client$process$kill()
      }
    }
  })
  client
}
