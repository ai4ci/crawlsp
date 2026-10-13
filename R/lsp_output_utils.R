.array_to_df = function(array, converter_fn, ...) {
  dplyr::bind_rows(lapply(array, converter_fn, ...))
}

# Convert a single symbol_information node to a data frame.
# These are the usual output of DocumentSymbols or WorkspaceSymbols requests
.symbol_information_to_df = function(symbol_information, dir) {
  if (length(symbol_information) == 0) {
    tmp = dplyr::tibble(
      location = .location_to_df(list(), dir),
      name = character(),
      kind = integer(),
      contained_in = character()
    )
  } else {
    tmp = dplyr::tibble(
      location = .location_to_df(symbol_information$location, dir),
      name = purrr::pluck(symbol_information, "name", .default = NA),
      kind = purrr::pluck(symbol_information, "kind", .default = NA),
      contained_in = purrr::pluck(
        symbol_information,
        "containerName",
        .default = NA
      )
    )
  }

  kinds = names(SymbolKind)[unlist(SymbolKind)]

  tmp %>%
    dplyr::mutate(kind = kinds[kind]) %>%
    tidyr::unnest(location)
}


# Convert a single `Location` response to a one row dataframe.
# These are the output of FindReferences.
.location_to_df = function(list, dir) {
  if (is.null(list) || length(list) == 0) {
    tmp = dplyr::tibble(
      range = .range_to_df(list()),
      path = character()
    )
  } else {
    tmp = tibble::tibble(
      range = .range_to_df(list$range),
      uri = purrr::pluck(list, "uri", .default = NA)
    )
  }
  tmp %>%
    dplyr::mutate(
      path = purrr::map_chr(uri, ~ fs::path_rel(path_from_uri(.x), dir))
    ) %>%
    dplyr::select(-uri) %>%
    tidyr::unnest(range)
}


# convert a single `Range` object to a one line data frame
.range_to_df = function(range) {
  if (is.null(range) || length(range) == 0) {
    return(dplyr::tibble(
      start_line = integer(),
      start_character = integer(),
      end_line = integer(),
      end_character = integer()
    ))
  }
  tibble::tibble(
    start_line = purrr::pluck(range, "start", "line", .default = NA),
    start_character = purrr::pluck(
      range,
      "start",
      "character",
      .default = NA
    ),
    end_line = purrr::pluck(range, "end", "line", .default = NA),
    end_character = purrr::pluck(
      range,
      "end",
      "character",
      .default = NA
    )
  )
}
