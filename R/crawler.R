.data = rlang::.data

options("crawlsp.debug" = TRUE)


#
# dir
#' Title
#'
#' @param dir
#'
#' @returns
#' @export
#'
#' @examples
#' devtools::load_all()
#' dir = system.file("projects/mypackage", package="crawlsp")
#' dir = system.file("projects/hello-java", package="crawlsp")
#' dir = system.file("projects/hello-markdown", package="crawlsp")
crawl = function(dir) {
  # Setup LSP config from the if it does not already exist:
  cfg_path = fs::path(rappdirs_user_config_dir("crawlsp"), "config.json")
  if (!fs::file_exists(cfg_path)) {
    fs::dir_create(rappdirs_user_config_dir("crawlsp"))
    fs::file_copy(
      system.file("data/config.json", package = "crawlsp"),
      cfg_path
    )
  }

  # The LSP configuration file.
  config = jsonlite::read_json(cfg_path)

  # For each server identify files in the project that match their glob
  # These will filter the files in the subdirectory to just ones the servers
  # understand
  used = lapply(config$servers, function(x) {
    x$files = .fs_glob(dir, globs = x$fileGlobs)
    x
  })
  used = Filter(function(x) length(x$files) > 0, used)

  # The unmatched files are for debugging.
  unmatched = fs::dir_ls(
    dir,
    recurse = TRUE,
    type = "file"
  )
  unmatched = Reduce(function(u, v) setdiff(u, v$files), init = unmatched, used)

  # ----

  for (i in seq_along(used)) {
    language = names(used)[[i]]
    server = used[[i]]

    # Marksman needs a specific file to be present to identify the file root
    if (language == "markdown") {
      fs::file_touch(fs::path(dir, ".marksman.toml"))
    }

    cmd = as.character(unlist(server$command[1]))
    args = as.character(unlist(tail(server$command, -1)))

    client = language_client(cmd = cmd, args = args, working_dir = dir)
    # .tree(client$ServerCapabilities)

    # scan the whole set of files for this directory...
    for (f in server$files) {
      client %>% did_open(f)
    }

    # NODES
    ws_symbols = c(
      respond_workspace_symbol(client, "*"),
      respond_workspace_symbol(client, "")
    )

    # EDGES
    sym_refs = .array_to_df(
      ws_symbols,
      .symbol_information_to_df,
      dir = dir
    ) %>%
      dplyr::mutate(
        refs = purrr::map(
          purrr::map2(
            # execute the find references query for each symbol
            start_line,
            start_character,
            ~ respond_references(client, defn_file, c(.x, .y))
          ),
          # reformat the list of locations output to a dataframe
          ~ .array_to_df(.x, .location_to_df, dir = dir)
        )
      ) %>%
      tidyr::unnest(refs, keep_empty = FALSE, names_sep = ".")

    for (defn_file in test$files) {
      client %>% did_open(defn_file)

      # NODES
      doc_symbols = respond_document_symbol(client, defn_file)

      # EDGES
      # Gives a set of edges to references in code but only for classes in java
      doc_refs = .array_to_df(
        doc_symbols,
        .symbol_information_to_df,
        dir = dir
      ) %>%
        dplyr::mutate(
          refs = purrr::map(
            purrr::map2(
              # execute the find references query for each symbol
              start_line,
              start_character,
              ~ respond_references(client, defn_file, c(.x, .y))
            ),
            # reformat the list of locations output to a dataframe
            ~ .array_to_df(.x, .location_to_df, dir = dir)
          )
        ) %>%
        tidyr::unnest(refs, keep_empty = FALSE, names_sep = ".")

      # FILE TO FILE LINKS
    }
  }
  # ----

  csv_path = fs::path(dir, ".agents/crawlsp")
  data_path = rappdirs_user_cache_dir("crawlsp")
  db_path = fs::path(data_path, "duckdb")

  # dir = "~/Git/crawlsp"

  fs::dir_create(data_path)
  fs::dir_create(csv_path)

  con = DBI::dbConnect(duckdb::duckdb(dbdir = fs::path(db_path)))

  # sapply(DBI::dbListTables(con), function(x) DBI::dbRemoveTable(con, x))
}


# Get a recursive file listing matching set of globs
.fs_glob = function(dir, globs) {
  unlist(
    sapply(globs, function(g) {
      fs::dir_ls(dir, recurse = TRUE, type = "file", glob = fs::path(dir, g))
    }),
    use.names = FALSE
  )
}
