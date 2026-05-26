.data = rlang::.data

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

  # For each server identify files in the project that match their regex
  # These will filter the files in the subdirectory to just ones the servers
  # understand
  used = lapply(config$servers, function(x) {
    x$files = fs::dir_ls(
      dir,
      recurse = TRUE,
      type = "file",
      regexp = x$pathRegex
    )
    x
  })
  used = Filter(function(x) length(x$files) > 0, used)

  # Check server exists
  used = Filter(
    function(x) {
      cmd = x$command[1]
      exists = Sys.which(cmd) != ""
      if (!exists) {
        warning(
          "Cannot find required language server: ",
          cmd,
          "; install it from: " + x$url
        )
      }
      return(exists)
    },
    used
  )

  # The unmatched files are for debugging.
  unmatched = fs::dir_ls(
    dir,
    recurse = TRUE,
    type = "file"
  )
  unmatched = Reduce(function(u, v) setdiff(u, v$files), init = unmatched, used)

  # ----
  ## Testing:
  ## Lets start a LSP and see that the output is

  client = language_client(used$r$command, working_dir = getwd())
  defn_file = used$r$files[[1]]
  client %>% did_open(defn_file)
  tmp = respond_document_symbol(client, used$r$files[[1]])
  fn = tmp[[4]] #get_tasks
  pos = c(fn$location$range$start$line, fn$location$range$start$character)
  tmp2 = respond_references(client, defn_file, pos)

  # ----

  csv_path = fs::path(dir, ".agents/crawlsp")
  data_path = rappdirs_user_cache_dir("crawlsp")
  db_path = fs::path(data_path, "duckdb")

  # dir = "~/Git/crawlsp"

  fs::dir_create(data_path)
  fs::dir_create(csv_path)

  con = DBI::dbConnect(duckdb::duckdb(dbdir = fs::path(db_path)))

  # DBI::dbRemoveTable(con, "nodes")
  # sapply(DBI::dbListTables(con), function(x) DBI::dbRemoveTable(con, x))

  nodes = get_nodes(con)
  tasks = get_tasks(con)
  revisions = get_revisions(con)
  rev_hx = get_revision_history(con)

  # default does not include hidden files, which conveniently gets rid of most
  # cruft
  tmp = fs::file_info(fs::dir_ls(dir, recurse = TRUE, type = "file")) %>%
    dplyr::mutate(
      filename = fs::path_rel(.data$path, dir),
      timestamp = as.integer(.data$modification_time),
      .keep = "none"
    ) %>%
    dplyr::copy_to(con, df = ., name = "tmp_files", overwrite = TRUE)

  to_add = tmp %>%
    dplyr::anti_join(revisions, by = c("filename", "timestamp")) %>%
    dplyr::mutate(revision_id = uuid(), status = "ACTIVE") %>%
    dplyr::compute()
  # dplyr::collect()

  to_update = revisions %>%
    dplyr::semi_join(tmp, by = c("filename")) %>%
    dplyr::anti_join(tmp, by = c("filename", "timestamp")) %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    # dplyr::collect() %>%
    dplyr::mutate(status = "SUPERCEDED") %>%
    dplyr::compute()

  to_retire = revisions %>%
    dplyr::anti_join(
      tmp %>% dplyr::select(.data$filename) %>% dplyr::distinct(),
      by = "filename"
    ) %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    # dplyr::collect() %>%
    dplyr::mutate(status = "DELETED") %>%
    dplyr::compute()

  revisions %>% upsert(to_update)
  revisions %>% upsert(to_retire)
  revisions %>% upsert(to_add)

  current = revisions %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    dplyr::compute()

  to_add_hx = to_update %>%
    dplyr::select(original_revision_id = revision_id, filename) %>%
    dplyr::inner_join(
      to_add %>% dplyr::select(new_revision_id = revision_id, filename),
      by = "filename"
    ) %>%
    dplyr::select(-filename)

  rev_hx %>% upsert(to_add_hx)

  tasks %>%
    upsert(
      to_retire %>% dplyr::transmute(revision_id, process = "INACTIVE")
    )

  tasks %>%
    upsert(
      to_add %>% dplyr::transmute(revision_id, process = "PENDING")
    )

  ## to_update is critical here now as it contains the map from filename, timestamp
  # to revision_id uuid.
}


servers = new.env(parent = emptyenv())


get_tasks = function(con) {
  return(get_table(con, "revision_tasks", i_revision_task, c("revision_id")))
}

get_revisions = function(con) {
  return(get_table(con, "revisions", i_file_revision, c("revision_id")))
}

get_revision_history = function(con) {
  return(get_table(
    con,
    "revision_history",
    i_revision_history,
    list("original_revision_id", "new_revision_id")
  ))
}

get_nodes = function(con) {
  return(get_table(con, "nodes", i_nodes, c("revision_id", "start")))
}
