## Nodes ----

# tmp = respond_document_symbol(client, test$files[[1]])
# ├─┬─$name: "App"
# │ ├─$kind: 5
# │ ├─$deprecated: FALSE
# │ ├─$location:
# │ │ ├─$uri: "file:///home/vp22681/Git/crawlsp/inst/projects/hello-java/src/main/java/com/example/App.java"
# │ │ └─$range:
# │ │   ├─$start:
# │ │   │ ├─$line: 7
# │ │   │ └─$character: 0
# │ │   └─$end:
# │ │     ├─$line: 17
# │ │     └─$character: 1
# │ └─$containerName: "com.example"

#
file_to_symbol_nodes = function(con, client, dir, revision_id) {
  revisions = get_revisions(con)
  nodes = get_nodes(con)
  edges = get_edges(con)
  # TODO: better to do this as one pass of multiple revisions?
  # So that we can get consistent set of nodes before running edges.
  # OR create a process within the tasks statuses that defines what build phase
  # is OK.

  revision = revisions %>%
    dplyr::filter(revision_id == local(revision_id)) %>%
    dplyr::collect()
  if (nrow(revision) == 0) {
    stop("Did not find revision for id: ", revision_id)
  }
  file = fs::path(dir, revision$filename)
  did_open(client, file)
  tmp = respond_document_symbol(client, file)
  # .tree(tmp)

  extr = dplyr::bind_rows(lapply(tmp, .node_to_df, dir)) %>%
    tidyr::unnest(location) %>%
    dplyr::mutate(
      revision_id = revision_id
    )

  nodes %>% upsert(extr %>% dplyr::select(-"contained_in", -"path"))

  edges = extr %>%
    dplyr::select(
      source_revision_id = "revision_id",
      source_start_line = start_line,
      source_start_col = start_col,
      name = contained_in
    ) %>%
    dplyr::inner_join(
      extr %>%
        dplyr::select(
          target_revision_id = "revision_id",
          target_start_line = start_line,
          target_start_col = start_col,
          name
        ),
      by = "name"
    ) %>%
    dplyr::mutate(relationship = rels$COMPONENT)
}


## Task ----

complete_task = function(con, revision_id) {
  tasks = get_tasks(con)
  tasks %>%
    upsert(
      dplyr::tibble(
        revision_id = revision_id,
        process = "COMPLETE"
      )
    )
}

get_active_task = function(con) {
  tasks = get_tasks(con)
  revisions = get_revisions(con)

  task = compare_and_set(
    tasks,
    list(process = "PENDING"),
    list(process = "IN_PROGRESS")
  )
  # N.B. The compare and set returns a plain dataframe with a single row.

  return(revisions %>% dplyr::filter(revision_id == task$revision_id))
}


## File version management ----

#' Detect version changes
#'
#' detect if any changes have happened on the filesystem compared to the
#' current database versions.
#'
#' @param con duckdb connection
#' @param files list of file paths relative to dir
#' @param dir a directory
#'
#' @returns TRUE if changes FLASE if unchanged
#' @unit
#' con = .clean_con()
#' dir = tempdir()
#' files = fs::path(dir,paste0("file",1:5))
#' fs::file_touch(files)
#'
#' tmp = update_file_versions(con, files, dir)
#'
#' testthat::expect_equal(detect_updates(con, files, dir), FALSE)
#'
#' Sys.sleep(time = 1)
#'
#' files2 = fs::path(dir,paste0("file",3:5))
#' fs::file_touch(files2)
#'
#' testthat::expect_equal(detect_updates(con, files, dir), TRUE)
#'
detect_updates = function(con, files, dir) {
  revisions = get_revisions(con)

  # For the input list of files, get all the info:
  tmp = fs::file_info(files) %>%
    dplyr::mutate(
      filename = fs::path_rel(.data$path, dir),
      timestamp = as.integer(.data$modification_time),
      .keep = "none"
    ) %>%
    dplyr::copy_to(con, df = ., name = "tmp_check", overwrite = TRUE)

  diff = tmp %>%
    dplyr::anti_join(revisions, by = c("filename", "timestamp")) %>%
    dplyr::count() %>%
    dplyr::pull(n)

  return(diff > 0)
}

#' Update version table with new files
#'
#' @param con duckdb connection
#' @param files list of absolute file paths
#' @param dir a directory
#'
#' @returns the current state of the revisions table
#' @unit
#' con = .clean_con()
#' dir = tempdir()
#' files = fs::path(dir,paste0("file",1:5))
#' fs::file_touch(files)
#'
#' tmp = update_file_versions(con, files, dir)
#' testthat::expect_equal(
#'   tmp %>% dplyr::pull(filename) %>% sort(),
#'   c("file1", "file2", "file3", "file4", "file5")
#' )
#'
#' testthat::expect_equal(tmp %>% dplyr::pull(status), structure(
#'   c(1L, 1L, 1L, 1L, 1L),
#'   levels = c("ACTIVE", "SUPERCEDED", "DELETED"),
#'   class = "factor"
#' ))
#'
#' Sys.sleep(time = 1)
#'
#' files2 = fs::path(dir,paste0("file",3:7))
#' fs::file_touch(files2)
#'
#' tmp2 = update_file_versions(con, files2, dir)
#'
#' testthat::expect_equal(tmp2 %>% dplyr::pull(status) %>% unique(), structure(
#'   1L,
#'   levels = c("ACTIVE", "SUPERCEDED", "DELETED"),
#'   class = "factor"
#' ))
#'
#' testthat::expect_equal(
#'   tmp2 %>% dplyr::pull(filename) %>% sort(),
#'   c("file1", "file2", "file3", "file4", "file5", "file6", "file7")
#' )
#'
#' testthat::expect_equal(
#'   tmp2 %>% dplyr::group_by(timestamp) %>% dplyr::count() %>% dplyr::pull(n),
#'   c(2, 5)
#' )
#'
#' testthat::expect_equal(
#'   get_revisions(con) %>% dplyr::filter(status == "SUPERCEDED") %>% dplyr::pull(filename) %>% sort(),
#'   c("file3", "file4", "file5")
#' )
#'
update_file_versions = function(con, files, dir) {
  tasks = get_tasks(con)
  revisions = get_revisions(con)
  rev_hx = get_revision_history(con)

  # For the input list of files, get all the info:
  tmp = fs::file_info(files) %>%
    dplyr::mutate(
      filename = fs::path_rel(.data$path, dir),
      timestamp = as.integer(.data$modification_time),
      .keep = "none"
    ) %>%
    dplyr::copy_to(con, df = ., name = "tmp_files", overwrite = TRUE) %>%
    dplyr::mutate(revision_id = uuid(), status = "ACTIVE")

  # Update if not already in the file versions list
  revisions %>% upsert(tmp, ignore = TRUE)

  # Look for file revisions that are active, but don't match the input
  # on both filename and timestamp. These are old versions, that need to be
  # deactivated

  to_update = revisions %>%
    dplyr::semi_join(tmp, by = c("filename")) %>%
    dplyr::anti_join(tmp, by = c("filename", "timestamp")) %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    # dplyr::collect() %>%
    dplyr::mutate(status = "SUPERCEDED") %>%
    dplyr::compute()

  revisions %>% upsert(to_update)

  # The file versions are up to date now:

  current = revisions %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    dplyr::compute()

  ## Populate revision history table with old_versions of file
  # and links to new versions of file:
  to_add_hx = to_update %>%
    dplyr::select(original_revision_id = revision_id, filename) %>%
    dplyr::inner_join(
      current %>% dplyr::select(new_revision_id = revision_id, filename),
      by = "filename"
    ) %>%
    dplyr::select(-filename)

  rev_hx %>% upsert(to_add_hx)

  # Add tasks in "PENDING" state if not already there
  tasks %>%
    upsert(
      current %>% dplyr::transmute(revision_id, process = "PENDING"),
      ignore = TRUE
    )

  return(current)
}


#' Delete removed files from database
#'
#' @param con the duckdb connection
#' @param dir a directory
#'
#' @returns a count of removed files
#' @export
#'
#' @unit
#' con = .clean_con()
#' dir = tempdir()
#' files = fs::path(dir,paste0("file",1:5))
#' fs::file_touch(files)
#'
#' tmp = update_file_versions(con, files, dir)
#'
#' deleted = delete_missing(con, dir)
#' testthat::expect_equal(deleted, 0)
#'
#' fs::file_delete(files[1:3])
#' deleted2 = delete_missing(con, dir)
#' testthat::expect_equal(deleted2, 3)
#'
delete_missing = function(con, dir) {
  revisions = get_revisions(con)
  tasks = get_tasks(con)

  current = revisions %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    dplyr::compute()

  tmp = dplyr::tibble(
    filename = fs::path_rel(fs::dir_ls(dir, recurse = TRUE), dir)
  ) %>%
    dplyr::copy_to(con, df = ., name = "tmp_not_retired", overwrite = TRUE)

  # find those files not in tmp
  to_retire = revisions %>%
    dplyr::anti_join(
      tmp,
      by = "filename"
    ) %>%
    dplyr::filter(.data$status == "ACTIVE") %>%
    # dplyr::collect() %>%
    dplyr::mutate(status = "DELETED") %>%
    dplyr::compute()

  retired = revisions %>% upsert(to_retire)

  # Deactivate tasks associated with retired files.

  tasks %>%
    upsert(
      to_retire %>% dplyr::transmute(revision_id, process = "INACTIVE")
    )

  return(retired)
}

## Table accessors ----

#' Get the tasks table
#'
#' @param con a duckdb connection
#'
#' @returns a duckdb "tasks" table
#' @unit
#' con = .clean_con()
#' tasks = get_tasks(con)
#' testthat::expect_equal(colnames(tasks), c("revision_id", "process"))
get_tasks = function(con) {
  return(get_table(con, "revision_tasks", i_revision_task, c("revision_id")))
}

#' Get the revisions table
#'
#' @param con a duckdb connection
#'
#' @returns a duckdb "revisions" table
#' @unit
#' con = .clean_con()
#' revisions = get_revisions(con)
#' testthat::expect_equal(
#'   colnames(revisions),
#'   c("revision_id", "filename", "timestamp", "status")
#' )
get_revisions = function(con) {
  return(get_table(con, "revisions", i_file_revision, c("revision_id")))
}

#' Get the revisions history table
#'
#' @param con a duckdb connection
#'
#' @returns a duckdb "revisions hx" table
#' @unit
#' con = .clean_con()
#' revision_hx = get_revision_history(con)
#' testthat::expect_equal(
#'   colnames(revision_hx),
#'   c("original_revision_id", "new_revision_id")
#' )
get_revision_history = function(con) {
  return(get_table(
    con,
    "revision_history",
    i_revision_history,
    c("original_revision_id", "new_revision_id")
  ))
}

#' Get the nodes table
#'
#' @param con a duckdb connection
#'
#' @returns a duckdb "nodes" table
#' @unit
#' con = .clean_con()
#' nodes = get_nodes(con)
#' testthat::expect_equal(colnames(nodes), c(
#'   "revision_id",
#'   "start_line",
#'   "start_character",
#'   "end_line",
#'   "end_character",
#'   "name",
#'   "kind"
#' ))
get_nodes = function(con) {
  return(get_table(
    con,
    "nodes",
    i_nodes,
    c("revision_id", "start_line", "start_character")
  ))
}

#' Get the nodes table
#'
#' @param con a duckdb connection
#'
#' @returns a duckdb "nodes" table
#' @unit
#' con = .clean_con()
#' edges = get_edges(con)
#' testthat::expect_equal(colnames(edges), c(
#'   "source_revision_id",
#'   "source_start_line",
#'   "source_start_character",
#'   "target_revision_id",
#'   "target_start_line",
#'   "target_start_character",
#'   "relationship"
#' ))
get_edges = function(con) {
  return(get_table(
    con,
    "edges",
    i_edges,
    c(
      "source_revision_id",
      "source_start_line",
      "source_start_character",
      "target_revision_id",
      "target_start_line",
      "target_start_character",
      "relationship"
    )
  ))
}

.clean_con = function() {
  con = DBI::dbConnect(duckdb::duckdb())
  lapply(DBI::dbListTables(con), function(t) DBI::dbRemoveTable(con, t))
  return(con)
}

## Data structure ----

i_task = interfacer::iface(
  filename = character ~ "The relative path to a file from the project root",
  timestamp = integer ~ "The last modified timestamp of the file",
  status = interfacer::enum(
    PENDING,
    NODES,
    EDGES,
    COMPLETE,
    CANCELLED
  ) ~ "The task status",
  details = character ~ "The task details"
)


i_file_revision = interfacer::iface(
  revision_id = character + unique_id ~ "A uuid of this file revision",
  filename = character ~ "The relative path to a file from the project root",
  timestamp = integer ~ "The last modified timestamp of the file",
  status = interfacer::enum(
    ACTIVE,
    SUPERCEDED,
    DELETED
  ) ~ "The status of the revision"
)

i_revision_history = interfacer::iface(
  original_revision_id = character ~ "A uuid of the old file revision",
  new_revision_id = character ~ "A uuid of the new file revision",
)

i_revision_task = interfacer::iface(
  revision_id = character + unique_id ~ "A uuid of this file revision",
  process = interfacer::enum(
    PENDING,
    IN_PROGRESS,
    COMPLETE,
    INACTIVE
  ) ~ "The processing status of the file"
)

i_nodes = interfacer::iface(
  revision_id = character ~ "A uuid of this file revision",
  start_line = integer ~ "The start line number in the file of the symbol (zero based)",
  start_character = integer ~ "The start position on the line of the symbol (zero_based)",
  end_line = integer ~ "The start line number in the file of the symbol (zero based)",
  end_character = integer ~ "The start position on the line of the symbol (zero_based)",
  name = character ~ "The name of the symbol",
  kind = integer ~ "The type of the resource (See: SymbolKind list for values)"
)

i_edges = interfacer::iface(
  source_revision_id = character ~ "The source file revision uuid",
  source_start_line = integer ~ "The source node start line number in the file of the symbol (zero based)",
  source_start_character = integer ~ "The source node start position on the line of the symbol (zero_based)",
  target_revision_id = character ~ "The target file revision uuid",
  target_start_line = integer ~ "The target node start line number in the file of the symbol (zero based)",
  target_start_character = integer ~ "The target node start position on the line of the symbol (zero_based)",
  relationship = character ~ "The relationship type (probably as a URI)"
)

## Relationships ----

rels = list(
  # File history relationships

  # Code relationships
  "COMPONENT" = "EM:PART_OF",
  "INHERITS" = "EM:IS_A",
  # Code / Design link
  "IMPLEMENTS" = "EM:IMPL",
  # Design relationships
  "FEATURE" = "EM:FEATURE",
  "IMPLEMENTED" = "EM:IMPL_BY",
  "PROTOTYPE" = "EM:PROTO_BY",
  "INTERFACE" = "EM:API",
  "TESTDATA" = "EM:PROVIDES_DATA",
  "DEPENDS" = "EM:DEPENDS_ON"
)
