.data = rlang::.data

options("crawlsp.debug" = TRUE)


devtools::load_all()

dir = system.file("projects/mypackage", package = "crawlsp")
dir = system.file("projects/hello-java", package = "crawlsp")
dir = system.file("projects/hello-markdown", package = "crawlsp")

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

# Check server exists
# used = Filter(
#   function(x) {
#     cmd = x$command[1]
#     exists = Sys.which(cmd) != ""
#     if (!exists) {
#       warning(
#         "Cannot find required language server: ",
#         cmd,
#         "; install it from: " + x$url
#       )
#     }
#     return(exists)
#   },
#   used
# )

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

language = names(used)[[1]]
if (language == "markdown") {
  fs::file_touch(fs::path(dir, ".marksman.toml"))
}
test = used[[1]]
cmd = as.character(unlist(test$command[1]))
args = as.character(unlist(tail(test$command, -1)))

# cmd = as.character(unlist(config$servers$r$command[1]))
# args = as.character(unlist(tail(config$servers$r$command, -1)))

client = language_client(cmd = cmd, args = args, working_dir = dir)

.tree(client$ServerCapabilities)

defn_file = test$files[[5]]

client %>% did_open(defn_file)

for (f in test$files) {
  client %>% did_open(f)
}

kinds = names(SymbolKind)[unlist(SymbolKind)]

symbols = c(
  respond_workspace_symbol(client, "*"),
  respond_workspace_symbol(client, "")
)
.tree(symbols)

sym_refs = .array_to_df(symbols, .symbol_information_to_df, dir = dir) %>%
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
  tidyr::unnest(refs, keep_empty = TRUE, names_sep = ".")

#tmp = respond_document_symbol(client, defn_file)
#.tree(tmp)

if (client$ServerCapabilities$documentSymbolProvider) {
  tmp = respond_document_symbol(client, defn_file)

  # Gives a set of edges to references in code but only for classes in java
  refs_df = dplyr::bind_rows(lapply(tmp, .node_to_df, dir = dir)) %>%
    dplyr::mutate(kind = kinds[kind]) %>%
    tidyr::unnest(location) %>%
    dplyr::mutate(
      refs = purrr::map2(
        start_line,
        start_character,
        # ~ lapply(
        ~ respond_references(client, defn_file, c(.x, .y)),
        # .location_list_to_df, dir = dir
        #))
      )
    ) %>%
    dplyr::mutate(
      refs = purrr::map(
        refs,
        ~ dplyr::bind_rows(lapply(.x, .location_to_df, dir = dir))
      )
    ) %>%
    tidyr::unnest(refs, keep_empty = TRUE, names_sep = ".")

  # Does not produce any content in java
  # help_df = refs_df %>%
  #   dplyr::mutate(
  #     help = purrr::map2(
  #       start_line,
  #       start_character,
  #       ~ respond_signature(client, defn_file, c(.x, .y)),
  #     )
  #   )
}

if (!is.null(client$ServerCapabilities$semanticTokensProvider$full)) {
  tok = respond_semantic_tokens_full(client, defn_file)
  df = dplyr::as_tibble(matrix(unlist(tok$data), ncol = 5, byrow = TRUE))
  colnames(df) = c(
    "start_line",
    "start_character",
    "length",
    "tokenTypeId",
    "tokenModifiersId"
  )
  types = unlist(
    client$ServerCapabilities$semanticTokensProvider$legend$tokenTypes
  )
  mods = unlist(
    client$ServerCapabilities$semanticTokensProvider$legend$tokenModifiers
  )

  df2 = df %>%
    dplyr::mutate(
      tokenType = types[tokenTypeId + 1],
      tokenModifier = lapply(tokenModifiersId, function(id) {
        mods[as.logical(intToBits(id))[seq_along(mods)]]
      }),
      start_line = cumsum(start_line)
    ) %>%
    dplyr::group_by(start_line) %>%
    dplyr::mutate(start_character = cumsum(start_character))

  if (!is.null(client$ServerCapabilities$typeDefinitionProvider)) {
    # Gives links to type definitions (class and interface) in java.
    # Also lots of errors when classes reference something outside the project
    df3 = df2 %>%
      dplyr::filter(
        tokenType %in% c("type", "class", "enum", "interface")
      ) %>%
      dplyr::mutate(
        origin = purrr::map2(
          start_line,
          start_character,
          ~ respond_goto_type_definition(client, defn_file, c(.x, .y))
        )
      )
    .tree(df3$origin)
  }

  if (!is.null(client$ServerCapabilities$callHierarchyProvider)) {
    # This does not work as expected in java
    df4 = df2 %>%
      dplyr::filter(
        tokenType %in% c("function", "method", "variable", "enumMember")
      ) %>%
      dplyr::mutate(
        call = purrr::map2(
          start_line,
          start_character,
          ~ respond_prepare_call_hierarchy(client, defn_file, c(.x, .y))
        )
      ) %>%
      dplyr::mutate(
        upstream = purrr::map(
          call,
          ~ respond_call_hierarchy_incoming_calls(client, .x[[1]])
        ),
        downstream = purrr::map(
          call,
          ~ respond_call_hierarchy_outgoing_calls(client, .x[[1]])
        ),
      )
    .tree(df4$call)
    .tree(df4$upstream) # always empty
    .tree(df4$downstream) # always empty
  }

  # respond references only seems to work for the definition and only
  # for classes in java.
  # df5 = df2 %>%
  #   dplyr::filter(
  #     tokenType %in% c("function", "method", "property", "variable")
  #   ) %>%
  #   dplyr::mutate(
  #     call = purrr::map2(
  #       start_line,
  #       start_character,
  #       ~ respond_references(client, defn_file, c(.x, .y))
  #     )
  #   )
}

# Not supported in java lsp
if (!is.null(client$ServerCapabilities$typeHierarchyProvider)) {
  df6 = df2 %>%
    dplyr::filter(tokenType %in% c("type", "class", "enum", "interface")) %>%
    dplyr::mutate(
      type = purrr::map2(
        start_line,
        start_character,
        ~ respond_prepare_type_hierarchy(client, defn_file, c(.x, .y))
      )
    ) %>%
    dplyr::mutate(
      parent = purrr::map(
        type,
        ~ respond_type_hierarchy_supertypes(client, .x[[1]])
      ),
      child = purrr::map(
        type,
        ~ respond_type_hierarchy_subtypes(client, .x[[1]])
      ),
    )
  .tree(df6$type)
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

# Get a recursive file listing matching set of globs
.fs_glob = function(dir, globs) {
  unlist(
    sapply(globs, function(g) {
      fs::dir_ls(dir, recurse = TRUE, type = "file", glob = fs::path(dir, g))
    }),
    use.names = FALSE
  )
}
