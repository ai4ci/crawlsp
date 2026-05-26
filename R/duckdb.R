#' Get a table by name or create it using interface spec
#'
#' @param con a duckdb connection
#' @param name the table name
#' @param iface the format of the table as a iface specifcation
#' @param keys a column name vector that define primary keys ( or a list of
#' vectors for multiple indices)
#'
#' @returns a duckdb remote table (possibly empty)
#' @export
#' @concept duckdb
get_table = function(con, name, iface, keys = NULL) {
  tables = DBI::dbListTables(con)
  if (!name %in% tables) {
    # Setup empty table if does not exist
    # unique indexes.
    # TODO: create from interfacer spec.

    idx = if (is.null(keys)) {
      list()
    } else if (is.list(keys)) {
      keys
    } else {
      list(keys)
    }

    # table structure
    dplyr::copy_to(
      con,
      name = name,
      df = interfacer::iproto(iface),
      unique_indexes = idx
    )
  }
  return(
    structure(
      dplyr::tbl(con, name),
      iface = iface
    )
  )
}

#' Upsert or insert ignore data into duckdb table
#'
#' @param db_tbl the duckdb table, which must have a primary key defined.
#' @param data a dataframe to insert. May be remote.
#' @param ignore Should we update non key entries for items that match by
#' unique key or ignore them. `ignore=FALSE` will ensure all items in `data`
#' are present in the final table. `ignore=TRUE` will result in new items from
#' `data` being inserted only.
#'
#' @returns the number of rows affected by the query
#' @export
#' @concept duckdb
#'
#' @unit
#' con = DBI::dbConnect(duckdb::duckdb(), path = ":memory:")
#' db = dplyr::copy_to(con, dplyr::tibble(
#'    a = 1:5,
#'    b = letters[2:6],
#'    c = 11:15
#' ), name="testq", unique_indexes = list(idx1 = c("a","b")))
#'
#' testthat::expect_equal(
#'   db %>% dplyr::count() %>% dplyr::pull(n),
#'   5
#' )
#'
#' db2 = dplyr::copy_to(con, dplyr::tibble(
#'    a = 3:5,
#'    b = letters[4:6],
#'    c = 1:3
#' ), name="testq2", unique_indexes = list(idx1 = c("a","b")))
#'
#' # upsert - update existing using remote:
#' rows = db %>% upsert(db2)
#' testthat::expect_equal(rows, 3)
#'
#' # The contents of c have been updated
#' testthat::expect_equal(db %>% dplyr::pull(c) %>% all(. %in% c(1, 2, 3, 11, 12)), TRUE)
#'
#' # upsert ignore - local table:
#' db3 = dplyr::tibble(
#'    a = 5:7,
#'    b = letters[6:8],
#'    c = 21:23
#' )
#'
#' # Only 2 rows will be affected asn the first one matches exising data
#' rows = db %>% upsert(db3, ignore = TRUE)
#' testthat::expect_equal(rows, 2)
#'
#' # The contents of c have been updated
#' testthat::expect_equal(db %>% dplyr::pull(c) %>% all(. %in% c(1, 2, 3, 11, 12,22,23)), TRUE)
#'
upsert = function(db_tbl, data, ignore = FALSE) {
  remote = inherits(data, "tbl_duckdb_connection")
  ## Check data conformant with target (if defined)
  iface = attr(db_tbl, "iface")

  nrow_data = data %>% dplyr::count() %>% dplyr::pull(n)
  if (nrow_data == 0) {
    return(invisible(0))
  }

  # generate upsert SQL
  con = dbplyr::remote_con(db_tbl)
  table = dbplyr::remote_name(db_tbl)
  if (remote) {
    # Generate an insert into select statement for the remote table.
    # accounts for the fact that `data` might be a view or lazy query.
    x = sprintf(
      "INSERT INTO %s BY NAME %s",
      DBI::dbQuoteIdentifier(con, table),
      dbplyr::sql_render(data)
    )
  } else {
    if (!is.null(iface)) {
      data = interfacer::iconvert(data %>% dplyr::collect(), iface)
    }
    x = DBI::sqlAppendTable(con, table, data, row.names = FALSE)
  }
  if (ignore) {
    x = sub("INSERT", "INSERT OR IGNORE", x)
  } else {
    x = sub("INSERT", "INSERT OR REPLACE", x)
  }
  # apply:
  tmp = DBI::dbExecute(con, x)
  return(invisible(tmp))
}


#' Delete matching items
#'
#' Items are matched to the supplied `data` argument which is processed row-wise
#' all fields procvided in each row must match.
#'
#' @param db_tbl a remote duckdb table
#' @param data a named list, a dataframe, or a remote table.
#'
#' @returns the number of rows deleted if successful, error otherwise
#' @concept duckdb
#' @export
#'
#' @unit
#' con = DBI::dbConnect(duckdb::duckdb(), path = ":memory:")
#' db = dplyr::copy_to(con, dplyr::tibble(
#'    a = 1:5,
#'    b = letters[2:6],
#'    status = factor(c("IN_PROGRESS","PENDING","COMPLETE","PENDING","PENDING"))
#' ), name="testq")
#'
#' testthat::expect_equal(
#'   db %>% dplyr::filter(status == "PENDING") %>% dplyr::count() %>% dplyr::pull(n),
#'   3
#' )
#'
#' # delete pending entries
#' rows = db %>% delete(list(status="PENDING"))
#'
#' # 3 rows deleted
#' testthat::expect_equal(rows, 3)
#'
#' # no pending rows left
#' testthat::expect_equal(
#'   db %>% dplyr::filter(status == "PENDING") %>% dplyr::count() %>% dplyr::pull(n),
#'   0
#' )
delete = function(db_tbl, data) {
  # data can have a subset of columns of db_tbl
  if (length(setdiff(colnames(data), colnames(db_tbl))) > 0) {
    stop("Unmatched columns in query table.")
  }

  # deleting is much less complex if we collect the results and resubmit
  remote = inherits(data, "tbl_duckdb_connection")
  if (remote) {
    data = data %>% dplyr::collect()
  }
  data = dplyr::as_tibble(data)

  # Empty is OK:
  if (nrow(data) == 0) {
    return(invisible(0))
  }

  con = dbplyr::remote_con(db_tbl)
  table = dbplyr::remote_name(db_tbl)
  rows = 0
  # delete each row in data individually:
  for (i in seq_len(nrow(data))) {
    # Assume if delete is a remote table it is the result of a summarise result
    # or a set of single rows. Alternative would be a DELETE FROM X USING Y WHERE
    # x.col1 = y.col1.
    row = dplyr::collect(data)[i, ]
    sql = sprintf(
      "DELETE FROM %s WHERE %s",
      DBI::dbQuoteIdentifier(con, table),
      .stanza(con, row)
    )
    rows = rows + DBI::dbExecute(con, sql)
  }
  return(invisible(rows))
}


.stanza = function(con, df, sep = " AND ") {
  sql = paste0(
    sapply(names(df), function(nm) {
      sprintf(
        "%s=%s",
        DBI::dbQuoteIdentifier(con, nm),
        DBI::dbQuoteLiteral(con, df[[nm]])
      )
    }),
    collapse = sep
  )
}


#' Compare and set duckdb table with status column
#'
#' Selects an item from the input table with a certain status and
#' updates it.
#'
#' @param db_tbl a remote duckdb lazy table
#' @param initial the initial status you want to change as a named list
#' @param final the to-be status
#'
#' @returns the row as it was before selection, the database table is updated with
#' the new status
#' @concept duckdb
#' @export
#'
#' @unit
#' con = DBI::dbConnect(duckdb::duckdb(), path = ":memory:")
#' db = dplyr::copy_to(con, dplyr::tibble(
#'    a = 1:5,
#'    b = letters[2:6],
#'    status = factor(c("IN_PROGRESS","PENDING","COMPLETE","PENDING","PENDING"))
#' ), name="testq")
#'
#' testthat::expect_equal(
#'   db %>% dplyr::filter(status == "PENDING") %>% dplyr::count() %>% dplyr::pull(n),
#'   3
#' )
#'
#' item = db %>% compare_and_set(list(status="PENDING"), list(status="IN_PROGRESS"))
#'
#' testthat::expect_equal(
#'   db %>% dplyr::filter(status == "PENDING") %>% dplyr::count() %>% dplyr::pull(n),
#'   2
#' )
#'
compare_and_set = function(db_tbl, initial, final) {
  con = dbplyr::remote_con(db_tbl)
  table = dbplyr::remote_name(db_tbl)
  if (
    !(.nz_named_list(initial) &&
      .nz_named_list(final))
  ) {
    stop("initial and final must be non empty lists.")
  }
  complete = FALSE
  while (!complete) {
    sql = sprintf(
      "SELECT * FROM %s WHERE %s LIMIT 1",
      DBI::dbQuoteIdentifier(con, table),
      .stanza(con, initial, " AND ")
    )
    row = DBI::dbGetQuery(con, sql)
    if (nrow(row) == 0) {
      stop("No matches for ", initial)
    }
    new_row = merge_list(row, final)
    sql2 = sprintf(
      "UPDATE %s SET %s WHERE %s",
      DBI::dbQuoteIdentifier(con, table),
      .stanza(con, final, ", "),
      .stanza(con, as.list(row), " AND ")
    )
    res = DBI::dbExecute(con, sql2)
    complete = res >= 1
  }
  return(row)
}

.nz_named_list = function(lst) {
  return(is.list(lst) && !is.null(names(lst)) && length(list) > 0)
}

# tmp = tasks %>% compare_and_set(list(process="PENDING"), list(process="IN_PROGRESS"))
