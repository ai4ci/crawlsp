# for testing purpose

LanguageClient <- R6::R6Class(
  "LanguageClient",
  # inherit = LanguageBase,
  private = list(
    ticket = 0,
    read_char_buf = raw(0),

    finalize = function() {
      if (self$process$is_alive()) {
        logger$info("Shutting down server (finalise).")
        if (!is.null(self$process) && self$process$is_alive()) {
          if (identical(Sys.getenv("R_COVR"), "true")) {
            self$process$wait()
          } else {
            self$process$kill_tree()
          }
        }
      }
      self$request_callbacks$clear()
    }
  ),
  public = list(
    process = NULL,
    rootUri = NULL,
    workspaceFolders = NULL,
    ClientCapabilities = NULL,
    ServerCapabilities = NULL,
    diagnostics = NULL,

    catch_callback_error = TRUE,
    request_handlers = NULL,
    notification_handlers = NULL,
    request_callbacks = NULL,

    initialize = function(cmd = NULL, args = NULL) {
      if (!is.null(cmd)) {
        self$process <- processx::process$new(
          cmd,
          args,
          stdin = "|",
          stdout = "|",
          stderr = "|"
        )
      }
      lapply(
        self$process$read_error_lines(),
        logger$error
      )
      self$diagnostics <- collections::dict()
      # super$initialize()
      self$register_handlers()
      self$request_callbacks <- collections::dict()
    },

    get_ticket = function() {
      private$ticket <- private$ticket + 1
      private$ticket
    },

    request = function(method, params) {
      Request$new(
        self$get_ticket(),
        method,
        params
      )
    },

    deliver = function(message, callback = NULL) {
      if (is.null(message)) {
        return(NULL)
      }
      logger$info("deliver: ", class(message))
      method <- message$method
      if (!is.null(method)) {
        logger$info("method: ", method)
      }
      self$write_text(message$format())

      # Log errors
      lapply(
        self$process$read_error_lines(),
        logger$error
      )

      if (inherits(message, "Request") && !is.null(callback)) {
        id <- message$id
        self$request_callbacks$set(as.character(id), callback)
      }
    },

    read_header = function() {
      header <- self$read_line()
      if (length(header) == 0 || !nzchar(header)) {
        return(NULL)
      }
      bytes <- NULL

      while (TRUE) {
        if (length(header) == 0) {
          Sys.sleep(0.01)
        } else if (!nzchar(header)) {
          break
        } else {
          logger$info("received: ", header)

          if (!startsWith(header, "Content")) {
            stop("Unexpected non-empty line")
          }
          matches <- stringi::stri_match_first_regex(
            header,
            "Content-Length: ([0-9]+)"
          )
          if (!is.na(matches[2])) {
            bytes <- as.integer(matches[2])
          }
        }
        header <- self$read_line()
      }
      bytes
    },

    read_content = function(nbytes) {
      data <- ""
      while (nbytes > 0) {
        newdata <- self$read_char(nbytes)
        if (length(newdata) > 0) {
          nbytes <- nbytes - nchar(newdata, type = "bytes")
          data <- paste0(data, newdata)
        }
        Sys.sleep(0.01)
      }
      data
    },

    fetch = function(blocking = FALSE, timeout = Inf) {
      self$check_connection()
      start_time <- Sys.time()
      nbytes <- self$read_header()
      if (is.null(nbytes)) {
        if (!blocking) {
          return(NULL)
        } else {
          while (is.null(nbytes)) {
            if (Sys.time() - start_time > timeout) {
              return(NULL)
            }
            Sys.sleep(0.1)
            nbytes <- self$read_header()
          }
        }
      }

      data <- self$read_content(nbytes)
      data
    },

    handle_raw = function(data) {
      payload <- tryCatchStack(
        jsonlite::fromJSON(data, simplifyVector = FALSE),
        error = function(e) e
      )
      if (inherits(payload, "error")) {
        logger$error("error handling json: ", payload)
        return(NULL)
      }
      pl_names <- names(payload)
      logger$info("received payload.")
      if ("id" %in% pl_names && "method" %in% pl_names) {
        self$handle_request(payload)
      } else if ("method" %in% pl_names) {
        self$handle_notification(payload)
      } else if ("id" %in% pl_names) {
        self$handle_response(payload)
      } else {
        logger$info("unknown message")
      }
    },

    handle_request = function(request) {
      id <- request$id
      method <- request$method
      params <- request$params
      if (method %in% names(self$request_handlers)) {
        logger$info("handling request: ", method)
        tryCatchStack(
          {
            dispatch <- self$request_handlers[[method]]
            dispatch(self, id, params)
          },
          error = function(e) {
            logger$info("internal error:", e)
            self$deliver(ResponseErrorMessage$new(
              id,
              "InternalError",
              to_string(e)
            ))
          }
        )
      } else {
        logger$info("unknown request: ", method)
        self$deliver(ResponseErrorMessage$new(
          id,
          "MethodNotFound",
          paste0("unknown request ", method)
        ))
      }
    },

    handle_notification = function(notification) {
      method <- notification$method
      params <- notification$params
      if (method %in% names(self$notification_handlers)) {
        logger$info("handling notification: ", method)
        tryCatchStack(
          {
            dispatch <- self$notification_handlers[[method]]
            dispatch(self, params)
          },
          error = function(e) {
            logger$info("internal error:", e)
          }
        )
      } else {
        logger$info("unknown notification: ", method)
      }
    },

    handle_response = function(response) {
      id <- response$id
      callback <- tryCatch(
        self$request_callbacks$pop(as.character(id)),
        error = function(e) NULL
      )
      if (is.null(response$error)) {
        if (!is.null(callback)) {
          logger$info("calling callback")
          if (self$catch_callback_error) {
            tryCatchStack(
              callback(self, result = response$result),
              error = function(e) logger$info("callback error: ", e)
            )
          } else {
            callback(self, result = response$result)
          }
        }
      } else {
        logger$info("error:", response$error)
        # only call callback if it handles error
        if (!is.null(callback) && "error" %in% names(formals(callback))) {
          logger$info("calling callback")
          if (self$catch_callback_error) {
            tryCatchStack(
              callback(self, result = response$result, error = response$error),
              error = function(e) logger$info("callback error: ", e)
            )
          } else {
            callback(self, result = response$result, error = response$error)
          }
        }
      }
    },

    check_connection = function() {
      if (!is.null(self$process) && !self$process$is_alive()) {
        stop("Server is dead.")
      }
    },

    write_text = function(text) {
      self$check_connection()
      self$process$write_input(text)
    },

    read_output_lines = function() {
      self$check_connection()
      if (self$process$is_alive() && self$process$poll_io(1)[1] != "ready") {
        return(NULL)
      }
      self$process$read_output_lines(1)
    },

    read_line = function() {
      self$check_connection()
      buf <- private$read_char_buf
      if (length(buf) > 0 && as.raw(10) %in% buf) {
        first_match <- min(which(buf == charToRaw("\n")))
        line <- buf[seq_len(first_match - 1)]
        if (length(line) > 0 && line[length(line)] == charToRaw("\r")) {
          line <- line[-length(line)]
        }
        private$read_char_buf <- buf[seq_safe(first_match + 1, length(buf))]
        return(rawToChar(line))
      }
      line <- self$read_output_lines()
      if (length(line) > 0) {
        line <- paste0(rawToChar(buf), line)
        private$read_char_buf <- raw(0)
        trimws(line, "right")
      }
    },

    read_output = function(n) {
      self$process$read_output(n)
    },

    read_char = function(n) {
      self$check_connection()
      if (length(private$read_char_buf) < n) {
        data <- c(
          private$read_char_buf,
          charToRaw(self$read_output(n - length(private$read_char_buf)))
        )
      } else {
        data <- private$read_char_buf
      }
      if (length(data) > n) {
        private$read_char_buf <- data[seq_safe(n + 1, length(data))]
        rawToChar(data[seq_len(n)])
      } else {
        private$read_char_buf <- raw(0)
        rawToChar(data)
      }
    },

    read_error = function() {
      self$check_connection()
      paste0(self$process$read_error_lines(), collapse = "\n")
    },

    welcome = function() {
      self$deliver(
        self$request(
          "initialize",
          list(
            rootUri = self$rootUri,
            workspaceFolders = self$workspaceFolders,
            capabilities = self$ClientCapabilities
          )
        ),
        callback = function(self, result) {
          self$ServerCapabilities <- result$capabilities
        }
      )
    },

    start = function(
      working_dir = getwd(),
      workspace_folders = NULL,
      capabilities = NULL
    ) {
      self$rootUri <- if (is.null(working_dir)) {
        NULL
      } else {
        path_to_uri(working_dir)
      }
      if (!is.null(workspace_folders)) {
        self$workspaceFolders <- lapply(workspace_folders, function(dir) {
          list(uri = path_to_uri(dir), name = basename(dir))
        })
      } else {
        self$workspaceFolders <- NULL
      }
      self$ClientCapabilities <- capabilities
      self$welcome()
    },

    shutdown = function() {
      logger$info("Shutting down server.")
      self$request("shutdown", NULL)
      if (self$process$is_alive()) {
        self$process$kill_tree()
      }
    },

    run = function() {
      # placeholder
    }
  )
)


LanguageClient$set("public", "register_handlers", function() {
  self$request_handlers <- list()
  self$notification_handlers <- list()
})


language_client <- function(
  cmd,
  args,
  working_dir = getwd(),
  workspace_folders = NULL,
  diagnostics = FALSE,
  capabilities = NULL
) {
  client <- LanguageClient$new(cmd, args)

  client$notification_handlers <- list(
    `textDocument/publishDiagnostics` = function(self, params) {
      uri <- params$uri
      diagnostics <- params$diagnostics
      self$diagnostics$set(uri, diagnostics)
    }
  )

  client$start(
    working_dir = working_dir,
    workspace_folders = workspace_folders,
    capabilities = capabilities
  )
  client$catch_callback_error <- FALSE
  # initialize request
  data <- client$fetch(blocking = TRUE)
  client$handle_raw(data)
  client %>% notify("initialized")
  client %>%
    notify(
      "workspace/didChangeConfiguration",
      list(settings = list(diagnostics = diagnostics))
    )
  client
}
