task_bulk_prepare <- function(X, FUN, DOTS, do_call, use_names, envir, db) {
  XX <- task_bulk_prepare_X(X, do_call, use_names)
  task_bulk_prepare_expr(XX, FUN, DOTS, do_call, envir, db)
}

## This is the new replacement bulk uploader.  It exists to support
## queuer only really, though will need to be exported (and that will
## be a bit weird).
task_bulk_save <- function(X, FUN, context, DOTS = NULL,
                           do_call = FALSE, use_names = TRUE,
                           envir = parent.frame()) {
  db <- context$db
  context_id <- context$id

  dat <- task_bulk_prepare(X, FUN, DOTS, do_call, use_names, envir, db)

  build_task <- function(x) {
    x$id <- ids::random_id()
    x$context_id <- context_id
    class(x) <- "task"
    x
  }

  n <- length(dat)
  context_log("bulk", sprintf("Creating %s tasks", n))
  tasks <- lapply(dat, build_task)
  ids <- vcapply(tasks, "[[", "id")
  ns <- c("tasks", "task_status", "task_context", "task_time_sub")
  send <- c(tasks,
            rep(list(TASK_PENDING), n),
            rep(list(context$id), n),
            rep(list(Sys.time()), n))
  db$mset(rep(ids, length(ns)), send, rep(ns, each = n))
  setNames(ids, names(X))
}

task_bulk_prepare_X <- function(X, do_call, use_names) {
  if (is.data.frame(X)) {
    if (ncol(X) == 0L) {
      stop("'X' must have at least one column")
    }
    if (nrow(X) == 0L) {
      stop("'X' must have at least one row")
    }
    X <- df_to_list(X, use_names || !do_call)
  } else if (is.atomic(X) && !is.null(X)) {
    X <- as.list(unname(X))
  } else if (is.list(X)) {
    if (do_call) {
      lens <- lengths(X)
      if (length(unique(lens)) != 1L) {
        stop("Every element of 'X' must have the same length")
      }
      if (lens[[1L]] == 0L) {
        stop("Elements of 'X' must have at least one element")
      }
      nms <- names(X[[1L]])
      if (!all(vlapply(X[-1], function(x) identical(names(x), nms)))) {
        stop("Elements of 'X' must have the same names")
      }
      ## This would be useful in the case of moving the ifelse from
      ## the rewrite function below.
      ##
      ## if (lens[[1]] == 0L && !do_call) {
      ##   X <- lapply(X, list)
      ## }
    }
  } else {
    stop("X must be a data.frame or list")
  }

  if (length(X) == 0L) {
    stop("'X' must have at least one element")
  }

  X
}

task_bulk_prepare_expr <- function(X, FUN, DOTS, do_call, envir, db) {
  if (bulk_callable(FUN)) {
    function_name <- FUN
    function_value <- NULL
  } else if (is.function(FUN)) {
    function_value <- FUN
    function_name <- NULL
  } else {
    stop("Expected 'FUN' to be a symbol, fully qualified name or function")
  }
  if (do_call) {
    ## These assumptions about the first element are tested above
    len <- length(X[[1L]])
    nms <- names(X[[1L]])
    args <- setNames(rep(list(NULL), len), nms)
    template <- as.call(c(list(function_name), args, DOTS))
    idx <- seq_len(len) + 1L
  } else {
    template <- as.call(c(list(function_name), list(NULL), DOTS))
    idx <- 2L
  }
  template <- prepare_expression(template, envir, db, function_value)

  ## TODO: I'm not 100% sure that this is a great idea; by running the
  ## object itself into the call we hit trouble if the objects that
  ## are being iterated over are nontrivial in size.  Consider
  ## detecting this and dumping them into the locals if they're big.
  rewrite_expr <- function(x) {
    if (do_call) {
      template$expr[idx] <- x
      if (!is.null(names(x))) {
        names(template$expr[idx]) <- names(x)
      }
    } else {
      template$expr[[idx]] <- x
    }
    template
  }

  lapply(X, rewrite_expr)
}

bulk_callable <- function(FUN) {
  is.symbol(FUN) || (is.call(FUN) && identical(FUN[[1L]], quote(`::`)))
}
