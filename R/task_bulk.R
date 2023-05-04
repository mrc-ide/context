##' Prepare many expressions
##' @title Prepare many expressions
##'
##' @param X Something to iterate over; a vector, list or data.frame
##'   (in the case of a data.frame, iteration will be row-by-row)
##'
##' @param FUN A function to apply to each element (or row) of
##'   \code{X}
##'
##' @param DOTS Additional arguments to apply with each elements of
##'   \code{X}
##'
##' @param do_call Treat each element of \code{X} as a \code{do.call} call
##'
##' @param use_names When preparing a data.frame, retain column names
##'   as argument names when using \code{do_call}.  If \code{FALSE}
##'   then positional matching will be used.
##'
##' @inheritParams prepare_expression
##'
##' @export
bulk_prepare_expression <- function(X, FUN, DOTS, do_call, use_names,
                                    envir, db) {
  XX <- bulk_prepare_expression_X(X, do_call, use_names)
  do_bulk_prepare_expression(XX, FUN, DOTS, do_call, envir, db)
}

##' Save bulk tasks
##' @title Save bulk tasks
##' @param context A context
##' @param depends_on Optional vector of task ids that this task
##'   depends on
##' @inheritParams bulk_prepare_expression
##' @export
bulk_task_save <- function(X, FUN, context, DOTS = NULL,
                           do_call = FALSE, use_names = TRUE,
                           envir = parent.frame(), depends_on = NULL) {
  db <- context$db
  context_id <- context$id

  verify_dependencies_exist(depends_on, context)

  dat <- bulk_prepare_expression(X, FUN, DOTS, do_call, use_names, envir, db)

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
  if (!is.null(depends_on)) {
    db$mset(ids, rep(depends_on, length(ids)), "task_deps")
  }
  setNames(ids, names(dat))
}

bulk_prepare_expression_X <- function(X, do_call, use_names) {
  if (is.data.frame(X)) {
    if (ncol(X) == 0L) {
      stop("'X' must have at least one column")
    }
    if (nrow(X) == 0L) {
      stop("'X' must have at least one row")
    }
    if (any(vlapply(X, is.factor))) {
      stop("Factors cannot be used in bulk expressions")
    }
    X <- df_to_list(X, use_names || !do_call)
  } else if (is.atomic(X) && !is.null(X)) {
    if (is.factor(X)) {
      stop("Factors cannot be used in bulk expressions")
    }
    X <- setNames(as.list(unname(X)), names(X))
  } else if (is.list(X)) {
    if (any(vlapply(X, function(x) any(vlapply(x, is.factor))))) {
      stop("Factors cannot be used in bulk expressions")
    }
    if (do_call) {
      lens <- lengths(X)
      ## Here, support recycling out scalars
      ul <- unique(lens)
      if (length(ul) == 2L && min(ul) == 1L) {
        n <- max(lens)
        X[lens == 1L] <- lapply(X[lens == 1L], rep_len, n)
        ul <- lens <- n
      }
      if (length(ul) != 1L) {
        stop("Every element of 'X' must have the same length")
      }
      if (ul == 0L) {
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

do_bulk_prepare_expression <- function(X, FUN, DOTS, do_call, envir, db) {
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

  setNames(lapply(X, rewrite_expr), names(X))
}

bulk_callable <- function(FUN) {
  is.symbol(FUN) || (is.call(FUN) && identical(FUN[[1L]], quote(`::`)))
}
