##' List tasks and test if they exist
##' @title List tasks
##' @inheritParams task_status
##' @export
task_exists <- function(ids, db) {
  db <- context_db_get(db)
  db$exists(ids, "tasks")
}

##' @rdname task_exists
##' @export
task_list <- function(db) {
  context_db_get(db)$list("tasks")
}

##' Task status
##' @title Task status
##' @param ids Vector of task ids
##' @param db Something that can be converted to a context db object
##'   (a database, root or context).
##' @param named Name the output with the task ids?
##' @export
task_status <- function(ids, db, named = FALSE) {
  if (length(ids) == 0L) {
    return(if (named) setNames(character(0), character(0)) else character(0))
  }
  db <- context_db_get(db)
  st <- vcapply(db$mget(ids, "task_status", missing = TASK_MISSING),
                identity, USE.NAMES = FALSE)
  if (named) setNames(st, ids) else st
}

##' Fetch result from completed task.
##' @title Fetch task result
##'
##' @param id Single task identifier
##'
##' @inheritParams task_status
##'
##' @param allow_incomplete Should we avoid throwing an error if a task is not
##'   completed?  Used internally, and not generally needed.
##'
##' @export
task_result <- function(id, db, allow_incomplete = FALSE) {
  assert_scalar_character(id)
  db <- context_db_get(db)
  status <- task_status(id, db, FALSE)
  if (status == "COMPLETE" || status == "ERROR") {
    db$get(id, "task_results")
  } else {
    err <- UnfetchableTask(id, status)
    if (allow_incomplete) {
      err
    } else {
      stop(err)
    }
  }
}

##' Fetch expression for a task
##' @title Fetch task expression
##'
##' @inheritParams task_result
##'
##' @param locals Return locals bound to the expression (as an
##'   attribute "locals")
##'
##' @export
task_expr <- function(id, db, locals = FALSE) {
  t <- task_read(id, db)
  ret <- t$expr
  if (locals) {
    ## TODO: I don't think that this gets the *value* of the locals,
    ## so don't know how useful this is.  Consider rewriting so that
    ## we always do this, but the switch controls whether we look the
    ## value.
    ##
    ## attr(ret, "locals") <- db$export(list(), t$objects, "objects")
    attr(ret, "locals") <- t$objects
  }
  ret
}

##' Fetch function name for a task
##' @title Fetch task function name
##' @inheritParams task_status
##' @export
task_function_name <- function(ids, db) {
  if (length(ids) == 1L) {
    paste(deparse(task_expr(ids, db, FALSE)[[1L]]), collapse = " ")
  } else {
    vcapply(ids, task_function_name, db)
  }
}

##' Return the log of a task, if enabled.
##'
##' The returned object is of class \code{task_log}, which has a print
##' method that will nicely display.  Output is grouped into phases.
##' @title Return task log
##'
##' @inheritParams task_result
##'
##' @param root A context root (not just the db as in
##'   \code{\link{task_result}} as we need to know the actual path to
##'   the root).  A \code{context} object is also OK.
##'
##' @export
task_log <- function(id, root) {
  root <- context_root_get(root)
  db <- root$db
  path <- tryCatch(db$get(id, "log_path"),
                   KeyError = function(e) stop("Logging not enabled"))
  if (is_relative_path(path)) {
    path <- file.path(root$path, path)
  }
  if (is_directory(path)) {
    path <- file.path(path, id)
  }
  if (!file.exists(path)) {
    stop("Logfile does not exist at ", path)
  }
  parse_context_log(readLines(path, warn = FALSE))
}

##' Fetch times taken to queue, run, and since running a task.
##'
##' @title Fetch task times
##'
##' @inheritParams task_status
##'
##' @param unit_elapsed Elapsed time unit.  The default is "secs".
##'   This is passed to the \code{as.numeric} method of a
##'   \code{difftime} object.
##'
##' @param sorted Sort the output in terms of submitted time?  If
##'   \code{FALSE} then the output is sorted based on task ids.
##'
##' @export
##' @author Rich FitzJohn
task_times <- function(ids, db, unit_elapsed = "secs", sorted = TRUE) {
  db <- context_db_get(db)
  n <- length(ids)
  if (length(ids) == 0L) {
    t <- empty_time()
    ret <- data.frame(task_id = character(0),
                      submitted = t,
                      started = t,
                      finished = t,
                      waiting = numeric(0),
                      running = numeric(0),
                      idle = numeric(0),
                      stringsAsFactors = FALSE)
  } else {
    ns <- c("task_time_sub", "task_time_beg", "task_time_end")
    m <- length(ns)
    ex <- as.POSIXct(NA)
    res <- db$mget(rep(ids, each = m), rep(ns, n), missing = ex)
    res <- structure(vapply(res, identity, ex), class = class(ex))
    i <- seq(1, by = m, length.out = n)
    ret <- data.frame(task_id = ids,
                      submitted = res[i],
                      started = res[i + 1L],
                      finished = res[i + 2L],
                      stringsAsFactors = FALSE)
    if (sorted) {
      ret <- ret[order(ret$submitted), ]
    }
    rownames(ret) <- NULL
    started2  <- ret$started
    finished2 <- ret$finished
    now <- Sys.time()
    finished2[is.na(finished2)] <- started2[is.na(started2)] <- now
    ret$waiting <- as.numeric(started2  - ret$submitted, unit_elapsed)
    ret$running <- as.numeric(finished2 - ret$started,   unit_elapsed)
    ret$idle    <- as.numeric(now       - ret$finished,  unit_elapsed)
  }
  ret
}

##' Find the context id associated with a task
##' @title Find context for a task
##' @inheritParams task_status
##' @export
task_context_id <- function(ids, db) {
  db <- context_db_get(db)
  vcapply(db$mget(ids, "task_context", missing = NA_character_), identity)
}

UnfetchableTask <- function(task_id, status) {
  msg <- sprintf("task %s is unfetchable: %s", task_id, status)
  structure(list(message = msg, task_id = task_id, task_status = status),
            class = c("UnfetchableTask", "error", "condition"))
}
