##' @rdname task
##' @export
##' @param root root
task_list <- function(root) {
  context_db_get(root)$list("tasks")
}

##' Task status
##' @title Task status
##' @param handle Task handle
##' @param named Name the output with the task ids?
##' @export
task_status <- function(ids, root, named = FALSE) {
  db <- context_db_get(root)
  vcapply(db$mget(ids, "task_status", missing = TASK_MISSING),
          identity, USE.NAMES = named)
}

##' Fetch result from completed task.
##' @title Fetch task result
##' @param handle A task handle
##'
##' @param sanitise Should we avoid throwing an error if a task is not
##'   completed?  Used internally, and not generally needed.
##'
##' @export
task_result <- function(id, root, sanitise = FALSE) {
  db <- context_db_get(root)
  status <- task_status(id, db, FALSE)
  if (status == "COMPLETE" || status == "ERROR") {
    db$get(id, "task_results")
  } else {
    err <- UnfetchableTask(id, status)
    if (sanitise) {
      err
    } else {
      stop(err)
    }
  }
}

##' Fetch expression for a task
##' @title Fetch task expression
##' @param handle A task handle
##'
##' @param locals Return locals bound to the expression (as an
##'   attribute "locals")
##'
##' @export
task_expr <- function(id, root, locals = FALSE) {
  t <- task_read(id, root)
  ret <- t$expr
  if (locals) {
    attr(ret, "locals") <- t$objects
  }
  ret
}

##' @rdname task_expr
##' @export
task_function_name <- function(id, root) {
  paste(deparse(task_expr(id, root, FALSE)[[1L]]), collapse = " ")
}

##' Return the log of a task, if enabled.
##'
##' The returned object is of class \code{task_log}, which has a print
##' method that will nicely display.  Output is grouped into phases.
##' @title Return task log
##' @inheritParams task_handle
##' @export
task_log <- function(id, root) {
  db <- context_db_get(root)
  ## TODO: I wonder if it's sensible to allow context to set a global
  ## log path here?
  path <- tryCatch(db$get(id, "log_path"),
                   KeyError = function(e) stop("Logging not enabled"))
  ## TODO: Need to check if this is a relative path -- pathr contains
  ## things for this.
  if (is_relative_path(path)) {
    path <- file.path(context_root_get(root)$path, path)
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
##' @param handle A task handle.  If the task handle has a
##'   \emph{vector} of ids in it, then it represents a number of
##'   tasks.  This will create a data.frame with that many rows.
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
task_times <- function(ids, root, unit_elapsed = "secs", sorted = TRUE) {
  db <- context_db_get(root)
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

UnfetchableTask <- function(task_id, status) {
  msg <- sprintf("task %s is unfetchable: %s", task_id, status)
  structure(list(message = msg, task_id = task_id, task_status = status),
            class = c("UnfetchableTask", "error", "condition"))
}
