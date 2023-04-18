##' Run a task
##' @title Run a task
##' @param id A task identifier
##'
##' @param context A context object
##'
##' @param filename Filename to log \emph{all} output to.  This will
##'   sink the message stream as well as the output stream, so if
##'   specified (i.e., is non-NULL) then this function will apparently
##'   print no output to the console, which will make debugging more
##'   difficult when run interactively.  However, when run
##'   non-interactively, especially on remote servers, this will allow
##'   collection of diagnostics that facilitate debugging.
##'
##' @export
task_run <- function(id, context, filename = NULL) {
  ## TODO: can remove filename and capture_log
  if (!is.null(filename)) {
    return(capture_log(task_run(id, context), filename))
  }

  assert_is(context, "context")
  if (is.null(context$envir)) {
    stop("context is not loaded")
  }

  root <- context$root
  db <- root$db
  context_log("root", root$path)
  context_log("context", context$id)
  context_log("task", id)

  dat <- task_load(id, context)

  context_log("expr", capture.output(print(dat$expr)))
  context_log("start", Sys_time())
  db$set(id, TASK_RUNNING, "task_status")
  db$set(id, Sys.time(), "task_time_beg")

  res <- eval_safely(dat$expr, dat$envir, "context_task_error", 3L)
  value <- res$value

  if (res$success) {
    context_log("ok", "")
    status <- TASK_COMPLETE
  } else {
    context_log("error", "")
    message(sub("\n$", "", paste(as.character(value), collapse = "\n")))
    status <- TASK_ERROR
  }

  db$set(id, value, "task_results")
  db$set(id, Sys.time(), "task_time_end")
  ## NOTE: Set this one *last* so that we can listen on the status and
  ## always be sure to get the results.
  db$set(id, status, "task_status")

  context_log("end", Sys_time())
  invisible(value)
}

task_load <- function(id, context) {
  root <- context$root
  dat <- task_read(id, root)
  dat$envir <- restore_locals(dat, context$envir, root$db)
  dat
}

##' Reset tasks
##' @title Reset status and submission time of tasks
##' @param id A vector of task identifiers
##'
##' @param context A context object
##'
##' @export
task_reset <- function(id, context) {
  assert_is(context, "context")
  db <- context_db_get(context)
  db$mset(id, rep(list(TASK_PENDING), length(id)), "task_status")
  db$mset(id, rep(Sys.time(), length(id)), "task_time_sub")
  db$mset(id, rep(NA, length(id)), "task_time_beg")
  db$mset(id, rep(NA, length(id)), "task_time_end")
  db$del(id, "task_results")
  id
}
