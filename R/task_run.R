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

  warnings <- collector()
  error <- NULL
  handler <- function(e) {
    trace <- call_trace(0, 3)
    e$warnings <- warnings$get()
    e$trace <- trace
    class(e) <- c("context_task_error", class(e))
    error <<- e
    NULL
  }

  value <- tryCatch(withCallingHandlers(eval(dat$expr, dat$envir),
                                        error = handler,
                                        warning = function(e) warnings$add(e)),
                    error = function(e) error)
  err <- !is.null(error)
  context_log(if (err) "error" else "ok", "")

  if (err) {
    message(sub("\n$", "", paste(as.character(error), collapse = "\n")))
  }

  db$set(id, if (err) TASK_ERROR else TASK_COMPLETE, "task_status")
  db$set(id, value, "task_results")
  db$set(id, Sys.time(), "task_time_end")

  context_log("end", Sys_time())
  invisible(value)
}

task_load <- function(id, context) {
  root <- context$root
  dat <- task_read(id, root)
  dat$envir <- restore_locals(dat, context$envir, root$db)
  dat
}
