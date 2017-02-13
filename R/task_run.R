##' Run a task
##' @title Run a task
##' @param handle Task handle
##' @param envir Environment to load global variables into.
##'
##' @param filename Filename to log \emph{all} output to.  This will
##'   sink the message stream as well as the output stream, so if
##'   specified (i.e., is non-NULL) then this function will apparently
##'   print no output to the console, which will make debugging more
##'   difficult when run interactively.  However, when run
##'   non-interactively, especially on remote servers, this will allow
##'   collection of diagnostics that facilitate debugging.
##'
##' @param print_error Print information about an error if one occurs?
##'
##' @export
task_run <- function(id, root, envir = .GlobalEnv, filename = NULL,
                     print_error = TRUE, load_context = TRUE) {
  if (!is.null(filename)) {
    return(capture_log(
      task_run(id, root, envir, print_error = print_error,
               load_context = load_context), filename))
  }
  root <- context_root_get(root)
  db <- root$db
  context_log("root", root$path)
  context_log("task", id)

  dat <- task_load(id, root, envir, load_context)
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

  if (err && print_error) {
    message(sub("\n$", "", paste(as.character(error), collapse = "\n")))
  }

  db$set(id, if (err) TASK_ERROR else TASK_COMPLETE, "task_status")
  db$set(id, value, "task_results")

  db$set(id, Sys.time(), "task_time_end")
  context_log("end", Sys_time())
  invisible(value)
}

## Right, this is a bit of a faff because loading a task must
## (optionally) load a *context* because we don't know what context
## the task was saved into until we read it.
task_load <- function(id, root, parent, load_context = FALSE) {
  root <- context_root_get(root)
  dat <- task_read(id, root)
  if (load_context) {
    context <- context_read(dat$context_id, root)
    parent <- context_load(context, parent)
  }
  dat$envir <- restore_locals(dat, parent, root$db)
  dat
}
