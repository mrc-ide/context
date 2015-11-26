TASK_PENDING  <- "PENDING"
TASK_RUNNING  <- "RUNNING"
TASK_COMPLETE <- "COMPLETE"
TASK_ERROR    <- "ERROR"

TASK_ORPHAN   <- "ORPHAN"
TASK_REDIRECT <- "REDIRECT"
TASK_MISSING  <- "MISSING"

##' Save and reload tasks.  Tasks consist of an expression bound to a
##' \code{context}.
##'
##' @title Save and reload tasks
##'
##' @param expr An expression to save
##'
##' @param context Either a \code{context} or a \code{context_handle}
##'   object.  If missing (or NULL) one will be automatically
##'   generated, using \code{envir} and \code{root}.  See
##'   \code{\link{context_save}}.
##'
##' @param envir Passed through to \code{context_save} when locating
##'   local variables.
##'
##' @export
##' @rdname task
task_save <- function(expr, context, envir=parent.frame()) {
  task_save_list(list(expr), context, envir)[[1]]
}

##' @export
##' @rdname task
##' @param list a \code{list} of tasks, all to be evaluted in the same context.
task_save_list <- function(list, context, envir=parent.frame()) {
  if (!is.list(list)) {
    stop("Expected a list")
  }
  if (!is.context_handle(context)) {
    stop("Invalid context")
  }
  root <- context$root
  ## TODO: we might want to check that the context *exists* in the given root.
  ##   if (!context_exists(context$id, root)) {
  ##     stop("Context not found")
  ##   }
  f <- function(x) {
    dat <- store_expression(x, envir)
    dat$context_id <- context$id
    dat$id <- random_id()
    dat$root <- root
    class(dat) <- "task"
    dir.create(path_tasks(root), FALSE, TRUE)
    dir.create(path_task_status(root), FALSE, TRUE)
    saveRDS(dat, path_tasks(root, dat[["id"]]))
    task_status_set(dat, TASK_PENDING)
    dat$id
  }
  task_handle(root, vcapply(list, f), FALSE)
}

##' @rdname task
##' @param install Should missing packages be installed?
##' @param handle A handle to load the task
##' @export
task_load <- function(handle, install=TRUE, envir=.GlobalEnv) {
  dat <- task_read(handle)
  ## This approch has worked well for rrqueue, so keeping it going here.
  context <- context_handle(dat$context_id, handle$root)
  dat$envir_context <- context_load(context, install, envir)
  dat$envir <- restore_locals(dat, dat$envir_context)
  dat
}

##' @rdname task
##' @export
task_read <- function(handle) {
  if (is.task(handle)) {
    handle
  } else if (is.task_handle(handle)) {
    readRDS(path_tasks(handle$root, handle$id))
  } else {
    stop("handle must be a task or task_handle")
  }
}

##' @rdname task
##' @export
##' @param root root
tasks_list <- function(root) {
  ## TODO: this becomes way easier with storr
  dir(path_tasks(root))
}

##' Fetch result from completed task.
##' @title Fetch task result
##' @param handle A task handle
##' @export
task_result <- function(handle) {
  filename <- path_task_results(handle$root, handle$id)
  if (!file.exists(filename)) {
    stop("Task does not have results")
  }
  readRDS(filename)
}

task_save_results <- function(handle, value) {
  path_result <- path_task_results(handle$root, handle$id)
  err <- is_error(value)
  context_log(if (err) "error" else "result", path_result)
  task_status_set(handle, if (err) TASK_ERROR else TASK_COMPLETE)
  saveRDS(value, path_result)
}

##' Create a handle to a task
##' @title Create task handle
##' @param root Context root
##' @param id Task identifier
##' @param check_exists Check that the task exists as the handle is created?  If
##'  \code{TRUE} then \code{task_handle} throws if creating a nonexistant task.
##' @export
task_handle <- function(root, id, check_exists=TRUE) {
  if (!is.character(id)) {
    stop("id must be a character")
  }
  if (check_exists) {
    ok <- file.exists(path_tasks(root, id))
    if (!all(ok)) {
      stop("tasks do not exist: ", id[!ok])
    }
  }
  structure(list(id=id, root=root), class="task_handle")
}

## Not yet exported:
is.task_handle <- function(x) {
  inherits(x, "task_handle")
}
is.task <- function(x) {
  inherits(x, "task")
}

##' @export
length.task_handle <- function(x) {
  length(x$id)
}

##' @export
`[.task_handle` <- function(x, i, ...) {
  x$id <- x$id[i]
  x
}

##' @export
`[[.task_handle` <- function(x, i, ...) {
  x$id <- x$id[[i]]
  x
}

##' @export
print.task_handle <- function(x, ...) {
  print_ad_hoc(x)
}

##' Run a task
##' @title Run a task
##' @param handle Task handle
##' @param install Install packages when constructing context?
##' @param envir Environment to load global variables into.
##' @export
task_run <- function(handle, install=FALSE, envir=.GlobalEnv) {
  context_log("root", handle$root)
  context_log("task", handle$id)
  dat <- task_load(handle, install, envir)
  context_log("expr", capture.output(print(dat$expr)))
  dir.create(path_task_results(handle$root), FALSE, TRUE)
  context_log("start", Sys_time())
  task_status_set(handle, TASK_RUNNING)
  value <- try(eval(dat$expr, dat$envir))
  task_save_results(handle, value)
  context_log("end", Sys_time())
  invisible(value)
}

## TODO: need to write a status object too (after porting to storr) so
## that SUCCESS/FAILED is OK.
##
## At the same time, this actually needs adding to the context so that
## on failure:
##
##   - write out the failed task by running with try().  Consider
##     dumping frames.
##   - throw on exit so system still registers the failure.

##' Task status
##' @title Task status
##' @param handle Task handle
##' @export
task_status_read <- function(handle) {
  path <- path_task_status(handle$root, handle$id)
  ok <- file.exists(path)
  res <- character(length(path))
  res[ok] <- vapply(path[ok], readLines, character(1))
  res[!ok] <- TASK_MISSING
  res
}

task_status_set <- function(handle, status) {
  writeLines(status, path_task_status(handle$root, handle$id))
}

## TODO: decide if this triggers gc.  In general this is a dangerous
## operation because non-filesystem things could depend on the context
## (e.g. a queue object that hasn't written any tasks yet).  The
## solution here is for contexts to exist in memory too, which we'll
## get for free with storr.

##' Delete a task, including its results.
##' @title Delete a task
##' @param handle A task handle
##' @export
##' @return \code{TRUE} if a task was actually deleted.
task_delete <- function(handle) {
  ok <- file_remove(path_tasks(handle$root, handle$id),
                    path_task_status(handle$root, handle$id),
                    path_task_results(handle$root, handle$id))
  invisible(any(ok))
}
