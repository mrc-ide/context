##' Save and reload tasks.  Tasks consist of an expression bound to a
##' \code{context}.
##'
##' @title Sve and reload tasks
##'
##' @param expr An expression to save
##'
##' @param context Either a \code{context} or a \code{context_handle}
##'   object.  If missing (or NULL) one will be automatically
##'   generated, using \code{envir} and \code{root}.  See
##'   \code{\link{save_context}}.
##'
##' @param envir Passed through to \code{save_context} when creating
##'   contexts automatically.
##'
##' @param root Root directory to store and retrieve files.
##' @export
##' @rdname task
save_task <- function(expr, context=NULL, envir=parent.frame(),
                      root=tempdir()) {
  if (is.null(context)) {
    context <- save_context(auto=TRUE, envir=envir, root=root)
  } else if (!is.context_handle(context) || is.context(context)) {
    stop("Invalid context")
  } else {
    if (missing(root)) {
      root <- context$root
    } else if (!identical(root, context$root)) {
      stop("Disagreement about the root")
    }
  }
  ## TODO: we might want to check that the context *exists* in the given root.
  ##   if (!context_exists(context$id, root)) {
  ##     stop("Context not found")
  ##   }
  dat <- store_expression(expr, envir)
  dat$context_id <- context$id
  dat$id <- random_id()
  class(dat) <- "task"
  dir.create(path_tasks(root), FALSE, TRUE)
  saveRDS(dat, path_tasks(root, dat[["id"]]))
  task_handle(dat$id, root)
}

##' @rdname task
##' @param install Should missing packages be installed?
##' @param handle A handle to load the task
##' @export
load_task <- function(handle, install=TRUE, envir=.GlobalEnv) {
  dat <- read_task(handle)
  ## This approch has worked well for rrqueue, so keeping it going here.
  context <- context_handle(dat$context_id, handle$root)
  dat$envir_context <- load_context(context, install, envir)
  dat$envir <- restore_locals(dat, dat$envir_context)
  dat
}

##' @rdname task
##' @export
read_task <- function(handle) {
  if (is.task(handle)) {
    handle
  } else if (is.task_handle(handle)) {
    readRDS(path_tasks(handle$root, handle$id))
  } else {
    stop("handle must be a task or task_handle")
  }
}

##' Fetch result from completed task.
##' @title Fetch task result
##' @param handle A task handle
##' @export
task_result <- function(handle) {
  filename <- path_results(handle$root, handle$id)
  if (!file.exists(filename)) {
    stop("Task does not have results")
  }
  readRDS(filename)
}

save_task_results <- function(handle, value) {
  path_result <- path_results(handle$root, handle$id)
  context_log("result", path_result)
  saveRDS(value, path_result)
}

task_handle <- function(id, root) {
  structure(list(id=id, root=root), class="task_handle")
}
is.task_handle <- function(x) {
  inherits(x, "task_handle")
}
is.task <- function(x) {
  inherits(x, "task")
}

##' @export
print.task_handle <- function(x, ...) {
  print_ad_hoc(x)
}
##' @export
print.task <- function(x, ...) {
  print_ad_hoc(x)
}

##' Run a task
##' @title Run a task
##' @param handle Task handle
##' @param install Install packages when constructing context?
##' @param envir Environment to load global variables into.
##' @export
run_task <- function(handle, install=FALSE, envir=.GlobalEnv) {
  context_log("root", handle$root)
  context_log("task", handle$id)
  dat <- load_task(handle, install, envir)
  context_log("expr", capture.output(print(dat$expr)))
  dir.create(path_results(handle$root), FALSE, TRUE)
  context_log("start", Sys_time())
  value <- eval(dat$expr, dat$envir)
  save_task_results(handle, value)
  context_log("end", Sys_time())
  invisible(value)
}
