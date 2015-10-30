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
  }
  ## TODO: might be worth warning if root and context$root (iff
  ## context is a context handle) differ.  Otherwise restoring may not
  ## go as expected.  More generally, we might want to check that the
  ## context *exists* in the given root.
  ##   if (!context_exists(context$id, root)) {
  ##     stop("Context not found")
  ##   }
  dat <- store_expression(expr, envir)
  dat$context_id <- context$id
  dat$id <- random_id()
  dir.create(path_tasks(root), FALSE, TRUE)
  saveRDS(dat, path_tasks(root, dat[["id"]]))
  task_handle(dat$id, root)
}

##' @rdname task
##' @param handle A handle to load the task
##' @export
load_task <- function(handle, envir=.GlobalEnv) {
  if (!is.task_handle(handle)) {
    stop("handle must be a task_handle")
  }
  id <- handle$id
  root <- handle$root
  dat <- readRDS(path_tasks(root, id))

  ## This approch has worked well for rrqueue, so keeping it going here.
  context <- context_handle(dat$context_id, root)
  dat$envir_context <- load_context(context, envir)
  dat$envir <- restore_locals(dat, dat$envir_context)

  dat
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

run_task <- function(handle, envir=.GlobalEnv) {
  context_log("root", handle$root)
  dat <- load_task(handle, envir)
  context_log("expr", capture.output(print(dat$expr)))
  dir.create(path_results(handle$root), FALSE, TRUE)
  context_log("start", Sys_time())
  value <- eval(dat$expr, dat$envir)
  save_task_results(handle, value)
  context_log("end", Sys_time())
  invisible(value)
}
