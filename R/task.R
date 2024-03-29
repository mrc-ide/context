TASK_PENDING  <- "PENDING"
TASK_RUNNING  <- "RUNNING"
TASK_COMPLETE <- "COMPLETE"
TASK_ERROR    <- "ERROR"

TASK_ORPHAN   <- "ORPHAN"
TASK_REDIRECT <- "REDIRECT"
TASK_MISSING  <- "MISSING"
TASK_CANCELLED <- "CANCELLED"

##' Save and reload tasks.  Tasks consist of an expression bound to a
##' \code{context}.
##'
##' @title Save and reload tasks
##'
##' @param expr An expression to save
##'
##' @param context A \code{context} object
##'
##' @param envir Passed through to \code{context_save} when locating
##'   local variables.
##'
##' @param depends_on Optional vector of task ids that this task
##'   depends on
##'
##' @return An identifier that can be used to retrieve or run the task
##'   later.  This is simply a short string.
##'
##' @export
##' @rdname task
task_save <- function(expr, context, envir = parent.frame(), depends_on = NULL) {
  ## NOTE: until updated, this requires a single expression now.  I'll
  ## get support for a series of related tasks in in another update, I
  ## think.  With more than one task we can either do 3 mset updates,
  ## n updates or or one massive one.
  assert_is(context, "context")
  assert_is(expr, "call")
  verify_dependencies_exist(depends_on, context)

  db <- context_db_get(context)
  dat <- prepare_expression(expr, envir, db)
  dat$id <- ids::random_id()
  dat$context_id <- context$id
  class(dat) <- "task"
  db$mset(dat$id,
          list(dat, TASK_PENDING, context$id, Sys.time()),
          c("tasks", "task_status", "task_context", "task_time_sub"))
  if (!is.null(depends_on)) {
    db$set(dat$id, depends_on, "task_deps")
  }
  dat$id
}

verify_dependencies_exist <- function(depends_on, context) {
  if (!is.null(depends_on)) {
    dependencies_exist <- task_exists(depends_on, context)
    if (!all(dependencies_exist)) {
      missing <- unique(depends_on[!dependencies_exist])
      error_msg <- ngettext(
        length(missing),
        "Failed to save as dependency %s does not exist.",
        "Failed to save as dependencies %s do not exist.")
      stop(sprintf(error_msg, paste0(missing, collapse = ", ")))
    }
  }
}

##' Delete a task, including its results.
##' @title Delete a task
##' @inheritParams task_status
##' @inheritParams task_log
##' @export
##' @return \code{TRUE} if a task was actually deleted.
task_delete <- function(ids, root) {
  root <- context_root_get(root)
  db <- root$db
  n <- length(ids)
  ns <- c("tasks", "task_status", "task_results")
  if (n == 1L) {
    res <- any(db$del(ids, ns))
  } else {
    m <- length(ns)
    res <- db$del(rep(ids, each = m), rep(ns, n))
    res <- apply(matrix(res, m, n), 2, any)
  }
  ## TODO: delete the log if it is present (for this reason, this
  ## function takes 'root' not 'db')
  invisible(res)
}

task_context <- function(ids, db) {
  db <- context_db_get(db)
  vcapply(db$mget(ids, "task_context", missing = NA_character_), identity)
}

##' @export
print.task <- function(x, ...) {
  print_ad_hoc(x)
}

task_read <- function(id, db) {
  db <- context_db_get(db)
  ret <- db$get(id, "tasks")
  ret$db <- db
  ret
}


##' Run a task in a separate process, using [callr::r]. Unlike
##' [context::task_run] this does not return the value, and is called
##' for the side effect of writing to the context.
##'
##' @title Run a task in separate process
##'
##' @inheritParams context_read
##'
##' @param task_id A task identifier
##'
##' @param path_log Path to log file
##'
##' @export
task_run_external <- function(root, identifier, task_id, path_log) {
  dir.create(dirname(path_log), FALSE, TRUE)
  args <- list(root, identifier, task_id)
  callr::r(task_run_external_helper, args, package = "context",
           stderr = path_log, stdout = path_log)
}


task_run_external_helper <- function(root, identifier, task_id) {
  ctx <- context::context_load(context::context_read(identifier, root))
  context::task_run(task_id, ctx)
  invisible()
}
