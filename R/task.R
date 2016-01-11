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
  ## TODO: This should work equally well with contexts and context handles.
  if (!is.context_handle(context)) {
    stop("Invalid context")
  }
  root <- context$root
  db <- context_db(context)

  if (!db$exists(context$id, namespace="contexts")) {
    stop(sprintf("Context %s does not exist", context$id))
  }

  f <- function(x) {
    dat <- store_expression(x, envir)
    ## Add some extra things:
    dat$context_id <- context$id
    dat$root <- root
    class(dat) <- "task"

    db$set(dat$id, dat, namespace="tasks")
    db$set(dat$id, TASK_PENDING, namespace="task_status")
    db$set(dat$id, Sys.time(), namespace="task_time_sub")
    dat$id
  }

  ret <- task_handle(root, vcapply(list, f), FALSE)
  ret$db <- db
  ret
}

##' @rdname task
##' @param install Should missing packages be installed?
##' @param handle A handle to load the task
##' @export
task_load <- function(handle, install=TRUE, envir=.GlobalEnv) {
  dat <- task_read(handle)
  ## OK, the handling of environments here is very confusing.  The
  ## `envir` argument here is going to be the _parent_; we'll need to
  ## source things into that though.
  ##
  ## The given environment will have things sourced into it by
  ## context_load, returning an environment.
  ##
  ## If envir is .GlobalEnv this is the situation:
  ##   <packages> --> <global> --> <local> --> <expression locals>
  ##
  ## If envir is not *and* we have a local environment, then we get a
  ## mess where the local environment does not have the correct
  ## parent.
  ##
  ##   <packages> --> <R global> --> <local> --> <expression locals>
  ##       ???    --> <global>
  ##
  ## (NOTE: this is incorrect and incomplete above -- need to think
  ## about this more).
  ##
  ## Basically, this is prone to being rewritten, but if envir is the
  ## global environment and if local has the global environment as a
  ## parent (before baseenv or emptyenv) then it should all work OK.
  ##
  ## The enclos environment to eval won't help because it's ignored if
  ## the environment argument is an actual environment.
  context <- context_handle(handle$root, dat$context_id, context_db(handle))
  dat$envir_context <- context_load(context, install, envir)

  ## This approch has worked well for rrqueue, so keeping it going
  ## here.  Based on rrqueue:::task_expr(), which might be worth
  ## pulling out more cleanly for testing?
  dat$envir <- restore_locals(dat, dat$envir_context)
  dat
}

##' @rdname task
##' @export
task_read <- function(handle) {
  if (is.task(handle)) {
    handle
  } else if (is.task_handle(handle)) {
    db <- context_db(handle)
    ret <- db$get(handle$id, namespace="tasks")
    ret$db <- db
    ret
  } else {
    stop("handle must be a task or task_handle")
  }
}

##' @rdname task
##' @export
##' @param root root
tasks_list <- function(root) {
  context_db(root)$list("tasks")
}

##' Fetch result from completed task.
##' @title Fetch task result
##' @param handle A task handle
##' @export
task_result <- function(handle) {
  ## TODO: Why is this a hash error not a key error?
  tryCatch(context_db(handle)$get(handle$id, "task_results"),
           HashError=function(e) stop("Task does not have results"))
}

##' Create a handle to a task
##' @title Create task handle
##'
##' @param root Context root (or a context, or anything context can
##'   internally wrangle into its database format; see
##'   \code{\link{context_db}}).
##'
##' @param id Task identifier
##'
##' @param check_exists Check that the task exists as the handle is
##'   created?  If \code{TRUE} then \code{task_handle} throws if
##'   creating a nonexistant task.
##' @export
task_handle <- function(root, id, check_exists=TRUE) {
  ## I don't think this one needs to care where root is if it can get
  ## the db.  Consider entirely dropping the root in favour of
  ## _always_ including the db.
  if (!is.character(id)) {
    stop("id must be a character")
  }
  db <- context_db(root)
  root <- if (is.recursive(root)) root$root else root
  ret <- structure(list(root=root, id=id, db=db), class="task_handle")
  if (check_exists) {
    ok <- vlapply(id, db$exists, "tasks")
    if (!all(ok)) {
      stop("tasks do not exist: ", id[!ok])
    }
  }
  ret
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
  db <- context_db(handle)
  context_log("root", handle$root)
  context_log("task", handle$id)
  dat <- task_load(handle, install, envir)
  context_log("expr", capture.output(print(dat$expr)))
  context_log("start", Sys_time())
  db$set(handle$id, TASK_RUNNING, "task_status")
  db$set(handle$id, Sys.time(), "task_time_beg")

  value <- try(eval(dat$expr, dat$envir))
  err <- is_error(value)
  context_log(if (err) "error" else "result", "") # not sure here...
  db$set(handle$id, if (err) TASK_ERROR else TASK_COMPLETE, "task_status")
  db$set(handle$id, value, "task_results")

  db$set(handle$id, Sys.time(), "task_time_end")
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
task_status <- function(handle) {
  ## TODO: rename -> task_status
  ## TODO: in storr, add a missing action wrapper here?
  db <- context_db(handle)
  f <- function(id) {
    tryCatch(db$get(id, "task_status"),
             KeyError=function(e) TASK_MISSING)
  }
  vcapply(handle$id, f, USE.NAMES=FALSE)
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
  db <- context_db(handle)
  f <- function(id) {
    ## NOTE: not short-circuiting 'OR' here because we want 'OR' of
    ## the results but still to run all three commands.
    db$del(handle$id, "tasks") |
      db$del(handle$id, "task_status") |
      db$del(handle$id, "task_results")
  }
  invisible(any(vlapply(handle$id, f, USE.NAMES=FALSE)))
}
