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
  if (!(is.context_handle(context) || is.context(context))) {
    stop("Invalid context")
  }
  root <- context$root
  db <- context_db(context)

  if (!db$exists(context$id, namespace="contexts")) {
    stop(sprintf("Context %s does not exist", context$id))
  }

  f <- function(x) {
    dat <- store_expression(x, envir, db)
    dat$context_id <- context$id
    db$set(dat$id, dat, namespace="tasks")
    db$set(dat$id, TASK_PENDING, namespace="task_status")
    db$set(dat$id, Sys.time(), namespace="task_time_sub")
    dat$id
  }

  task_handle(context, vcapply(list, f), FALSE)
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

  class(dat) <- "task"
  dat
}

##' @rdname task
##' @export
task_read <- function(handle) {
  if (is.task(handle)) {
    ret <- handle
  } else if (is.task_handle(handle)) {
    db <- context_db(handle)
    ret <- db$get(handle$id, namespace="tasks")
    ret$db <- db
    ret
  } else {
    stop("handle must be a task or task_handle")
  }
  ret
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
##'
##' @param sanitise Should we avoid throwing an error if a task is not
##'   completed?  Used internally, and not generally needed.
##'
##' @export
task_result <- function(handle, sanitise=FALSE) {
  status <- task_status(handle, FALSE)
  if (status == "COMPLETE" || status == "ERROR") {
    context_db(handle)$get(handle$id, "task_results")
  } else {
    err <- UnfetchableTask(handle$id, status)
    if (sanitise) {
      err
    } else {
      stop(err)
    }
  }
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
  if (inherits(root, "task_handle") &&
      (is.null(id) || identical(id, root$id))) {
    return(root)
  }
  ## I don't think this one needs to care where root is if it can get
  ## the db.  Consider entirely dropping the root in favour of
  ## _always_ including the db.
  if (!is.character(id)) {
    stop("id must be a character")
  }
  db <- context_db(root)
  root <- context_root(root)
  ret <- structure(list(root=root, id=id, db=db), class="task_handle")
  if (check_exists) {
    ok <- vlapply(id, db$exists, "tasks")
    if (!all(ok)) {
      stop("tasks do not exist: ", id[!ok])
    }
  }
  ret
}

##' Fetch expression for a task
##' @title Fetch task expression
##' @param handle A task handle
##'
##' @param locals Return locals bound to the expression (as an
##'   attribute "locals")
##'
##' @export
task_expr <- function(handle, locals=FALSE) {
  t <- task_read(handle)
  ret <- t$expr
  if (locals) {
    attr(ret, "locals") <- t$objects
  }
  ret
}

##' @rdname task_expr
##' @export
task_function_name <- function(handle) {
  paste(deparse(task_expr(handle)[[1L]]), collapse=" ")
}

##' Return the log of a task, if enabled
##' @title Return task log
##' @inheritParams task_handle
##' @export
task_log <- function(root, id) {
  db <- context_db(root)
  root <- context_root(root)
  path <- tryCatch(db$get(id, "log_path"),
                   error=function(e) stop("Logging not enabled"))
  ## TODO: Need to check if this is a relative path -- pathr contains
  ## things for this.
  if (is_relative_path(path)) {
    path <- file.path(root, path)
  }
  if (is_dir(path)) {
    path <- file.path(path, id)
  }
  if (!file.exists(path)) {
    stop("Logfile does not exist at ", path)
  }
  parse_context_log(readLines(path, warn=FALSE))
}

## TODO: why is this not in context?
##
## TODO: It might be useful to have a "sorted" option here, because
## it's a bit confusing that when a specific list of tasks is given
## the output is a different order.  However, because the task_ids
## might be generated at any point above this it's hard to tell when a
## sorted/unsorted list is wanted.

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
tasks_times <- function(handle, unit_elapsed="secs", sorted=TRUE) {
  db <- context::context_db(handle)
  task_ids <- handle$id
  if (length(task_ids) == 0L) {
    empty_time <- Sys.time()[-1]
    ret <- data.frame(task_id=character(0),
                      submitted=empty_time,
                      started=empty_time,
                      finished=empty_time,
                      waiting=numeric(0),
                      running=numeric(0),
                      idle=numeric(0),
                      stringsAsFactors=FALSE)
  } else {
    f <- function(ids, type) {
      NA_POSIXct <- as.POSIXct(NA)
      gett <- function(id) {
        tryCatch(db$get(id, type),
                 KeyError=function(e) NA_POSIXct)
      }
      res <- lapply(ids, gett)
      ret <- unlist(res)
      class(ret) <- c("POSIXct", "POSIXt")
      ret
    }
    ret <- data.frame(task_id   = task_ids,
                      submitted = f(task_ids, "task_time_sub"),
                      started   = f(task_ids, "task_time_beg"),
                      finished  = f(task_ids, "task_time_end"),
                      stringsAsFactors=FALSE)
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
##' @export
task_run <- function(handle, install=FALSE, envir=.GlobalEnv, filename=NULL,
                     print_error=TRUE) {
  if (!is.null(filename)) {
    return(capture_log(task_run(handle, install, envir, NULL), filename))
  }
  db <- context_db(handle)
  context_log("root", handle$root)
  context_log("task", handle$id)
  dat <- task_load(handle, install, envir)
  context_log("expr", capture.output(print(dat$expr)))
  context_log("start", Sys_time())
  db$set(handle$id, TASK_RUNNING, "task_status")
  db$set(handle$id, Sys.time(), "task_time_beg")

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
                                        error=handler,
                                        warning=function(e) warnings$add(e)),
                    error=function(e) error)

  err <- !is.null(error)
  if (err && print_error) {
    message(paste(as.character(error), collapse="\n"))
  }

  context_log(if (err) "error" else "ok", "")
  db$set(handle$id, if (err) TASK_ERROR else TASK_COMPLETE, "task_status")
  db$set(handle$id, value, "task_results")

  db$set(handle$id, Sys.time(), "task_time_end")
  context_log("end", Sys_time())
  invisible(value)
}

##' @export
as.character.context_task_error <- function(x, ...) {
  call <- conditionCall(x)
  if (!is.null(call)) {
    dcall <- deparse(call)[1L]
    prefix <- sprintf("Error in:\n    %s", dcall)
  } else {
    prefix <- "Error:"
  }

  ret <- paste0(prefix, "\n", conditionMessage(x), "\n")

  if (length(x$trace) > 0L) {
    ret <- c(ret,
             "trace: (outermost first)",
             sprintf("  %d: %s", seq_along(x$trace), x$trace))
  }

  nw <- length(x$warnings)
  if (nw > 0L) {
    calls <- lapply(x$warnings, "[[", "call")
    msgs <- vcapply(x$warnings, "[[", "message")
    tmp <- capture.output(print.warnings(structure(setNames(calls, msgs))))
    ret <- c(ret,
             ngettext(nw, "There was 1 warning",
                      sprintf("There were %d warnings:", nw)),
             tmp[-1])
  }
  ret
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
##' @param named Name the output with the task ids?
##' @export
task_status <- function(handle, named=FALSE) {
  ## TODO: rename -> tasks_status
  ## TODO: in storr, add a missing action wrapper here?
  db <- context_db(handle)
  f <- function(id) {
    tryCatch(db$get(id, "task_status"),
             KeyError=function(e) TASK_MISSING)
  }
  vcapply(handle$id, f, USE.NAMES=named)
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

UnfetchableTask <- function(task_id, status) {
  msg <- sprintf("task %s is unfetchable: %s", task_id, status)
  structure(list(message=msg, task_id=task_id, task_status=status),
            class=c("UnfetchableTask", "error", "condition"))
}
