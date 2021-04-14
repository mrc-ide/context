write_context_script <- function(path) {
  code <- c(
    "#!/usr/bin/env Rscript",
    deparse_fn("context_startup"),
    "context_startup()",
    "context:::main_task_run()")
  dir.create(path, FALSE, TRUE)
  write_script_exec(code, file.path(path, "task_run"))
}


context_startup <- function() {
  fmt <- "[ %-9s ]  %s"
  message(sprintf(fmt, "hello", Sys.time()))
  message(sprintf(fmt, "wd", getwd()))
  withCallingHandlers(
    loadNamespace("context"),
    error = function(e) {
      message("Could not find context package; aborting startup")
      message(".libPaths():\n", paste("*", .libPaths(), collapse = "\n"))
    })
  invisible()
}


parse_main_task_run <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
context_task_run <root> <id>"
  if (length(args) != 2L) {
    stop(usage, call. = FALSE)
  }
  list(root = args[[1]], id = args[[2]])
}


main_task_run <- function(args = commandArgs(TRUE)) {
  context_log("init", Sys_time())
  context_log("hostname", hostname())
  context_log("process", process_id())
  context_log("version", packageVersion("context"))

  ## There is no way of doing any error handling if we have not yet
  ## got the database loaded because we don't even know where things
  ## are being stored.  So the first step is to get the database
  ## loaded.
  ##
  ## Of course, this means that just parsing arguments (which can
  ## throw errors) can leave jobs stranded in PENDING.
  args <- parse_main_task_run(args)
  root <- context_root_get(args$root)
  db <- root$db
  task_id <- args$id

  ## This section here runs the load so that if things go pear shaped
  ## then we'll at least capture the error and set the task status
  ## appropriately to ERROR.  Unlikel running the task itself, this
  ## *always* propagates the error (so Rscript exits with status != 0)
  ## because this can never be considered an uncontrolled error.
  warnings <- collector()
  handler <- function(e) {
    db$set(task_id, TASK_ERROR, "task_status")
    trace <- call_trace(0, 3)
    e$warnings <- warnings$get()
    e$trace <- trace
    class(e) <- c("context_task_error", class(e))
    db$set(task_id, e, "task_results")
  }

  withCallingHandlers({
    context_id <- task_context(task_id, root)
    if (is.na(context_id)) {
      ## I don't think that this can be triggered, but it seems like
      ## if a bug in the package *makes* it triggerable, this will
      ## be a decent message
      stop("[context bug] No context found for task ", task_id) # nocov
    }
    ctx <- context_load(context_read(context_id, root, root$db),
                        .GlobalEnv)

    cores <- Sys.getenv("CONTEXT_CORES")
    if (nzchar(cores) && is.null(context_cache$cl)) {
      cores <- as.integer(cores)
      context_log("parallel",
                  sprintf("running as parallel job [%d cores]", cores))
      parallel_cluster_start(cores, context_read(context_id, root))
      on.exit(parallel_cluster_stop())
    } else {
      context_log("parallel", "running as single core job")
    }
  },
  error = handler,
  warning = function(e) warnings$add(e))

  ## Below here, error handling is done within task_run
  res <- task_run(task_id, ctx)

  if (inherits(res, "context_task_error")) {
    stop("Error while running task:\n", attr(res, "condition")$message)
  }

  invisible()
}
