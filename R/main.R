main <- function(args=commandArgs(TRUE)) {
  context_log_start()
  context_log("init", Sys_time())
  context_log("version", packageVersion(.packageName))
  args <- main_parse_args(args)

  h <- task_handle(args$root, args$id)
  cores <- Sys.getenv("CONTEXT_CORES")

  if (cores != "" && is.null(par$cl)) {
    context_log("debug", "running as parallel job")
    ctx <- context_handle(h$root, task_read(h)$context_id)
    start_parallel_cluster(as.integer(cores), ctx)
    on.exit(stop_parallel_cluster())
  } else {
    context_log("debug", "running as single core job")
  }

  res <- task_run(h, install=TRUE, envir=.GlobalEnv)

  propagate_error <- toupper(Sys.getenv("CONTEXT_PROPAGATE_ERROR")) == "TRUE"

  if (is_error(res) && propagate_error) {
    msg <- attr(res, "condition")$message
    stop("Error while running task:\n", msg)
  }
  invisible()
}

main_parse_args <- function(args) {
  nargs <- length(args)
  usage <- "context <root> <task_id>"
  if (nargs != 2L) {
    stop("Exactly two arguments required\nUsage: ", usage,
         "(given: ", paste(args), ")",
         call.=FALSE)
  }
  list(root=args[[1]], id=args[[2]])
}

## This won't work on windows; need to find out how to get this to
## work there; probably just have to write a batch file I suspect.

##' Install a script that runs the context.  This won't work on
##' windows, but I'll get that sorted soon.
##'
##' This script requires that context (and its dependencies are
##' installed in a library that is readable on startup (e.g., the user
##' or site lib).  If it is not installed, then this script will fail.
##'
##' @title Install script
##' @param path Path to install the script, ideally on your \code{$PATH}.
##'   This directory must exist.
##' @export
install_context <- function(path) {
  code <- c("#!/usr/bin/env Rscript", "library(methods)", "context:::main()")
  dest <- file.path(path, "context")
  writeLines(code, dest)
  Sys.chmod(dest, "0755")
  invisible(dest)
}
