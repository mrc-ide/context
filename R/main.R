main <- function(args=commandArgs(TRUE)) {
  context_log_start()
  context_log("init", Sys_time())
  context_log("version", packageVersion(.packageName))
  args <- main_parse_args(args)
  run_task(task_handle(args$id, args$root), install=TRUE, envir=.GlobalEnv)
  invisible()
}

main_parse_args <- function(args) {
  nargs <- length(args)
  usage <- "context <id> <root>"
  if (nargs != 2L) {
    stop("Exactly two arguments required\nUsage: ", usage,
         call.=FALSE)
  }
  list(id=args[[1]], root=args[[2]])
}

## This won't work on windows; need to find out how to get this to
## work there; probably just have to write a batch file I suspect.

##' Install a script that runs the context.  This won't work on
##' windows, but I'll get that sorted soon.
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

write_runner <- function(path) {
  env <- environment(install_context)
  funs <- vcapply(c("main_parse_args", find_funcs(main_parse_args, env)),
                  fun_to_str, env, USE.NAMES=FALSE)

  ## This code is a bit horiffic because we need to get everything in
  ## place so that context will be found.  That requires getting just
  ## enough so that use_local_library is found, and parsing the
  ## arguments to get the correct version.  This makes a much less
  ## robust runner than usual.  So at the same time we'll arrange to
  ## read the bootstrap script, too!
  code <- c("#!/usr/bin/env Rscript",
            "library(methods)",
            "local({",
            funs,
            "args <- main_parse_args(commandArgs(TRUE))",
            "CONTEXT_ROOT <- args$root",
            'source(file.path(CONTEXT_ROOT, "context_bootstrap.R"), TRUE)',
            "})",
            "context:::main()")
  dest <- file.path(path, "context_runner")
  writeLines(code, dest)
  Sys.chmod(dest, "0755")
  invisible(dest)
}
