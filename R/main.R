main <- function(args=commandArgs(TRUE)) {
  context_log_start()
  context_log("init", Sys_time())
  context_log("version", packageVersion(.packageName))
  args <- main_parse_args(args)
  run_task(task_handle(args$id, args$root), .GlobalEnv)
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
install_context <- function(path) {
  code <- c("#!/usr/bin/env Rscript", "library(methods)", "context:::main()")
  dest <- file.path(path, "context")
  writeLines(code, dest)
  Sys.chmod(dest, "0755")
}
