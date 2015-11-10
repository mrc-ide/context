env_chain <- function(e, stop_at=list(.GlobalEnv, emptyenv())) {
  ret <- list()
  while (!any(vlapply(stop_at, identical, e))) {
    ret <- c(ret, e)
    e <- parent.env(e)
  }
  c(ret, e)
}

cleanup <- function(root) {
  unlink(root, recursive=TRUE)
  ## This prunes libPaths down to the set of files that exist, so with
  ## the above should do a reasonable job of trimming any additions
  ## because the actual directories will have been deleted.
  .libPaths(.libPaths())
}

capture_messages <- function(expr) {
  msgs <- character(0)
  res <- withCallingHandlers(expr, message=function(e) {
    msgs <<- c(msgs, e$message)
    e
  })
  attr(msgs, "result") <- res
  msgs
}

skip_if_no_fork <- function() {
  if (exists("mcfork", getNamespace("parallel"))) {
    return()
  }
  stop("Fork is not available")
}
