env_chain <- function(e, stop_at=list(.GlobalEnv, emptyenv())) {
  ret <- list()
  while (!any(vlapply(stop_at, identical, e))) {
    ret <- c(ret, e)
    e <- parent.env(e)
  }
  c(ret, e)
}
