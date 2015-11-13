## I probably should use loggr here, but I can swap this out easily
## enough later.

##' Start and stop the context log.  Soon this might swap out for
##' \code{loggr}, but for now this should do.  When active, some
##' actions will print diagnostic information to the message stream.
##' This is used in particular by the program installed by
##' \code{\link{install_context}}.
##'
##' The interface here will change by adding arguments.  Future versions
##' may support logging to a file.
##' @title Start and stop log
##' @export
##' @rdname context_log
context_log_start <- function() {
  options(context.log=TRUE)
}
##' @export
##' @rdname context_log
context_log_stop <- function() {
  options(context.log=NULL)
}
context_log <- function(topic, value) {
  if (isTRUE(getOption("context.log"))) {
    message(trimws(sprintf("[ %-9s ]  %s", topic, value)))
  }
}

##' @export
##' @rdname context_log
##' @param x Vector of log output
parse_context_log <- function(x) {
  re <- "^\\[ (.{9}) \\](.*)$"
  i <- grep(re, x)

  title <- trimws(sub(re, "\\1", x[i]))
  value <- sub(re, "\\2", x[i])

  ## split the rest of the file up among these:
  j <- c(i[-1], length(x))
  tmp <- vector("list", length(i))
  f <- function(idx) {
    x[setdiff(i[[idx]]:j[[idx]], i)]
  }
  body <- lapply(seq_along(i), f)
  ret <- list(str=x[i], title=title, value=value, body=body)
  class(ret) <- "context_log"
  ret
}

##' @export
print.context_log <- function(x, ...) {
  prep <- function(x) {
    if (length(x) == 0) {
      ""
    } else {
      paste0("\n", paste0("    ", x, collapse="\n"))
    }
  }
  body <- vcapply(x$body, prep)
  cat(paste0(paste(paste0(x$str, body), collapse="\n"), "\n"))
}
