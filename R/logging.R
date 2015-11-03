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
    message(trimws(sprintf("[ %-8s ]  %s", topic, value)))
  }
}
