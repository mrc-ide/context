## I probably should use loggr here, but I can swap this out easily
## enough later.
context_log_start <- function() {
  options(context.log=TRUE)
}
context_log_stop <- function() {
  options(context.log=NULL)
}
context_log <- function(topic, value) {
  if (isTRUE(getOption("context.log"))) {
    message(trimws(sprintf("[ %-7s ]  %s", topic, value)))
  }
}
