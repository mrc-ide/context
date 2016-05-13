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
##'
##' @return \code{context_log_start} invisibly returns a logical
##'   indicating if logging was previously enabled.  This allows
##'   patterns like:
##' \preformatted{if (!context::context_log_start()) {
##'   context::context_log_stop()
##' }
##' }
##' to have a scoped log (i.e., log for the duration of a function).
context_log_start <- function() {
  invisible(isTRUE(options(context.log=TRUE)$context.log))
}
##' @export
##' @rdname context_log
context_log_stop <- function() {
  options(context.log=NULL)
}

context_log <- function(topic, value) {
  if (isTRUE(getOption("context.log"))) {
    n <- length(value) - 1L
    if (n > 0L) {
      topic <- c(topic, rep_len("...", n))
    }
    str <- trimws(sprintf("[ %-9s ]  %s", topic, value))
    if (n > 0L) {
      str <- paste(str, collapse="\n")
    }
    message(str)
    if (!is.null(par$cl)) {
      ## Logging is rare enough that we should communicate with
      ## workers; the cost is relatively low.  It might be worth
      ## making this an option though.
      ##
      ## NOTE: if one of the nodes here is busy (though not sure how
      ## that can happen) then this will hang or cause things to error
      ## out.  OTOH, given that the underlying computations will use a
      ## cluster, we would simply hang there instead...
      parallel::clusterCall(par$cl, "message", str)
    }
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
print.context_log <- function(x, pretty=TRUE, ...) {
  prep <- function(x) {
    if (length(x) == 0) {
      ""
    } else {
      paste0("\n", paste0("    ", x, collapse="\n"))
    }
  }
  if (pretty && crayon::has_color()) {
    x <- pretty_context_log(x)
  }
  body <- vcapply(x$body, prep)
  cat(paste0(paste(paste0(x$str, body), collapse="\n"), "\n"))
}

pretty_context_log <- function(x) {
  yellow <- crayon::make_style("yellow")$bold
  green <- crayon::make_style("blue")$bold
  x$str <- green(x$str)
  i <- vapply(x$body, length, integer(1)) > 0L
  x$body[i] <- lapply(x$body[i], yellow)
  x
}
