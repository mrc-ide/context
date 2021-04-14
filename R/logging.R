##' Send an entry to the context log.  This is designed primarily for
##' use with packages that build off of context, so that they can log
##' in a consistent way.
##' @title Send entry to context log
##' @param topic Up to 9 character text string with the log topic
##' @param value Character string with the log entry
##' @export
context_log <- function(topic, value) {
  n <- length(value) - 1L
  if (n > 0L) {
    topic <- c(topic, rep_len("...", n))
  }
  str <- trimws(sprintf("[ %-9s ]  %s", topic, value))
  if (n > 0L) {
    str <- paste(str, collapse = "\n")
  }
  message(str)
  if (!is.null(context_cache$cl)) {
    ## Logging is rare enough that we should communicate with
    ## workers; the cost is relatively low.  It might be worth
    ## making this an option though.
    ##
    ## NOTE: if one of the nodes here is busy (though not sure how
    ## that can happen) then this will hang or cause things to error
    ## out.  OTOH, given that the underlying computations will use a
    ## cluster, we would simply hang there instead...
    parallel::clusterCall(context_cache$cl, "message", str)
  }
}

parse_context_log <- function(x) {
  re <- "^\\[ (.{9}) \\](.*)$"
  i <- grep(re, x)

  if (length(i) == 0L && length(x) > 0L) {
    str <- title <- "<top level error>"
    value <- ""
    body <- list(x)
  } else {
    str <- x[i]
    title <- trimws(sub(re, "\\1", x[i]))
    value <- sub(re, "\\2", x[i])

    ## split the rest of the file up among these:
    j <- c(i[-1], length(x))
    f <- function(idx) {
      x[setdiff(i[[idx]]:j[[idx]], i)]
    }
    body <- lapply(seq_along(i), f)
  }
  ret <- list(str = str, title = title, value = value, body = body)
  class(ret) <- "context_log"
  ret
}

##' @export
print.context_log <- function(x, pretty = TRUE, ...) {
  prep <- function(x) {
    if (length(x) == 0) {
      ""
    } else {
      paste0("\n", paste0("    ", x, collapse = "\n"))
    }
  }
  if (pretty && crayon::has_color()) {
    x <- pretty_context_log(x)
  }
  body <- vcapply(x$body, prep)
  cat(paste0(paste(paste0(x$str, body), collapse = "\n"), "\n"))
}

pretty_context_log <- function(x) {
  yellow <- crayon::make_style("yellow")$bold
  green <- crayon::make_style("blue")$bold
  x$str <- green(x$str)
  i <- vapply(x$body, length, integer(1)) > 0L
  x$body[i] <- lapply(x$body[i], yellow)
  x
}
