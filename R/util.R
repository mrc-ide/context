## Base imports:
##' @importFrom stats na.omit setNames
##'
##' @importFrom utils capture.output modifyList packageVersion sessionInfo
##'   limitedLabels
NULL

## Typed sapply
vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

## Determine if an object is the global environment
is.GlobalEnv <- function(x) {
  identical(x, .GlobalEnv)
}

print_ad_hoc <- function(x) {
  cl <- class(x)[[1]]
  xp <- unclass(x)
  i <- vlapply(xp, is.raw)
  if (any(i)) {
    xp[i] <- sprintf("raw <%d bytes>", lengths(xp[i]))
  }
  i <- vlapply(xp, is.atomic) & lengths(xp) > 1L
  if (any(i)) {
    xp[i] <- vcapply(xp[i], function(el)
      paste(sprintf("\n   - %s", el), collapse = ""))
  }
  members <- paste(sprintf(" - %s: %s\n", names(xp), unname(xp)), collapse = "")
  cat(sprintf("<%s>\n%s", cl, members))
  invisible(x)
}

Sys_time <- function() {
  op <- options(digits.secs = 3)
  on.exit(options(op))
  as.character(Sys.time())
}

is_directory <- function(x) {
  v <- file.info(x)[["isdir"]]
  v & !is.na(v)
}

string_starts_with <- function(x, y) {
  substr(x, 1, nchar(y)) == y
}

hostname <- function() {
  Sys.info()[["nodename"]]
}
process_id <- function() {
  Sys.getpid()
}

r_platform_name <- function(platform = NULL) {
  if (is.null(platform)) {
    ## TODO: this needs serious work for mavericks; as is, this will
    ## work OK for windows/linux (which is all I need right now).
    ## Currently this yields "darwin" for all osx.  There's not a lot
    ## of guidance about how we detect if the system is mavericks or
    ## not (.Platform$pkgType says it should *not* be used to identify
    ## platform).
    tolower(Sys.info()[["sysname"]])
  } else {
    match_value(platform, valid_platforms())
  }
}

r_version <- function(n) {
  if (n < 1L || n > 3L) {
    stop("Invalid n")
  }
  getRversion()[1, seq_len(n)]
}

capture_log <- function(expr, filename) {
  if (!is.null(filename)) {
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    con <- file(filename, open = "w")
    sink(con, type = "message") # Dangerous!
    sink(con, type = "output")
    on.exit({
      sink(NULL, type = "message")
      sink(NULL, type = "output")
      close(con)
    })
  }
  eval(expr, parent.frame())
}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

call_trace <- function(skip_outer = 0, skip_inner = 0) {
  calls <- sys.calls()
  limitedLabels(trim_calls(calls, skip_outer, skip_inner))
}

trim_calls <- function(calls, skip_outer = 0, skip_inner = 0) {
  if (skip_outer > length(calls)) {
    return(list())
  } else if (skip_outer > 0L) {
    calls <- calls[-seq_len(skip_outer)]
  }

  if (skip_inner > length(calls)) {
    return(list())
  } else if (skip_inner > 0L) {
    calls <- calls[-seq(by = 1, length.out = skip_inner, to = length(calls))]
  }

  calls
}

collector <- function(init = list()) {
  res <- init
  list(add = function(x) res <<- c(res, list(x)),
       get = function() res)
}

## R time objects really want me poke my eyes out.  Perhaps there is a
## better way of doing this?  Who knows?
unlist_times <- function(x) {
  if (length(x) == 0L) {
    empty_time()
  } else {
    tmp <- vnapply(x, identity)
    attributes(tmp) <- attributes(x[[1L]])
    tmp
  }
}

empty_time <- function() {
  Sys.time()[-1]
}

valid_platforms <- function() {
  c("windows", "macosx", "macosx/mavericks", "linux")
}

## Not necessarily the fastest, but it should do.
df_to_list <- function(x, use_names) {
  keep <- c("names", "class", "row.names")
  at <- attributes(x)
  attributes(x) <- at[intersect(names(at), keep)]

  i <- vapply(x, is.list, logical(1))
  prepare <- function(el) {
    el <- as.list(el)
    el[i] <- lapply(el[i], unlist, FALSE)
    el
  }
  ret <- unname(lapply(split(x, seq_len(nrow(x))), prepare))
  if (!use_names) {
    ret <- lapply(ret, unname)
  }
  if (is.character(at$row.names)) {
    names(ret) <- at$row.names
  }
  ret
}

is_call <- function(expr, what) {
  is.call(expr) && any(vlapply(what, identical, expr[[1L]]))
}

eval_safely <- function(expr, envir, class, depth = 0L) {
  warnings <- collector()
  error <- NULL

  handler <- function(e) {
    w <- warnings$get()
    if (length(w) > 0L) {
      e$warnings <- warnings$get()
    }
    e$trace <- call_trace(0, depth + 1L)
    class(e) <- c(class, class(e))
    error <<- e
    NULL
  }

  value <- tryCatch(
    withCallingHandlers(
      eval(expr, envir),
      warning = function(e) warnings$add(e),
      error = function(e) handler(e)),
    error = function(e) error)

  list(value = value,
       success = is.null(error))
}


deparse_fn <- function(nm, ...) {
  value <- trimws(deparse(get(nm, ...)), "right")
  value[[1]] <- sprintf("%s <- %s", nm, value[[1]])
  value
}


write_script_exec <- function(code, path) {
  writeLines(code, path)
  Sys.chmod(path, "755")
}
