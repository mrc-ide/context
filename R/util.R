## Base imports:
##' @importFrom stats na.omit setNames
##'
##' @importFrom utils capture.output modifyList packageVersion sessionInfo
NULL

## Typed sapply
vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
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

file_remove <- function(...) {
  files <- c(...)
  ok <- file.exists(files)
  if (any(ok)) {
    file.remove(files[ok])
  }
  ok
}

print_ad_hoc <- function(x) {
  cl <- class(x)[[1]]
  x <- unclass(x)
  i <- vlapply(unclass(x), is.raw)
  if (any(i)) {
    x[i] <- sprintf("raw <%d bytes>", lengths(x[i]))
  }
  i <- vlapply(x, is.atomic) & lengths(x) > 1L
  if (any(i)) {
    x[i] <- vcapply(x[i], function(el)
      paste(sprintf("\n   - %s", el), collapse=""))
  }
  members <- paste(sprintf(" - %s: %s\n", names(x), unname(x)), collapse="")
  cat(sprintf("<%s>\n%s", cl, members))
  invisible(x)
}

Sys_time <- function() {
  op <- options(digits.secs=3)
  on.exit(options(op))
  as.character(Sys.time())
}

is_directory <- function(x) {
  file.info(x)[["isdir"]]
}

## Convert a path to a file:// that R can understand.  Some care is
## needed on windows.  This will create a path with *three* leading
## slashes.
file_url <- function(path) {
  full_path <- normalizePath(path, winslash="/")
  paste0("file://", if (substr(full_path, 1, 1) == "/") "" else "/", full_path)
}

file_unurl <- function(url) {
  if (is_windows()) {
    sub("^file:///", "", url)
  } else {
    sub("^file://", "", url)
  }
}

find_funcs <- function(fun, env) {
  ours <- names(env)
  ours <- ours[vlapply(ours, function(x) is.function(env[[x]]))]
  seen <- character(0)
  test <- list(fun)
  while (length(test) > 0L) {
    new <- setdiff(intersect(codetools::findGlobals(test[[1]]), ours), seen)
    seen <- c(seen, new)
    test <- c(test[-1], lapply(new, get, env, inherits=FALSE))
  }
  sort(seen)
}

string_starts_with <- function(x, y) {
  substr(x, 1, nchar(y)) == y
}

file_exists_under_wd <- function(filename) {
  ok <- file.exists(filename)
  nok <- !ok
  if (any(ok)) {
    ok[ok] <- string_starts_with(normalizePath(filename[ok]),
                                 normalizePath(getwd()))
  }
  ok[nok] <- NA
  ok
}

hostname <- function() {
  Sys.info()[["nodename"]]
}
process_id <- function() {
  Sys.getpid()
}

is_error <- function(x) {
  inherits(x, "error")
}

## Like save.image but:
##
##   - save into a raw vector
##   - exclude .Random.seed
##
## It does involve a potentially unnecessary disk round trip, but
## based on wch's benchmarks that's probably the fastest thing anyway.
serialise_image <- function() {
  exclude <- ".Random.seed"
  tmp <- tempfile()
  on.exit(file_remove(tmp))
  save(list=setdiff(names(.GlobalEnv), exclude), envir=.GlobalEnv,
       file=tmp)
  read_binary(tmp)
}

deserialise_image <- function(bin, ...) {
  tmp <- tempfile()
  on.exit(file_remove(tmp))
  writeBin(bin, tmp)
  load(tmp, ...)
}

read_binary <- function(filename) {
  readBin(filename, raw(), file.size(filename))
}

capture_log <- function(expr, filename) {
  if (!is.null(filename)) {
    dir.create(dirname(filename), showWarnings=FALSE, recursive=TRUE)
    con <- file(filename, open="w")
    sink(con, type="message") # Dangerous!
    sink(con, type="output")
    on.exit({
      sink(NULL, type="message")
      sink(NULL, type="output")
      close(con)
    })
  }
  eval(expr, parent.frame())
}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

call_trace <- function(skip_outer=0, skip_inner=0) {
  calls <- sys.calls()

  if (skip_outer > length(calls)) {
    return(character(0))
  } else if (skip_outer > 0L) {
    calls <- calls[-seq_len(skip_outer)]
  }

  if (skip_inner > length(calls)) {
    return(character(0))
  } else if (skip_inner > 0L) {
    calls <- calls[-seq(by=1, length.out=skip_inner, to=length(calls))]
  }

  limitedLabels(calls)
}

collector <- function(init=list()) {
  res <- init
  list(add=function(x) res <<- c(res, list(x)),
       get=function() res)
}

is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}
