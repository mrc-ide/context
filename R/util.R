## Base imports:
##' @importFrom stats na.omit setNames
##'
##' @importFrom utils capture.output modifyList packageVersion sessionInfo
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
platform <- function() {
  R.version[["platform"]]
}
r_version <- function(n) {
  if (n < 1L || n > 3L) {
    stop("Invalid n")
  }
  getRversion()[1, seq_len(n)]
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
  save(list = setdiff(names(.GlobalEnv), exclude), envir = .GlobalEnv,
       file = tmp)
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

find_functions <- function(fun, env) {
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

fun_to_str <- function(x, env) {
  paste0(x, " <- ",
         paste(deparse(get(x, env, inherits=FALSE)), collapse="\n"))
}

Rscript <- function(...) {
  Sys.setenv("R_TESTS" = "")
  system2(file.path(R.home(), "bin", "Rscript"), ...)
}
