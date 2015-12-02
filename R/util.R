## Like file.path, but NULL elements are skipped over rather than
## rendering the string non-existant.
file_path <- function(...) {
  paths <- list(...)
  paths <- paths[!vlapply(paths, is.null)]
  do.call("file.path", paths, quote=TRUE)
}

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

## R version to n significant digits
r_version <- function(n) {
  if (n < 0L || n > 3L) {
    stop("Invalid n")
  }
  getRversion()[1, seq_len(n)]
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

## This should be swapped out for the uuid function, but let's see how
## that goes
random_id <- function() {
  pos <- c(as.character(0:9), letters[1:6])
  paste(sample(pos, 32, replace=TRUE), collapse="")
}

print_ad_hoc <- function(x) {
  i <- vlapply(unclass(x), is.raw)
  if (any(i)) {
    x[i] <- sprintf("raw <%d bytes>", lengths(x[i]))
  }
  members <- paste(sprintf(" - %s: %s\n", names(x), unname(x)), collapse="")
  cat(sprintf("<%s>\n%s", class(x)[[1]], members))
  invisible(x)
}

Sys_time <- function() {
  op <- options(digits.secs=3)
  on.exit(options(op))
  as.character(Sys.time())
}

is_dir <- function(x) {
  file.info(x)[["isdir"]]
}

## Convert a path to a file:// that R can understand.  Some care is
## needed on windows.  This will create a path with *three* leading
## slashes.
file_url <- function(path) {
  full_path <- normalizePath(path, winslash="/")
  paste0("file://", if (substr(full_path, 1, 1) == "/") "" else "/", full_path)
}

find_funcs <- function(fun, env) {
  ours <- names(env)
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

clean_path <- function(x) {
  sub("/+$", "", gsub("\\", "/", x, fixed=TRUE))
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

## This does not handle the case of a file /a/b/c and working
## directory of the same.
relative_paths <- function(filename, dir=getwd()) {
  msg <- !file.exists(filename)
  if (any(msg)) {
    stop("files do not exist: ", paste(filename[msg], collapse=", "))
  }

  filename_abs <- clean_path(normalizePath(filename))
  dir <- clean_path(normalizePath(dir))

  ok <- string_starts_with(filename_abs, paste0(dir, "/"))
  if (!all(ok)) {
    stop("files above working directory: ",
         paste(filename[!ok], collapse=", "))
  }

  substr(filename_abs, nchar(dir) + 2L, nchar(filename_abs))
}

hostname <- function() {
  Sys.info()[["nodename"]]
}
process_id <- function() {
  Sys.getpid()
}

is_error <- function(x) {
  inherits(x, "try-error")
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

write_script <- function(text, dest) {
  dir.create(dirname(dest), FALSE, TRUE)
  writeLines(text, dest)
  Sys.chmod(dest, "0755")
}

invert_names <- function(x) {
  setNames(names(x), x)
}
