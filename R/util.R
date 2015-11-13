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

## Functions for saving images and objects into files that are named
## after their md5 hashes, giving some degree of content addressable
## storage.  This will change if I can generalise the interface a
## little.
save_image <- function(path) {
  exclude <- ".Random.seed"
  tmp <- tempfile()
  save(list=setdiff(names(.GlobalEnv), exclude), envir=.GlobalEnv,
       file=tmp)
  rename_to_md5(tmp, path)
}

save_object <- function(envir, path) {
  if (is.GlobalEnv(envir)) {
    NULL
  } else {
    tmp <- tempfile()
    saveRDS(envir, tmp)
    rename_to_md5(tmp, path)
  }
}

## file.remame: "This is subject to the limitations of the OS's
## corresponding system call (see something like ‘man 2 rename’ on a
## Unix-alike): in particular in the interpretation of ‘file’: most
## platforms will not rename files across file systems."
##
## similar issues affect python's os.rename rather than shutil.move
##
## Note that this is no longer atomic, and may do slightly weird
## things to directories; need to check that this is all reasonable
## with some tests.  However, for the main use here (rename_to_md5)
## this is guaranteed to be a file (and actually won't copy if dest
## exists).
file_rename <- function(from, to) {
  file.copy(from, to, overwrite=TRUE)
  unlink(from, recursive=TRUE)
}

rename_to_md5 <- function(filename, path, ext="") {
  md5 <- tools::md5sum(filename)
  dest <- file.path(path, paste0(md5, ext))
  dir.create(dirname(dest), FALSE, TRUE)
  if (file.exists(dest)) {
    file.remove(filename)
  } else {
    file_rename(filename, dest)
  }
  unname(md5)
}

## This should be swapped out for the uuid function, but let's see how
## that goes
random_id <- function() {
  pos <- c(as.character(0:9), letters[1:6])
  paste(sample(pos, 32, replace=TRUE), collapse="")
}

print_ad_hoc <- function(x) {
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
