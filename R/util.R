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
  save(list=setdiff(names(.GlobalEnv), exclude),
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

rename_to_md5 <- function(filename, path, ext="") {
  md5 <- tools::md5sum(filename)
  dest <- file.path(path, paste0(md5, ext))
  dir.create(dirname(dest), FALSE, TRUE)
  if (file.exists(dest)) {
    file.remove(filename)
  } else {
    file.rename(filename, dest)
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
  paste(sprintf(" - %s: %s\n", names(x), unname(x)), collapse="")

  cat(sprintf("<%s>\n - id: %s\n - root: %s\n", class(x)[[1]], x$id, x$root))
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
