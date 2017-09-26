cleanup <- function(root) {
  if (file.exists(path_config(root))) {
    db <- tryCatch(context_db_get(root), error = function(e) NULL)
    if (!is.null(db)) {
      db$driver$destroy()
    }
  }
  unlink(root, recursive = TRUE)
  ## This prunes libPaths down to the set of files that exist, so with
  ## the above should do a reasonable job of trimming any additions
  ## because the actual directories will have been deleted.
  .libPaths(.libPaths())
  context_cache$last_loaded_context <- NULL
}

missing_time <- function(n = 1) {
  Sys.time()[rep(NA, n)]
}

has_internet <- function() {
  if (is_windows()) {
    !inherits(try(suppressWarnings(
       readLines("http://google.com")), silent = TRUE), "try-error")
  } else {
    !is.null(suppressWarnings(utils::nsl("www.google.com")))
  }
}

skip_if_no_internet <- function() {
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
}

alter_package_version <- function(path, increase) {
  desc <- file.path(path, "DESCRIPTION")
  d <- read.dcf(desc)
  v <- alter_version(d[, "Version"], increase)
  d[, "Version"] <- v
  write.dcf(d, desc)
  invisible(numeric_version(v))
}

alter_version <- function(v, increase) {
  if (inherits(v, "numeric_version")) {
    as_version <- TRUE
  } else {
    v <- numeric_version(v)
    as_version <- FALSE
  }
  if (increase) {
    i <- length(unclass(v)[[1L]])
    v[[1L, i]] <- v[[1L, i]] + 1L
  } else {
    for (i in rev(seq_along(unclass(v)[[1L]]))) {
      if (v[[1L, i]] > 0L) {
        v[[1L, i]] <- v[[1L, i]] - 1L
        break
      }
    }
  }
  if (as_version) v else as.character(v)
}

read_package_version <- function(path) {
  numeric_version(read.dcf(file.path(path, "DESCRIPTION"), "Version")[[1]])
}
