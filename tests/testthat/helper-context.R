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
  !is.null(suppressWarnings(utils::nsl("www.google.com")))
}

skip_if_no_internet <- function() {
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
}
