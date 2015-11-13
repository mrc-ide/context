download_cache <- new.env(parent=emptyenv())
download_file <- function(url, dest, quiet=TRUE, mode="wb", cache=TRUE) {
  if (!cache) {
    curl::curl_download(url, dest, quiet=quiet, mode=mode)
  } else {
    if (is.null(download_cache[[url]])) {
      tmp <- tempfile("context_download_")
      curl::curl_download(url, tmp, quiet=quiet, mode=mode)
      ## Only write to the cache if download successful:
      download_cache[[url]] <- tmp
    } else if (!quiet) {
      message("using cached copy of ", url)
    }
    file.copy(download_cache[[url]], dest)
  }
  dest
}

filter_warnings <- function(expr, pattern) {
  match_any <- function(x, pattern) {
    any(vlapply(pattern, grepl, x))
  }
  w <- function(w) {
    if (match_any(w$message, pattern)) {
      invokeRestart("muffleWarning")
    }
  }
  withCallingHandlers(expr, warning = w)
}
