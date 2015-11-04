download_file <- function(url, dest, quiet=TRUE, mode="wb") {
  curl::curl_download(url, dest, quiet=quiet, mode=mode)
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
