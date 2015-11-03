## OK, so we want a new function to download a file and abstract over
## loading different packages to make it all work.  But I'm also cool
## with just using curl and falling back on download.file perhaps?
## Who knows.  But the dependency needs to be sorted out at some point
## so that we can *rely* on these functions on new computers.
##
## The progress bar that downloader::download creates is much nicer.
## It's also very hard to tune in/out.  But for big files it's
## probably really required.  Argh.  There are no good choices, only
## less bad ones.
##
## It's possible (probable) that a failed download will overwrite the
## destination file; this will download elsewhere and move into place.
##
## More trouble; as documented in a recent issue, this really requires
## that downloader is installed.  However, that might no longer really
## be required if libcurl support is enabled.  I'm going to guess that
## is enabled on all offical distributions (so, OS/X, Windows, perhaps
## Debian/Ubuntu installations, too).  So in that case we can ignore
## downloader *unless* capabilities("libcurl") is FALSE _and_ the URL
## is an https:// url.
download_file <- function(url, dest, quiet, ..., mode="wb") {
  tmp <- tempfile("download_")
  status <- filter_warnings(downloader::download(url, tmp, ...,
                                                 mode=mode, quiet=quiet),
                            "downloaded length.*reported length")
  if (status != 0) {
    stop("Download failed with code ", status)
  }
  ok <- file.rename(tmp, dest)
  if (!ok) {
    ## Linux
    ok <- file.copy(tmp, dest)
    file.remove(tmp)
  }
  dest
}

## downloaded length 5875 != reported length 0
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
