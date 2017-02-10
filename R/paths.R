## Helpers for paths
path_version <- function(path_root) {
  file.path(path_root, "context_version")
}
path_db <- function(path_root) {
  file.path(path_root, "db")
}
path_config <- function(path_root) {
  file.path(path_root, "config")
}
path_bin <- function(path_root) {
  file.path(path_root, "bin")
}

## TODO: together with r_platform_name, this needs to deal with the
## case of mac binaries (macosx/mavericks is not a good name here).
path_library <- function(path_root, platform = NULL, version = NULL) {
  if (is.null(version)) {
    version_str <- as.character(r_version(2))
  } else {
    if (!inherits(version, "numeric_version")) {
      version <- numeric_version(version)
    }
    version_str <- as.character(version[1, 1:2])
  }
  platform_str <- r_platform_name(platform)
  file.path(path_root, "lib", platform_str, version_str)
}
path_drat <- function(path_root) {
  file.path(normalizePath(path_root, mustWork = TRUE), "drat")
}

## TODO: belongs in pathr, also used in remotefile / filestorr
##' Test if path is absolute or relative.
##' @title Test if path is absolute or relative
##' @param path Vector of paths to test
##' @export
is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\]|//|\\\\\\\\)", path)
}
##' @export
##' @rdname is_absolute_path
is_relative_path <- function(path) {
  !is_absolute_path(path)
}

## This does not handle the case of a file /a/b/c and working
## directory of the same.
relative_paths <- function(filename, dir=getwd()) {
  msg <- !file.exists(filename)
  if (any(msg)) {
    stop("files do not exist: ", paste(filename[msg], collapse = ", "))
  }

  filename_abs <- clean_path(normalizePath(filename))
  dir <- clean_path(normalizePath(dir))

  ok <- string_starts_with(filename_abs, paste0(dir, "/"))
  if (!all(ok)) {
    stop("files above working directory: ",
         paste(filename[!ok], collapse = ", "))
  }

  substr(filename_abs, nchar(dir) + 2L, nchar(filename_abs))
}

clean_path <- function(x) {
  sub("/+$", "", gsub("\\", "/", x, fixed = TRUE))
}
