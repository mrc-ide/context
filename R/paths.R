## Helpers for paths
path_library <- function(root) {
  file.path(root, "R", R.version[["platform"]], r_version(3))
}
path_drat <- function(root) {
  file.path(root, "drat")
}
path_version <- function(root) {
  file.path(root, "context_version")
}
path_bin <- function(root, script=NULL) {
  file_path(root, "bin", script)
}
path_db <- function(root) {
  file.path(root, "db")
}
path_config <- function(root) {
  file.path(root, "config")
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
