## Helpers for paths
path_library <- function(root) {
  file.path(root, "R", R.version[["platform"]], r_version(2))
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
