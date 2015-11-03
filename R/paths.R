## Helpers for paths
path_library <- function(root) {
  file.path(root, "R", R.version[["platform"]], r_version(2))
}
path_tasks <- function(root, id=NULL) {
  file_path(root, "tasks", id)
}
path_contexts <- function(root, id=NULL) {
  file_path(root, "contexts", id)
}
path_environments <- function(root, id=NULL) {
  file_path(root, "environments", id)
}
path_results <- function(root, id=NULL) {
  file_path(root, "results", id)
}
path_drat <- function(root) {
  file.path(root, "drat")
}
