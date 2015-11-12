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
path_task_status <- function(root, id=NULL) {
  file_path(root, "task_status", id)
}
path_task_results <- function(root, id=NULL) {
  file_path(root, "task_results", id)
}
path_drat <- function(root) {
  file.path(root, "drat")
}
path_bootstrap <- function(root) {
  file.path(root, "context_bootstrap")
}
path_runner <- function(root) {
  file.path(root, "context_runner")
}
path_version <- function(root) {
  file.path(root, "context_version")
}
