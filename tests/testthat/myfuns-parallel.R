get_cluster_pids <- function() {
  workers <- vapply(parallel::clusterCall(NULL, "Sys.getpid"),
                    identity, numeric(1))
  list(host = Sys.getpid(), workers = workers)
}

manual_parallel_test <- function() {
  cl <- parallel::makeCluster(1, "PSOCK")
  on.exit(parallel::stopCluster(cl))
  parallel::clusterCall(cl, ".libPaths")
}
