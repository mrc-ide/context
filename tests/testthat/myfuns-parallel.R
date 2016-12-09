get_cluster_pids <- function() {
  workers <- vapply(parallel::clusterCall(NULL, "Sys.getpid"),
                    identity, numeric(1))
  list(host = Sys.getpid(), workers = workers)
}
