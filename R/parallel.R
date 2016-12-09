par <- new.env(parent = emptyenv())

##' Start a sub-cluster, using the \code{parallel} package.  This will
##' be available via either the return value of this function, the
##' \code{parallel_cluster} function or by using \code{cl = NULL} with
##' any of the \code{parallel} package functions.  The cluster will be
##' started so that it is ready to use the context.
##'
##' @title Start a sub-cluster
##'
##' @param n The number of nodes.  No attempt at guessing this number
##'   is made as that is terribly error prone.  If you're using this
##'   function you should know how many resources you have available.
##'
##' @param ctx The context to initialise on each cluster node.
##'
##' @export
start_parallel_cluster <- function(n, ctx) {
  if (is.null(par$cl)) {
    context_log("cluster", "Starting cluster")
    ## Log to <base>/workers/<context_id>_<pid>_%d I think
    path <- context_root_get(ctx)$path
    fmt <- sprintf("%s/workers/%s_%d_%%d", path, ctx$id, Sys.getpid())
    dir.create(dirname(fmt), FALSE, TRUE)
    par$cl <- start_cluster(n, fmt)
    ## Then the question becomes -- how to we most simply point these
    ## at the current R installation so that everything works well?
    ## Ideally these will load the context so that they're good to go.
    context_start <- function(root, id) {
      context_log_start()
      context_load(context_read(id, root))
      invisible()
    }
    invisible(parallel::clusterCall(par$cl, ".libPaths", .libPaths()))
    invisible(parallel::clusterCall(par$cl, context_start, ctx$root, ctx$id))
    parallel::setDefaultCluster(par$cl)
    context_log("cluster", "Cluster started")
  } else {
    stop("Parallel cluster already running?")
  }
  invisible(par$cl)
}

##' @rdname start_parallel_cluster
##' @export
stop_parallel_cluster <- function() {
  registered <- !is.null(par$cl)
  if (registered) {
    context_log("cluster", sprintf("Stopping %d nodes", length(par$cl)))
    parallel::stopCluster(par$cl)
    parallel::setDefaultCluster(NULL)
    par$cl <- NULL
  }
  registered
}

##' @rdname start_parallel_cluster
##' @export
parallel_cluster <- function() {
  par$cl %||% stop("Cluster has not been started yet")
}

start_cluster <- function(n, logfile_fmt) {
  ## NOTE: None of the context_log() lines here will trigger logging
  ## on the cluster because the cluster jas not been assigned into the
  ## `par` environment yet.
  logfiles <- sprintf(logfile_fmt, seq_len(n))
  cl <- vector("list", n)
  context_log("cluster", sprintf("Starting %d nodes", n))
  for (i in seq_len(n)) {
    context_log("worker", logfiles[[i]])
    node <- parallel::makeCluster(1L, "PSOCK", outfile = logfiles[[i]])
    cl[[i]] <- node[[1L]]
  }
  class(cl) <- c("SOCKcluster", "cluster")
  pid <- as.integer(parallel::clusterCall(cl, Sys.getpid))
  context_log("cluster", sprintf("pids: %s", paste(pid, collapse = ", ")))
  attr(cl, "pid") <- pid
  cl
}
