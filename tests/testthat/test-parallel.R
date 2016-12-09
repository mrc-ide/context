context("parallel")

test_that("parallel cluster", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns-parallel.R")

  context_log_start()
  cl <- start_parallel_cluster(2L, ctx)

  source("myfuns-parallel.R", local = TRUE)
  res <- get_cluster_pids()
  expect_equal(length(res$workers), 2)

  ## The context package is loaded
  res <- parallel::clusterCall(NULL, "loadedNamespaces")
  expect_true(all(vlapply(res, function(x) "context" %in% x)))

  ## But not attached
  res <- parallel::clusterCall(NULL, "search")
  expect_false(any(vlapply(res, function(x) "packge:context" %in% x)))

  expect_equal(cl, parallel_cluster())

  expect_error(start_parallel_cluster(2L, ctx),
               "Parallel cluster already running")
  expect_equal(cl, parallel_cluster())

  expect_true(stop_parallel_cluster())

  expect_error(parallel_cluster(), "Cluster has not been started yet")
  expect_false(stop_parallel_cluster())

  expect_error(parallel::clusterCall(NULL, "loadedNamespaces"),
               "registered")
})
