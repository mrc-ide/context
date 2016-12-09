context("script")

test_that("basic", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path)
  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  full <- file.path(path_bin(path), "task_run")
  res <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status", exact = TRUE))

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  expect_equal(task_result(t, ctx), eval(expr))
})

test_that("error", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns.R")
  t1 <- task_save(quote(g(-1)), ctx)
  t2 <- task_save(quote(g(-1)), ctx)

  full <- file.path(path_bin(path), "task_run")

  res <- Rscript(c(full, path, t1), stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status", exact = TRUE))
  expect_equal(task_status(t1, ctx), TASK_ERROR)

  Sys.setenv("CONTEXT_PROPAGATE_ERROR" = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_PROPAGATE_ERROR"), add = TRUE)
  res <- suppressWarnings(
    Rscript(c(full, path, t2), stdout = TRUE, stderr = TRUE))
  expect_gt(attr(res, "status", exact = TRUE), 0)
  expect_equal(task_status(t2, ctx), TASK_ERROR)

  r1 <- task_result(t1, ctx)
  r2 <- task_result(t2, ctx)
  expect_equal(r1, r2)
  expect_is(r1, "context_task_error")
  expect_equal(r1$trace[[1]], "context:::main_task_run()")
})

test_that("parallel", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns-parallel.R")
  t <- task_save(quote(get_cluster_pids()), ctx)

  full <- file.path(path_bin(path), "task_run")
  log1 <- suppressWarnings(
    Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE))
  expect_equal(task_status(t, ctx), TASK_ERROR)

  res1 <- task_result(t, ctx)
  expect_is(res1, "context_task_error")
  expect_match(res1$message, "registered")
  expect_true(any(grepl("defaultCluster", res1$trace)))

  Sys.setenv("CONTEXT_CORES" = 2L)
  on.exit(Sys.unsetenv("CONTEXT_CORES"), add = TRUE)
  log2 <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  res2 <- task_result(t, ctx)
  expect_true(process_id() != res2$host)
  expect_equal(length(res2$workers), 2)
  expect_false(res2$host %in% res2$workers)
})
