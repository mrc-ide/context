context("other storage drivers")

test_that("sql", {
  skip_if_not_installed("RPostgres")
  ## Worse than this, we need to skip unless a postgres server is
  ## running!

  Sys.setenv(R_TESTS = "")
  context_log_start()
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, storage_type = storage_driver_psql)

  expect_equal(ctx$db$driver$type(), "DBI/PqConnection")
  expect_equal(context_root_get(path)$db$driver$type(), "DBI/PqConnection")

  expect_error(context_save(path, storage_type = "rds"),
               "Incompatible storage types: requested rds, stored: postgres")

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)
  expect_equal(task_status(t, ctx, TRUE), setNames(TASK_PENDING, t))

  e <- new.env()
  ctx_run <- context_load(ctx, e)
  expect_equal(task_run(t, ctx_run), eval(expr))
})
