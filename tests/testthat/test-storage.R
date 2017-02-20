context("other storage drivers")

test_that("sql", {
  skip_if_not_installed("RPostgres")

  fn <- function(tbl_data, tbl_keys) {
    host <- Sys.getenv("CONTEXT_PGHOST", "localhost")
    con <- DBI::dbConnect(RPostgres::Postgres(), host = host)
    storr::storr_dbi(con = con, tbl_data = tbl_data, tbl_keys = tbl_keys,
                     binary = FALSE, hash_algorithm = "sha1")
  }
  prefix <- paste(sample(letters, 8), collapse = "")
  args <- list(tbl_data = sprintf("context_%s_data", prefix),
               tbl_keys = sprintf("context_%s_keys", prefix))
  config <- list(type = fn, args = args)

  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, storage_type = fn, storage_args = args)
  expect_equal(ctx$db$driver$type(), "DBI/PqConnection")
  expect_equal(context_root_get(path)$db$driver$type(), "DBI/PqConnection")
  rm(prefix, args, config, fn)
  expect_equal(context_root_get(path)$db$driver$type(), "DBI/PqConnection")

  expect_error(context_save(path, storage_type = "rds"),
               "Incompatible storage types: requested rds, stored: user")

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)
  expect_equal(task_status(t, ctx, TRUE), setNames(TASK_PENDING, t))

  e <- new.env()
  ctx_run <- context_load(ctx, e)
  expect_equal(task_run(t, ctx_run), eval(expr))
})
