context("compatibility")

## To generate the archive, let's do:

## v <- "0.x.x"
## base <- paste0("compat_", v)
## ctx <- context::context_save(base)
## t <- context::task_save(quote(sin(1)), ctx)
## context::task_run(t, context::context_load(ctx))
## obj <- queuer:::queue_base(ctx)
## invisible(obj$lapply(1:10, sin))
## zip(paste0(base, ".zip"), base)

test_that("old", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  dir.create(path)

  unzip("compat_0.1.0.zip", exdir = path)
  path_root <- file.path(path, "compat_0.1.0")
  expect_message(root <- context_root_init(path_root, NULL, NULL),
                 "HEY THERE")
  expect_equal(context_list(root),
               c("1f2ca4595dc036ce74d4869b5f54592f",
                 "22e190de445a3f18a283c400a3452dd6"))
})
