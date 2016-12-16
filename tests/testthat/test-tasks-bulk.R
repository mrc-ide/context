context("tasks - bulk")

## This is the simplest case; where we only have a single element to
## add.
test_that("simple", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  template <- quote(sin(NULL))
  ids <- task_save_bulk(template, 1:5, 1L, ctx)
  expect_false(any(duplicated(ids)))

  expect_equal(task_list(ctx), sort(ids))

  expect_equal(lapply(ids, task_run, ctx), as.list(sin(1:5)))
  expect_equal(lapply(ids, task_result, ctx), as.list(sin(1:5)))
})

test_that("bulk, multiple arguments", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  X <- expand.grid(x = as.numeric(1:3), rate = as.numeric(1:3),
                   KEEP.OUT.ATTRS = FALSE)
  XX <- unname(lapply(split(X, seq_len(nrow(X))), as.list))

  template <- quote(dgamma(NULL, NULL, shape = 2))
  ids <- task_save_bulk(template, XX, 1:2, ctx)
  expect_equal(task_expr(ids[[1]], ctx),
               quote(dgamma(1, 1, shape = 2)))
  expect_equal(task_expr(ids[[5]], ctx),
               bquote(dgamma(.(XX[[5]][[1]]), .(XX[[5]][[2]]), shape = 2)))

  template2 <- quote(dgamma(NULL, shape = 2))
  expect_error(task_save_bulk(template2, XX, 1, ctx),
               "All 'x' must be length 1")
  XX2 <- unname(lapply(split(X, seq_len(nrow(X))), function(x)
                       list(unlist(x, use.names = FALSE))))
  ids2 <- task_save_bulk(template2, XX2, 1, ctx)

  expect_equal(task_expr(ids2[[1]], ctx),
               quote(dgamma(c(1, 1), shape = 2)))
  expect_equal(task_expr(ids2[[5]], ctx),
               bquote(dgamma(c(.(XX[[5]][[1]]), .(XX[[5]][[2]])), shape = 2)))
})

test_that("empty", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  template <- quote(sin(NULL))
  expect_equal(task_save_bulk(template, list(), 1, ctx), character(0))
})

test_that("invalid index", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  template <- quote(sin(NULL))
  expect_error(task_save_bulk(template, list(1), 0, ctx),
               "Invalid index")
  expect_error(task_save_bulk(template, list(1), 2, ctx),
               "Invalid index")
})
