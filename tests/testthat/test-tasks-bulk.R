context("tasks - bulk")

test_that("prepare; single arg", {
  x <- 1:4
  cmp <- lapply(x, function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, TRUE, FALSE), cmp)
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, FALSE, FALSE), cmp)
})

test_that("prepare; named single arg", {
  x <- setNames(1:4, letters[1:4])
  cmp <- lapply(unname(x), function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, TRUE, FALSE), cmp)
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, FALSE, FALSE), cmp)
})

test_that("prepare", {
  x <- list(c(1, 2), c(3, 4))

  ## Not do_call
  cmp <- lapply(unname(x), function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, FALSE, FALSE), cmp)
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, FALSE, TRUE), cmp)

  ## do_call
  cmp <- lapply(x, function(el) list(expr = as.call(c(list(quote(foo)), el))))
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, TRUE, FALSE), cmp)
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, TRUE, TRUE), cmp)
})

test_that("prepare; data.frame", {
  df <- data.frame(a = 1:5, b = runif(5))

  ## not do_call, preserving and dropping names:
  cmp <- lapply(df_to_list(df, TRUE), function(el)
    list(expr = bquote(foo(.(el)))))
  expect_equal(task_bulk_prepare(df, quote(foo), NULL, FALSE, TRUE),
               cmp)
  expect_equal(task_bulk_prepare(df, quote(foo), NULL, FALSE, FALSE), cmp)

  ## do_call, preserving names
  cmp <- lapply(df_to_list(df, TRUE), function(el)
    list(expr = as.call(c(list(quote(foo)), el))))
  expect_equal(task_bulk_prepare(df, quote(foo), NULL, TRUE, TRUE), cmp)

  ## do_call, dropping names
  cmp <- lapply(df_to_list(df, TRUE), function(el)
    list(expr = as.call(c(list(quote(foo)), unname(el)))))
  expect_equal(task_bulk_prepare(df, quote(foo), NULL, TRUE, FALSE), cmp)
})

test_that("prepare; uneven length lists", {
  x <- list(1, c(1, 2))
  cmp <- lapply(x, function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(task_bulk_prepare(x, quote(foo), NULL, FALSE, TRUE),
               cmp)

  ## Error case
  expect_error(task_bulk_prepare(list(1, 1:2), quote(foo), NULL, TRUE, TRUE),
               "Every element of 'X' must have the same length")
})

test_that("prepare; error cases", {
  expect_error(task_bulk_prepare(NULL, quote(foo), NULL, TRUE, TRUE),
               "X must be a data.frame or list", fixed = TRUE)
  expect_error(task_bulk_prepare(quote(sin), quote(foo), NULL, TRUE, TRUE),
               "X must be a data.frame or list", fixed = TRUE)
  expect_error(task_bulk_prepare(quote(1 + 2), quote(foo), NULL, TRUE, TRUE),
               "X must be a data.frame or list", fixed = TRUE)

  ## Zero length
  df <- data.frame(a = 1:5, b = runif(5))
  expect_error(
    task_bulk_prepare(df[integer(0), ], quote(foo), NULL, FALSE, TRUE),
    "'X' must have at least one row", fixed = TRUE)
  expect_error(
    task_bulk_prepare(df[, integer(0)], quote(foo), NULL, FALSE, TRUE),
    "'X' must have at least one column", fixed = TRUE)
  expect_error(
    task_bulk_prepare(list(), quote(foo), NULL, FALSE, TRUE),
    "'X' must have at least one element", fixed = TRUE)
  expect_error(
    task_bulk_prepare(list(list(), list()), quote(foo), NULL, TRUE, TRUE),
    "Elements of 'X' must have at least one element", fixed = TRUE)
})

## This is the simplest case; where we only have a single element to
## add.
test_that("simple", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  ids <- task_bulk_save(1:5, quote(sin), ctx)
  expect_is(ids, "character")
  expect_equal(length(ids), 5)
  expect_false(any(duplicated(ids)))

  expect_equal(task_status(ids, ctx), rep("PENDING", length(ids)))
  expect_is(task_times(ids, ctx)$submitted, "POSIXt")

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
  ids <- task_bulk_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                        do_call = TRUE, use_names = FALSE)

  expect_equal(task_expr(ids[[1]], ctx),
               quote(dgamma(1, 1, shape = 2)))
  expect_equal(task_expr(ids[[5]], ctx),
               bquote(dgamma(.(X[[c(1, 5)]]), .(X[[c(2, 5)]]), shape = 2)))
})

test_that("local variables", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  e <- environment()
  e$shape <- 2

  X <- expand.grid(x = as.numeric(1:3), rate = as.numeric(1:3),
                   KEEP.OUT.ATTRS = FALSE)
  ids <- task_bulk_save(X, quote(dgamma), ctx,
                        DOTS = list(shape = quote(shape)),
                        do_call = TRUE, use_names = FALSE)

  ## As above:
  expect_equal(task_expr(ids[[1]], ctx),
               quote(dgamma(1, 1, shape = shape)))
  expect_equal(task_expr(ids[[5]], ctx),
               bquote(dgamma(.(X[[c(1, 5)]]), .(X[[c(2, 5)]]), shape = shape)))

  res <- task_expr(ids[[1]], ctx$db, TRUE)
  locals <- attr(res, "locals")
  expect_equal(locals, c(shape = ctx$db$hash_object(shape)))
  expect_equal(ctx$db$get(locals), e$shape)
  expect_equal(ctx$db$export(list()), setNames(list(e$shape), locals))
})
