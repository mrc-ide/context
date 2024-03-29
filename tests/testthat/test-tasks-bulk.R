context("tasks - bulk")

test_that("prepare; single arg", {
  x <- 1:4
  cmp <- lapply(x, function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, TRUE, FALSE), cmp)
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, FALSE, FALSE), cmp)
})

test_that("prepare; named single arg", {
  x <- setNames(1:4, letters[1:4])
  cmp <- lapply(x, function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, TRUE, FALSE), cmp)
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, FALSE, FALSE), cmp)
})

test_that("prepare", {
  x <- list(c(1, 2), c(3, 4))

  ## Not do_call
  cmp <- lapply(unname(x), function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, FALSE, FALSE), cmp)
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, FALSE, TRUE), cmp)

  ## do_call
  cmp <- lapply(x, function(el) list(expr = as.call(c(list(quote(foo)), el))))
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, TRUE, FALSE), cmp)
  expect_equal(bulk_prepare_expression(x, quote(foo), NULL, TRUE, TRUE), cmp)
})

test_that("prepare; data.frame", {
  df <- data.frame(a = 1:5, b = runif(5))

  ## not do_call, preserving and dropping names:
  cmp <- lapply(df_to_list(df, TRUE), function(el)
    list(expr = bquote(foo(.(el)))))
  expect_equal(bulk_prepare_expression(df, quote(foo), NULL, FALSE, TRUE),
               cmp)
  expect_equal(bulk_prepare_expression(df, quote(foo), NULL, FALSE, FALSE), cmp)

  ## do_call, preserving names
  cmp <- lapply(df_to_list(df, TRUE), function(el)
    list(expr = as.call(c(list(quote(foo)), el))))
  expect_equal(bulk_prepare_expression(df, quote(foo), NULL, TRUE, TRUE), cmp)

  ## do_call, dropping names
  cmp <- lapply(df_to_list(df, TRUE), function(el)
    list(expr = as.call(c(list(quote(foo)), unname(el)))))
  expect_equal(bulk_prepare_expression(df, quote(foo), NULL, TRUE, FALSE), cmp)
})

test_that("prepare; uneven length lists", {
  x <- list(1, c(1, 2))
  cmp <- lapply(x, function(el) list(expr = bquote(foo(.(el)))))
  expect_equal(
    bulk_prepare_expression(x, quote(foo), NULL, FALSE, TRUE),
    cmp)

  ## Error case
  expect_error(
    bulk_prepare_expression(list(1, 1:2, 1:3), quote(foo), NULL, TRUE, TRUE),
    "Every element of 'X' must have the same length")
})

test_that("prepare; error cases", {
  expect_error(
    bulk_prepare_expression(NULL, quote(foo), NULL, TRUE, TRUE),
    "X must be a data.frame or list", fixed = TRUE)
  expect_error(
    bulk_prepare_expression(quote(sin), quote(foo), NULL, TRUE, TRUE),
    "X must be a data.frame or list", fixed = TRUE)
  expect_error(
    bulk_prepare_expression(quote(1 + 2), quote(foo), NULL, TRUE, TRUE),
    "X must be a data.frame or list", fixed = TRUE)

  ## Zero length
  df <- data.frame(a = 1:5, b = runif(5))
  expect_error(
    bulk_prepare_expression(df[integer(0),], quote(foo), NULL, FALSE, TRUE),
    "'X' must have at least one row", fixed = TRUE)
  expect_error(
    bulk_prepare_expression(df[, integer(0)], quote(foo), NULL, FALSE, TRUE),
    "'X' must have at least one column", fixed = TRUE)
  expect_error(
    bulk_prepare_expression(list(), quote(foo), NULL, FALSE, TRUE),
    "'X' must have at least one element", fixed = TRUE)
  expect_error(
    bulk_prepare_expression(list(list(), list()), quote(foo), NULL, TRUE, TRUE),
    "Elements of 'X' must have at least one element", fixed = TRUE)
})

## This is the simplest case; where we only have a single element to
## add.
test_that("simple", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  ids <- bulk_task_save(1:5, quote(sin), ctx)
  expect_is(ids, "character")
  expect_equal(length(ids), 5)
  expect_false(any(duplicated(ids)))

  expect_equal(task_status(ids, ctx), rep("PENDING", length(ids)))
  expect_is(task_times(ids, ctx)$submitted, "POSIXt")

  expect_equal(task_list(ctx), sort(ids))

  expect_equal(lapply(ids, task_run, ctx_run), as.list(sin(1:5)))
  expect_equal(lapply(ids, task_result, ctx), as.list(sin(1:5)))
})

test_that("named", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  x <- setNames(1:5, letters[1:5])
  ids <- bulk_task_save(x, quote(sin), ctx)
  expect_is(ids, "character")
  expect_equal(names(ids), names(x))
})

test_that("data.frame", {
  df <- data.frame(a = 1:5, b = runif(5))
  ctx <- context_save(tempfile(), storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  ids <- bulk_task_save(df, quote(foo), ctx)
  expect_is(ids, "character")
  expect_null(names(ids))

  rownames(df) <- letters[seq_len(nrow(df))]
  ids <- bulk_task_save(df, quote(foo), ctx)
  expect_is(ids, "character")
  expect_equal(names(ids), rownames(df))
})

test_that("bulk, multiple arguments", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  X <- expand.grid(x = as.numeric(1:3), rate = as.numeric(1:3),
                   KEEP.OUT.ATTRS = FALSE)

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)
  t2 <- task_save(expr, ctx)
  ids <- bulk_task_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                        do_call = TRUE, use_names = FALSE)
  expect_equal(task_expr(ids[[1]], ctx),
               quote(dgamma(1, 1, shape = 2)))
  expect_equal(task_expr(ids[[5]], ctx),
               bquote(dgamma(.(X[[c(1, 5)]]), .(X[[c(2, 5)]]), shape = 2)))
})

test_that("validates dependencies", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  X <- expand.grid(x = as.numeric(1:1), rate = as.numeric(1:3),
                   KEEP.OUT.ATTRS = FALSE)
  expect_error(bulk_task_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                              do_call = TRUE, use_names = FALSE, depends_on = rep("123", 3)),
               "Failed to save as dependency 123 does not exist")
  expect_error(bulk_task_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                              do_call = TRUE, use_names = FALSE, depends_on = list(list("123", "456"), list(), list("124"))),
               "Failed to save as dependencies 123, 456, 124 do not exist")

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)
  t2 <- task_save(expr, ctx)
  expect_error(bulk_task_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                              do_call = TRUE, use_names = FALSE, depends_on = list(t, t2)),
               paste("'depends_on' must either be a vector or a list of length 3",
                     "with an element per task, but was a list of length 2."))

  # vector of dependencies
  ids <- bulk_task_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                        do_call = TRUE, use_names = FALSE, depends_on = c(t, t2))
  expect_equal(task_deps(ids, ctx), replicate(3, c(t, t2), FALSE))
  expect_equal(task_expr(ids[[1]], ctx),
               quote(dgamma(1, 1, shape = 2)))

  # list of lists of dependencies
  deps <- list(list(t, t2), list(), t)
  ids <- bulk_task_save(X, quote(dgamma), ctx, DOTS = list(shape = 2),
                        do_call = TRUE, use_names = FALSE, depends_on = deps)
  expect_equal(task_deps(ids, ctx), deps)
})

test_that("local variables", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  e <- environment()
  e$shape <- 2

  X <- expand.grid(x = as.numeric(1:3), rate = as.numeric(1:3),
                   KEEP.OUT.ATTRS = FALSE)
  ids <- bulk_task_save(X, quote(dgamma), ctx,
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

test_that("function-by-value", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  on.exit(unlink(ctx$root$path, recursive = TRUE))

  ids <- bulk_task_save(1:5, function(x) x * 2, ctx)
  expect_is(ids, "character")
  expect_equal(length(ids), 5)
  expect_false(any(duplicated(ids)))

  expect_equal(task_status(ids, ctx), rep("PENDING", length(ids)))
  expect_is(task_times(ids, ctx)$submitted, "POSIXt")

  expect_equal(task_list(ctx), sort(ids))

  nm <- unname(task_function_name(ids, ctx))
  expect_equal(length(unique(nm)), 1L)
  expect_true(is_id(nm[[1]]))

  expect_equal(lapply(ids, task_run, ctx_run), as.list(1:5 * 2))
  expect_equal(lapply(ids, task_result, ctx), as.list(1:5 * 2))
})

test_that("heterogenous list do_call fails", {
  expect_error(
    bulk_prepare_expression_X(list(list(a = 1), list(b = 2)), TRUE),
    "Elements of 'X' must have the same names")
})

test_that("invalid function", {
  db <- storr::storr_environment()
  expect_error(
    bulk_prepare_expression(1:4, NULL, NULL, TRUE, FALSE, .GlobalEnv, db),
    "Expected 'FUN' to be a symbol, fully qualified name or function")
})

test_that("factors", {
  ## We can't easily deal with factors yet; most of the time they
  ## should just be character anyway, but I want to be explicit here.
  ##
  ## There are doubtless other types of constructs that don't survive
  ## being inserted into the expressions in the way that I'm doing
  ## here.  I don't really know what a better way of doing that is
  ## though; we don't want to end up with a big pile of dput()
  ## statements.  At the same time, most data-frame-to-matrix
  ## approaches will strip off additional class attributes, etc, so
  ## it's not likely to be that bad in practice.
  dat <- data.frame(a = 1:2, b = c('a', 'b'),
                    stringsAsFactors = TRUE)
  expect_error(bulk_prepare_expression_X(dat, FALSE, TRUE),
               "Factors cannot be used in bulk expressions")
  expect_error(bulk_prepare_expression_X(df_to_list(dat, TRUE), FALSE, TRUE),
               "Factors cannot be used in bulk expressions")
  expect_error(bulk_prepare_expression_X(factor("a"), FALSE, TRUE),
               "Factors cannot be used in bulk expressions")
})

test_that("recycle", {
  expect_equal(bulk_prepare_expression_X(list(a = 1:2, b = "a"), TRUE, TRUE),
               list(a = 1:2, b = c("a", "a")))
})
