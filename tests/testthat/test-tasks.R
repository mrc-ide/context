context("tasks")

test_that("tasks", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  expr <- quote(sin(1))
  ctx <- context_save(auto=TRUE, root=root)
  handle <- task_save(expr, ctx)

  expect_is(handle, "task_handle")
  expect_equal(nchar(handle$id), 32)
  expect_identical(handle$root, root)

  expect_identical(task_status_read(handle), TASK_PENDING)

  e <- new.env()
  dat <- task_load(handle, FALSE, e)
  expect_is(dat, "task")
  ## OK, this is nasty.  If we have a local environment, like in this
  ## situation, then unserialising that environment is going to create
  ## a situation where our *globals* aren't in the right place.  Such
  ## is life; I don't see what else we can do about that.
  expect_equal(ls(dat$envir), character(0))
  expect_identical(parent.env(dat$envir), dat$envir_context)
  expect_false(identical(dat$envir_context, e))
  expect_equal(ls(e), ls(.GlobalEnv))
  expect_equal(dat$expr, expr)
  expect_equal(eval(dat$expr, dat$envir), eval(expr))
})

test_that("task_list", {
  root <- tempfile("cluster_")
  x <- list(quote(sin(1)), quote(sin(2)))
  ctx <- context_save(auto=TRUE, root=root)
  obj <- task_save_list(x, ctx)
  expect_is(obj, "task_handle")
  expect_equal(length(obj), length(x))

  expect_identical(task_status_read(obj),
                   rep(TASK_PENDING, length(x)))

  ## subsetting:
  el <- obj[[1]]
  expect_is(el, "task_handle")
  expect_equal(el$id, obj$id[[1]])
  expect_identical(obj[1:2], obj)

  tmp <- lapply(obj, task_read)
  ctx <- vcapply(tmp, "[[", "context_id")
  expect_identical(ctx[[1]], ctx[[2]])
})
