context("tasks")

test_that("tasks", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  expr <- quote(sin(1))
  handle <- save_task(expr, root=root)

  expect_is(handle, "task_handle")
  expect_equal(nchar(handle$id), 32)
  expect_identical(handle$root, root)

  e <- new.env()
  dat <- load_task(handle, FALSE, e)
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
