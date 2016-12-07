context("tasks")

## This cycles through all the core task query functions in a totally
## empty context database and checks that everything behaves sensibly.
## These are mostly edge cases.
test_that("tasks in empty context", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, auto = TRUE)

  expect_equal(tasks_list(ctx), character(0))
  expect_equal(tasks_list(path), character(0))
  expect_equal(tasks_list(context_root(path)), character(0))

  ids <- ids::random_id(2)
  expect_false(tasks_delete(ids[[1]], ctx))
  expect_equal(tasks_delete(ids, ctx), c(FALSE, FALSE))
  expect_equal(tasks_delete(character(0), ctx), logical(0))

  id <- ids[[1]]
  expect_is(task_result(id, ctx, TRUE), "UnfetchableTask")
  expect_error(task_result(id, ctx), "unfetchable: MISSING")

  expect_equal(tasks_status(id, ctx), TASK_MISSING)
  expect_equal(tasks_status(ids, ctx), rep(TASK_MISSING, length(ids)))
  expect_equal(tasks_status(character(0), ctx), character(0))

  ## This affects task_load, task_expr, task_function_name
  expect_error(task_read(id, ctx), "not found")
  expect_error(task_log(id, ctx), "Logging not enabled")

  res <- tasks_times(ids, ctx)
  expect_is(res, "data.frame")
  expect_equal(res$task_id, ids)
  expect_equal(res$submitted, missing_time(length(ids)))
  expect_equal(res$started, missing_time(length(ids)))
  expect_equal(res$finished, missing_time(length(ids)))
  expect_equal(res$waiting, rep(NA_real_, 2))
  expect_equal(res$running, rep(NA_real_, 2))
  expect_equal(res$idle, rep(NA_real_, 2))
})

test_that("single task", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, auto = TRUE)

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  expect_true(is_id(t))
  expect_equal(tasks_list(ctx), t)
  expect_equal(tasks_status(t, ctx), TASK_PENDING)

  expect_is(task_result(t, ctx, TRUE), "UnfetchableTask")
  expect_error(task_result(t, ctx), "unfetchable: PENDING")

  res <- tasks_times(t, ctx)
  expect_is(res, "data.frame")
  expect_equal(res$task_id, t)
  expect_is(res$submitted, "POSIXt")
  expect_false(is.na(res$submitted))
  expect_equal(res$started, missing_time())
  expect_equal(res$finished, missing_time())
  expect_false(is.na(res$waiting))
  expect_equal(res$running, NA_real_)
  expect_equal(res$idle, NA_real_)

  dat <- task_read(t, path)
  expect_is(dat$db, "storr")
  expect_equal(dat$context_id, ctx$id)
  expect_equal(dat$expr, expr)
  expect_null(dat$objects)

  e <- new.env()
  dat <- task_load(t, ctx, e, load_context = TRUE, install = FALSE)

  expect_is(dat$db, "storr")
  expect_equal(dat$expr, expr)
  expect_equal(eval(dat$expr, dat$envir), eval(expr))

  ## OK, this is nasty.  If we have a local environment, like in this
  ## situation, then unserialising that environment is going to create
  ## a situation where our *globals* aren't in the right place.  Such
  ## is life; I don't see what else we can do about that.
  expect_equal(ls(dat$envir), character(0))
  expect_identical(parent.env(dat$envir), dat$envir_context)
  if (!identical(environment(), .GlobalEnv)) {
    expect_false(identical(dat$envir_context, e))
    expect_equal(ls(e), ls(.GlobalEnv))
  }

  expect_equal(task_run(t, ctx), eval(expr))
})

test_that("task_list", {
  skip("rework this")
  path <- tempfile("cluster_")
  x <- list(quote(sin(1)), quote(sin(2)))
  ctx <- context_save(auto = TRUE, path = path)
  obj <- task_save_list(x, ctx)
  expect_is(obj, "task_handle")
  expect_equal(length(obj), length(x))

  expect_identical(task_status(obj),
                   rep(TASK_PENDING, length(x)))
  expect_identical(task_status(obj, TRUE),
                   setNames(rep(TASK_PENDING, length(x)),
                            obj$id))

  ## subsetting:
  el <- obj[[1]]
  expect_is(el, "task_handle")
  expect_equal(el$id, obj$id[[1]])
  expect_identical(obj[1:2], obj)

  tmp <- lapply(seq_along(obj), function(i) task_read(obj[[i]]))
  ctx <- vcapply(tmp, "[[", "context_id")
  expect_identical(ctx[[1]], ctx[[2]])
})

test_that("task_delete (single)", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, auto = TRUE)

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  expect_equal(tasks_list(ctx), t)
  expect_true(ctx$db$exists(t, "tasks"))
  expect_true(tasks_delete(t, ctx))
  expect_equal(tasks_list(ctx), character(0))

  expect_false(ctx$db$exists(t, "tasks"))
  expect_false(tasks_delete(t, ctx))
})

test_that("task_delete (multiple)", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, auto = TRUE)

  expr <- quote(sin(1))
  t1 <- task_save(expr, ctx)
  t2 <- task_save(expr, ctx)
  t3 <- task_save(expr, ctx)
  tt <- c(t1, t2, t3)

  expect_true(all(tt %in% tasks_list(ctx)))
  expect_equal(ctx$db$exists(tt, "tasks"), rep(TRUE, length(tt)))
  i <- 1:2
  expect_equal(tasks_delete(tt[i], ctx), rep(TRUE, length(tt[i])))
  expect_equal(sort(tt[-i]), sort(tasks_list(ctx)))

  expect_equal(tasks_delete(tt, ctx), !(seq_along(tt) %in% i))
  expect_equal(tasks_delete(tt, ctx), rep(FALSE, length(tt)))

  expect_equal(tasks_list(ctx), character(0))
})

test_that("local variables", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  x <- 1
  expr <- quote(sin(x))
  ctx <- context_save(path)
  t <- task_save(expr, ctx)

  dat <- task_read(t, ctx)
  expect_equal(names(dat$objects), "x")
  expect_equal(unname(dat$objects), ctx$db$hash_object(x))

  e <- new.env(parent = .GlobalEnv)
  dat <- task_load(t, ctx, envir = e)

  expect_identical(ls(dat$envir), "x")
  expect_identical(dat$envir$x, x)
  expect_identical(parent.env(dat$envir), dat$envir_context)

  res <- task_run(t, ctx, , envir = e)
  expect_equal(res, sin(1))
})

test_that("task_run & times", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  x <- 1
  y <- 2
  expr <- quote(list(x, y))
  ctx <- context_save(path)

  expect_equal(ctx$db$list("task_time_sub"), character(0))
  expect_equal(ctx$db$list("task_time_beg"), character(0))
  expect_equal(ctx$db$list("task_time_end"), character(0))

  t <- task_save(expr, ctx)

  expect_equal(ctx$db$list("task_time_sub"), t)
  expect_equal(ctx$db$list("task_time_beg"), character(0))
  expect_equal(ctx$db$list("task_time_end"), character(0))

  e <- new.env(parent = environment())
  res <- task_run(t, ctx, envir = e)
  expect_identical(res, list(x, y))

  expect_equal(ctx$db$list("task_time_sub"), t)
  expect_equal(ctx$db$list("task_time_beg"), t)
  expect_equal(ctx$db$list("task_time_end"), t)

  t_sub <- ctx$db$get(t, "task_time_sub")
  t_beg <- ctx$db$get(t, "task_time_beg")
  t_end <- ctx$db$get(t, "task_time_end")

  expect_is(t_sub, "POSIXt")
  expect_true(t_beg >= t_sub)
  expect_true(t_end >= t_beg)

  expect_equal(tasks_status(t, ctx), TASK_COMPLETE)
})

test_that("complex expressions", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  x <- 1
  y <- 10
  n <- 2
  expr <- quote(rep(x:y, n))
  ctx <- context_save(path)

  t <- task_save(expr, ctx)

  tmp <- task_read(t, ctx)
  expect_equal(tmp$expr, expr)
  expect_equal(sort(names(tmp$objects)), c("n", "x", "y"))

  res <- task_run(t, ctx)
  expect_equal(res, eval(expr))
})

test_that("stack trace", {
  ## context_log_start()
  ctx <- context::context_save(tempfile(), storage_type = "environment")
  t <- task_save(quote(readLines("asdfa.txt")), ctx)
  ## Warning is not suppressed:
  expect_warning(res <- task_run(t, ctx, print_error = FALSE), "No such file")
  expect_is(res, "context_task_error")
  expect_is(res$trace, "character")
  expect_match(tail(res$trace, 2)[[1]], "^readLines")
})

test_that("stack trace, no warning", {
  ctx <- context::context_save(tempfile(), storage_type = "environment",
                               sources = "myfuns.R")
  t <- task_save(quote(f(-10)), ctx)
  context_log_start()
  on.exit(context_log_stop())
  expect_message(res <- task_run(t, ctx, print_error = TRUE),
                 "Need positive x")
})

test_that("long expr", {
  ## TODO: this is obviously missing a second half where something
  ## that went *wrong* was printed.  Not sure what that is though!
  ctx <- context::context_save(tempfile(), storage_type = "environment")
  task <- task_save(quote(list(a_label = "a value",
                               another_label = pi,
                               one_more = c(exp(1), pi, 123.12312),
                               last_one = "a very long string here to wrap")),
                    ctx)
})

test_that("load_context", {
  ctx <- context::context_save(tempfile(),
                               sources = "noisy.R",
                               storage_type = "environment")
  t <- task_save(quote(times_two(2)), ctx)
  expect_message(env <- context_load(ctx), "NOISY")

  expect_silent(ans <- task_run(t, ctx, load_context = FALSE, envir = env))
  expect_equal(ans, 4)
  expect_message(ans <- task_run(t, ctx, load_context = TRUE), "NOISY")
  expect_equal(ans, 4)
})

test_that("print", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  t <- task_save(quote(sin(2)), ctx)
  dat <- task_read(t, ctx)
  expect_output(print(dat), "<task>", fixed = TRUE)
})
