context("tasks")

## This cycles through all the core task query functions in a totally
## empty context database and checks that everything behaves sensibly.
## These are mostly edge cases.
test_that("tasks in empty context", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path)

  expect_equal(task_list(ctx), character(0))
  expect_equal(task_list(path), character(0))
  expect_equal(task_list(context_root(path)), character(0))

  ids <- ids::random_id(2)
  id <- ids[[1]]

  expect_false(task_delete(id, ctx))
  expect_equal(task_delete(ids, ctx), c(FALSE, FALSE))
  expect_equal(task_delete(character(0), ctx), logical(0))

  expect_is(task_result(id, ctx, TRUE), "UnfetchableTask")
  expect_error(task_result(id, ctx), "unfetchable: MISSING")

  expect_equal(task_status(id, ctx), TASK_MISSING)
  expect_equal(task_status(ids, ctx), rep(TASK_MISSING, length(ids)))
  expect_equal(task_status(character(0), ctx, named = FALSE), character(0))
  expect_equal(task_status(character(0), ctx, named = TRUE),
               setNames(character(0), character(0)))

  expect_equal(task_context_id(character(0), ctx), character(0))
  expect_equal(task_context_id(id, ctx), NA_character_)
  expect_equal(task_context_id(ids, ctx), rep(NA_character_, length(ids)))

  expect_error(task_read(id, ctx), "not found")
  expect_error(task_log(id, ctx), "Logging not enabled")

  expect_equal(task_context(id, ctx), NA_character_)
  expect_equal(task_context(ids, ctx), rep(NA_character_, 2L))
  expect_equal(task_context(character(0), ctx), character(0))

  res <- task_times(ids, ctx)
  expect_is(res, "data.frame")
  expect_equal(res$task_id, ids)
  expect_equal(res$submitted, missing_time(length(ids)))
  expect_equal(res$started, missing_time(length(ids)))
  expect_equal(res$finished, missing_time(length(ids)))
  expect_equal(res$waiting, rep(NA_real_, 2))
  expect_equal(res$running, rep(NA_real_, 2))
  expect_equal(res$idle, rep(NA_real_, 2))

  res0 <- task_times(character(0), ctx)
  expect_equal(res0, res[integer(0), ])
})

test_that("single task", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path)

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  expect_true(is_id(t))
  expect_equal(task_list(ctx), t)
  expect_equal(task_status(t, ctx, TRUE), setNames(TASK_PENDING, t))
  expect_equal(task_status(t, ctx, FALSE), TASK_PENDING)
  expect_equal(task_context(t, ctx), ctx$id)

  expect_is(task_result(t, ctx, TRUE), "UnfetchableTask")
  expect_error(task_result(t, ctx), "unfetchable: PENDING")

  res <- task_times(t, ctx)
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
  expect_equal(task_context_id(t, path), ctx$id)

  expect_equal(task_expr(t, path), expr)
  expect_equal(task_function_name(t, path), "sin")

  e <- new.env()
  ctx_run <- context_load(ctx, e)
  dat <- task_load(t, ctx_run)

  expect_is(dat$db, "storr")
  expect_equal(dat$expr, expr)
  expect_equal(eval(dat$expr, dat$envir), eval(expr))

  ## OK, this is nasty.  If we have a local environment, like in this
  ## situation, then unserialising that environment is going to create
  ## a situation where our *globals* aren't in the right place.  Such
  ## is life; I don't see what else we can do about that.
  expect_equal(ls(dat$envir), character(0))
  expect_identical(parent.env(dat$envir), ctx_run$envir)
  if (!identical(environment(), .GlobalEnv)) {
    expect_false(identical(dat$parent, e))
    expect_equal(ls(e), ls(.GlobalEnv))
  }

  expect_equal(task_run(t, ctx_run), eval(expr))
})

test_that("task_delete (single)", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, storage_type = "environment")

  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  expect_equal(task_list(ctx), t)
  expect_true(ctx$db$exists(t, "tasks"))
  expect_true(task_delete(t, ctx))
  expect_equal(task_list(ctx), character(0))

  expect_false(ctx$db$exists(t, "tasks"))
  expect_false(task_delete(t, ctx))
})

test_that("task_delete (multiple)", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, storage_type = "environment")

  expr <- quote(sin(1))
  t1 <- task_save(expr, ctx)
  t2 <- task_save(expr, ctx)
  t3 <- task_save(expr, ctx)
  tt <- c(t1, t2, t3)

  expect_true(all(tt %in% task_list(ctx)))
  expect_equal(ctx$db$exists(tt, "tasks"), rep(TRUE, length(tt)))
  i <- 1:2
  expect_equal(task_delete(tt[i], ctx), rep(TRUE, length(tt[i])))
  expect_equal(sort(tt[-i]), sort(task_list(ctx)))

  expect_equal(task_delete(tt, ctx), !(seq_along(tt) %in% i))
  expect_equal(task_delete(tt, ctx), rep(FALSE, length(tt)))

  expect_equal(task_list(ctx), character(0))
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
  ctx_run <- context_load(ctx, e)
  dat <- task_load(t, ctx_run)

  expect_identical(ls(dat$envir), "x")
  expect_identical(dat$envir$x, x)
  expect_identical(parent.env(dat$envir), ctx_run$envir)

  expect_equal(task_expr(t, ctx), expr)
  expect_equal(task_expr(t, ctx, TRUE),
               structure(expr, locals = dat$objects))

  res <- task_run(t, ctx_run)
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
  ctx_run <- context_load(ctx, e)
  res <- task_run(t, ctx_run)
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

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
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

  res <- task_run(t, context_load(ctx, new.env(parent = .GlobalEnv)))
  expect_equal(res, eval(expr))
})

test_that("stack trace", {
  ## context_log_start()
  ctx <- context_save(tempfile(), storage_type = "environment")
  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  t <- task_save(quote(readLines("asdfa.txt")), ctx)
  ## Warning is not suppressed:
  expect_warning(res <- task_run(t, ctx_run), "No such file")
  expect_is(res, "context_task_error")
  expect_is(res$trace, "character")
  expect_match(tail(res$trace, 2)[[1]], "^readLines")
})

test_that("stack trace, no warning", {
  ctx <- context_save(tempfile(), storage_type = "environment",
                      sources = "myfuns.R")
  t <- task_save(quote(f(-10)), ctx)

  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  context_log_start()
  on.exit(context_log_stop())
  expect_message(res <- task_run(t, ctx_run),
                 "Need positive x")
  expect_null(res$warnings)
  expect_match(tail(res$trace, 1), "stop")
})

test_that("long expr", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  task <- task_save(quote(list(a_label = "a value",
                               another_label = pi,
                               one_more = c(exp(1), pi, 123.12312),
                               last_one = "a very long string here to wrap")),
                    ctx)

  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  context_log_start()
  on.exit(context_log_stop())
  msg <- capture_messages(res <- task_run(task, ctx_run))
  msg <- strsplit(paste(msg, collapse = ""), "\n")[[1]]
  dat <- parse_context_log(sub("\n$", "", msg))
  expect_true("..." %in% dat$title)
  expect_equal(dat$title[[which(dat$title == "expr") + 1L]], "...")
})

test_that("print", {
  ctx <- context_save(tempfile(), storage_type = "environment")
  t <- task_save(quote(sin(2)), ctx)
  dat <- task_read(t, ctx)
  expect_output(print(dat), "<task>", fixed = TRUE)
})

test_that("capture output", {
  context_log_start()
  on.exit(context_log_stop())
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  t <- task_save(quote(sin(2)), ctx)
  logfile <- tempfile()

  ## Problem: This one is not working with testthat, unfortunately;
  ## getting it working probably will mean either working out how to
  ## win the sink fight with testthat, or testing in a subprocess.
  ##
  ## Solution via:
  ## https://github.com/hadley/testthat/issues/460
  res <- withCallingHandlers(
    task_run(t, ctx_run, filename = logfile),
    message = function(e) cat(conditionMessage(e), file = stderr(), sep = ""))
  expect_equal(res, sin(2))
  expect_true(file.exists(logfile))
  dat <- parse_context_log(readLines(logfile))
  expect_is(dat, "context_log")
  str <- capture.output(print(dat))
  expect_match(str[[1]], "[ root", fixed = TRUE)

  ctx$db$set(t, logfile, "log_path")
  expect_equal(task_log(t, ctx), dat)

  txt <- task_log(t, ctx, FALSE)
  expect_equal(parse_context_log(txt), task_log(t, ctx))

  logpath <- file.path(ctx$root$path, "logs")
  dir.create(logpath, FALSE, TRUE)
  file.copy(logfile, file.path(logpath, t))

  ctx$db$set(t, "logs", "log_path")
  expect_equal(task_log(t, ctx), dat)

  ctx$db$set(t, file.path("logs", t), "log_path")
  expect_equal(task_log(t, ctx), dat)

  ctx$db$set(t, tempfile(), "log_path")
  expect_error(task_log(t, ctx), "Logfile does not exist at")
})

test_that("can't run without loading", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  t <- task_save(quote(sin(1)), ctx)
  expect_error(task_run(t, ctx), "context is not loaded")
})

test_that("fetch task result", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  ctx_run <- context_load(ctx, new.env(parent = .GlobalEnv))
  on.exit(unlink(ctx$root$path, recursive = TRUE))
  t <- task_save(quote(sin(1)), ctx)
  expect_equal(task_run(t, ctx_run), sin(1))

  expect_equal(task_result(t, ctx$root), sin(1))
  expect_equal(task_status(t, ctx$root), TASK_COMPLETE)
})

test_that("task_function_name", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))
  t1 <- task_save(quote(sin(1)), ctx)
  t2 <- task_save(quote(cos(1)), ctx)

  expect_equal(task_function_name(character(0), ctx),
               setNames(character(0), character(0)))
  expect_equal(task_function_name(c(t1, t2), ctx),
               setNames(c("sin", "cos"), c(t1, t2)))
  expect_equal(task_function_name(c(t1, t1), ctx),
               setNames(c("sin", "sin"), c(t1, t1)))
  expect_error(task_function_name(ids::random_id(), ctx),
               "not found")

  ## Names pass through:
  expect_equal(task_function_name(c(a = t1, b = t2), ctx),
               setNames(c("sin", "cos"), c("a", "b")))
})

test_that("task_exists", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))
  t1 <- task_save(quote(sin(1)), ctx)
  t2 <- task_save(quote(cos(1)), ctx)
  t3 <- ids::random_id()

  expect_equal(task_exists(character(0), ctx), logical(0))
  expect_equal(task_exists(t1, ctx), TRUE)
  expect_equal(task_exists(c(t1, t2), ctx), rep(TRUE, 2))
  expect_equal(task_exists(c(t1, t3, t2), ctx), c(TRUE, FALSE, TRUE))
})

test_that("invalid task", {
  ctx <- context_save(tempfile(),
                      storage_type = "environment")
  on.exit(unlink(ctx$root$path, recursive = TRUE))
  ## Not a great message:
  expect_error(task_save(sin(1), ctx),
               "expr must inherit from call")
  expect_error(task_save(pi, ctx),
               "expr must inherit from call")
  expect_error(task_save(quote(sin), ctx),
               "expr must inherit from call")
})
