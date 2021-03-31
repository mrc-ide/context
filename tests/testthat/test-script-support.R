context("script support")

test_that("can parse cli args", {
  expect_error(parse_main_task_run(character(0)), "Usage:")
  expect_error(parse_main_task_run("root"), "Usage:")
  expect_error(parse_main_task_run(c("root", "a", "b")), "Usage:")
  expect_equal(parse_main_task_run(c("root", "id")),
               list(root = "root", id = "id"))
})


test_that("run main entry point", {
  path <- tempfile("context_")
  on.exit(cleanup(path))
  ctx <- context_save(path, envir = .GlobalEnv)

  t <- task_save(quote(sin(1)), ctx)

  res <- evaluate_promise(main_task_run(c(path, t)))
  ans <- task_run_callr(path, t)
  expect_null(res$result)
  expect_match(res$messages, "[ start", fixed = TRUE, all = FALSE)

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  expect_equal(task_result(t, ctx), sin(1))
})


test_that("Can fail informatively", {
  skip_if_not_installed("mockery")
  mock_load_ns <- mockery::mock(stop("there is no package called 'context'"))

  mockery::stub(context_startup, "loadNamespace", mock_load_ns)

  expect_message(
    res <- tryCatch(context_startup(), error = identity),
    ".libPaths()", fixed = TRUE)
  mockery::expect_called(mock_load_ns, 1L)
  expect_match(res$message, "there is no package called 'context'")
})


test_that("Can call context from a script", {
  skip_if_not_installed("callr")

  path <- tempfile("context_")
  on.exit(cleanup(path))
  ctx <- context_save(path, envir = .GlobalEnv)

  t <- task_save(quote(sin(1)), ctx)

  bin <- file.path(path_bin(path), "task_run")
  ans <- callr::rscript(bin, c(path, t), echo = FALSE, show = FALSE)
  expect_equal(ans$status, 0)

  expect_equal(task_result(t, ctx), sin(1))
})
