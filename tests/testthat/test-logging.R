context("logging")

test_that("logging", {
  if (context_log_start()) {
    on.exit(context_log_stop())
  }
  expect_message(context_log("topic", "value"),
                 "[ topic     ]  value", fixed = TRUE)
  expect_message(context_log("topic", c("a", "b")),
                 "[ topic     ]  a\n[ ...       ]  b", fixed = TRUE)
})

test_that("parse_context_log", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns.R")
  expr <- quote(loop(1, 10))
  t <- task_save(expr, ctx)

  logfile <- tempfile()
  ctx$db$set(t, logfile, "log_path")

  full <- file.path(path_bin(path), "task_run")
  res <- Rscript(c(full, path, t), stdout = logfile, stderr = logfile)
  expect_equal(res, 0)

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  expect_equal(task_result(t, ctx), 1024)

  log <- task_log(t, ctx)
  expect_is(log, "context_log")
  oo <- options(crayon.enabled = TRUE)
  on.exit(options(oo), add = TRUE)
  res <- capture.output(print(log, TRUE))
  expect_true(!identical(crayon::strip_style(res), res))
})
