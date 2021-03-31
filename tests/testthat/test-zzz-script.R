context("script")

test_that("basic", {
  skip_if_not_installed("callr")

  path <- tempfile("context_")
  on.exit(cleanup(path))
  ctx <- context_save(path, envir = .GlobalEnv)

  t <- task_save(quote(sin(1)), ctx)

  ans <- task_run_callr(path, t)
  expect_equal(ans$status, 0)

  expect_equal(task_result(t, ctx), sin(1))
  expect_equal(task_status(t, ctx), TASK_COMPLETE)
})


test_that("error", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns.R")
  t <- task_save(quote(g(-1)), ctx)

  full <- file.path(path_bin(path), "task_run")

  res <- task_run_callr(path, t, fail_on_status = FALSE)
  expect_equal(res$status, 1)
  expect_equal(task_status(t, ctx), TASK_ERROR)

  r <- task_result(t, ctx)
  expect_is(r, "context_task_error")
  expect_equal(r$trace[[1]], "context:::main_task_run()")
})


test_that("parallel", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns-parallel.R")
  t <- task_save(quote(get_cluster_pids()), ctx)

  ans1 <- task_run_callr(path, t, fail_on_status = FALSE)
  expect_equal(ans1$status, 1)
  expect_equal(task_status(t, ctx), TASK_ERROR)

  res1 <- task_result(t, ctx)
  expect_is(res1, "context_task_error")
  expect_match(res1$message, "registered")
  expect_true(any(grepl("defaultCluster", res1$trace)))

  ans2 <- withr::with_envvar(
    c(CONTEXT_CORES = 2L),
    task_run_callr(path, t, fail_on_status = FALSE))
  expect_equal(ans2$status, 0)

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  res2 <- task_result(t, ctx)
  expect_true(process_id() != res2$host)
  expect_equal(length(res2$workers), 2)
  expect_false(res2$host %in% res2$workers)
})


test_that("failure on startup", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile()
  tmp <- basename(tempfile("myfuns_", fileext = ".R"))
  file.copy("myfuns.R", tmp)
  ctx <- context::context_save(path, sources = tmp)

  ctx <- context::context_load(ctx, new.env(parent = .GlobalEnv))
  file.remove(tmp)

  ## Now we're ready to see how the scripts do
  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  ans <- task_run_callr(path, t, fail_on_status = FALSE)
  expect_equal(ans$status, 1)

  expect_equal(task_status(t, ctx), TASK_ERROR)
  e <- task_result(t, ctx)
  expect_is(e, "context_task_error")
  expect_is(e$trace, "character")
  expect_equal(e$trace[[1]], "context:::main_task_run()")
  expect_is(e$warnings, "list")
  expect_true(length(e$warnings) >= 1)
})


test_that("load packages", {
  ## this is failing due to (I think) crayon issues
  skip_on_os("windows")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  packages <- c("knitr", "rmarkdown")
  ctx <- context_save(path, packages = packages)
  expr <- quote(sessionInfo())
  t <- task_save(expr, ctx)

  res <- task_run_callr(path, t)

  ans <- parse_context_log(strsplit(res$stderr, "\\n")[[1]])
  i <- which(ans$title == "library")
  expect_equal(length(i), 1)
  expect_equal(trimws(ans$value[[i]]), paste(packages, collapse = ", "))

  i <- which(ans$title == "namespace")
  expect_equal(length(i), 1)
  expect_equal(trimws(ans$value[[i]]), "")

  info <- task_result(t, path)
  expect_true(all(packages %in% names(info$otherPkgs)))
})


test_that("load namespaces", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  packages <- c("knitr", "rmarkdown")
  ctx <- context_save(path, packages = list(loaded = packages))
  expr <- quote(sessionInfo())
  t <- task_save(expr, ctx)

  res <- task_run_callr(path, t)

  ans <- parse_context_log(strsplit(res$stderr, "\\n")[[1]])

  i <- which(ans$title == "library")
  expect_equal(length(i), 1)
  expect_equal(trimws(ans$value[[i]]), "")

  i <- which(ans$title == "namespace")
  expect_equal(length(i), 1)
  expect_equal(trimws(ans$value[[i]]), paste(packages, collapse = ", "))

  info <- task_result(t, path)
  expect_true(all(packages %in% names(info$loadedOnly)))
})
