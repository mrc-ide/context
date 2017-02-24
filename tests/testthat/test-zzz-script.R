context("script")

test_that("basic", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path)
  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  full <- file.path(path_bin(path), "task_run")
  res <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status", exact = TRUE))

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  expect_equal(task_result(t, ctx), eval(expr))
})

test_that("error", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns.R")
  t1 <- task_save(quote(g(-1)), ctx)
  t2 <- task_save(quote(g(-1)), ctx)

  full <- file.path(path_bin(path), "task_run")

  res <- Rscript(c(full, path, t1), stdout = TRUE, stderr = TRUE)
  expect_null(attr(res, "status", exact = TRUE))
  expect_equal(task_status(t1, ctx), TASK_ERROR)

  Sys.setenv("CONTEXT_PROPAGATE_ERROR" = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_PROPAGATE_ERROR"), add = TRUE)
  res <- suppressWarnings(
    Rscript(c(full, path, t2), stdout = TRUE, stderr = TRUE))
  expect_gt(attr(res, "status", exact = TRUE), 0)
  expect_equal(task_status(t2, ctx), TASK_ERROR)

  r1 <- task_result(t1, ctx)
  r2 <- task_result(t2, ctx)
  expect_equal(r1, r2)
  expect_is(r1, "context_task_error")
  expect_equal(r1$trace[[1]], "context:::main_task_run()")
})

test_that("parallel", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, sources = "myfuns-parallel.R")
  t <- task_save(quote(get_cluster_pids()), ctx)

  full <- file.path(path_bin(path), "task_run")
  log1 <- suppressWarnings(
    Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE))
  expect_equal(task_status(t, ctx), TASK_ERROR)

  res1 <- task_result(t, ctx)
  expect_is(res1, "context_task_error")
  expect_match(res1$message, "registered")
  expect_true(any(grepl("defaultCluster", res1$trace)))

  Sys.setenv("CONTEXT_CORES" = 2L)
  on.exit(Sys.unsetenv("CONTEXT_CORES"), add = TRUE)
  log2 <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)

  expect_equal(task_status(t, ctx), TASK_COMPLETE)
  res2 <- task_result(t, ctx)
  expect_true(process_id() != res2$host)
  expect_equal(length(res2$workers), 2)
  expect_false(res2$host %in% res2$workers)
})

## This tests the issue that Ilaria found; when a package uses
## startCluster(), it should find the set of packages that we want it
## to find (so that if this is running on a provisioned context, then
## the cross installed packages are all found)
test_that("manual parallel cluster", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  Sys.setenv(CONTEXT_BOOTSTRAP = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_BOOTSTRAP"), add = TRUE)

  ctx <- context_save(path, sources = "myfuns-parallel.R")
  t <- task_save(quote(manual_parallel_test()), ctx)

  full <- file.path(path_bin(path), "task_run")
  res1 <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)
  paths1 <- task_result(t, ctx)

  ## Then, we create the directory that would the the library path:
  dir.create(path_library(path), recursive = TRUE)

  res2 <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)
  paths2 <- task_result(t, ctx)

  ## This is not ideal, but under devtools::test() (but not under
  ## running tests interactively) R_LIBS is set and changes the order
  ## of the paths here.  So we test that _any_ of the library paths
  ## are consistent with the library path having been set when we
  ## really should test for the first one.
  path_norm <- normalizePath(path)
  expect_false(any(string_starts_with(paths1[[1]], path_norm)))
  expect_true(any(string_starts_with(paths2[[1]], path_norm)))

  if (!nzchar(Sys.getenv("R_LIBS"))) {
    expect_false(string_starts_with(paths1[[1]][[1]], path_norm))
    expect_true(string_starts_with(paths2[[1]][[1]], path_norm))
  }
})

test_that("bootstrap", {
  skip_if_not_installed("provisionr")
  path <- tempfile()

  ctx <- context_save(path)
  provision_context(ctx, quiet = TRUE)

  t1 <- task_save(quote(sin(1)), ctx)
  t2 <- task_save(quote(sin(2)), ctx)
  t3 <- task_save(quote(sin(3)), ctx)

  ## Need to provision this context.
  full <- file.path(path_bin(path), "task_run")
  res <- Rscript(c(full, path, t1), stdout = TRUE, stderr = TRUE)
  log <- parse_context_log(res)

  expect_false(any(log$title == "bootstrap"))
  expect_false(any(log$title == "lib"))
  expect_equal(task_result(t1, ctx), sin(1))

  Sys.setenv(CONTEXT_BOOTSTRAP = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_BOOTSTRAP"), add = TRUE)

  res <- Rscript(c(full, path, t2), stdout = TRUE, stderr = TRUE)
  log <- parse_context_log(res)

  expect_true(any(log$title == "bootstrap"))
  expect_true(any(log$title == "lib"))
  expect_equal(trimws(log$value[which(log$title == "lib")]),
               path_library(path))
  expect_equal(task_result(t2, ctx), sin(2))

  unlink(path_library(path), recursive = TRUE)
  res <- Rscript(c(full, path, t3), stdout = TRUE, stderr = TRUE)
  log <- parse_context_log(res)

  i <- which(log$title == "lib")
  expect_match(log$value[[i]], "library not found at")
  expect_match(log$body[[i]], "library not found at", all = FALSE)
})

test_that("provision - source github", {
  skip_if_not_installed("provisionr")
  path <- tempfile()
  src <- provisionr::package_sources(github = "richfitz/kitten")
  ctx <- context_save(path, packages = "kitten", package_sources = src)
  res <- provision_context(ctx, quiet = TRUE)
  expect_true(file.exists(file.path(path, "drat")))
  expect_equal(provisionr:::drat_storr(file.path(path, "drat"))$list(),
               "github::richfitz/kitten")
  expect_true("kitten" %in% dir(path_library(path)))
})

test_that("provision - binary github", {
  skip_if_not_installed("provisionr")
  path <- tempfile()
  src <- provisionr::package_sources(github = "richfitz/seagull")
  ctx <- context_save(path, packages = "seagull", package_sources = src)
  res <- provision_context(ctx, "windows", quiet = TRUE, allow_missing = TRUE)

  expect_true(file.exists(file.path(path, "drat")))
  expect_equal(provisionr:::drat_storr(file.path(path, "drat"))$list(),
               "github::richfitz/seagull")

  expect_true("context" %in% dir(path_library(path, "windows")))
  expect_false("seagull" %in% dir(path_library(path, "windows")))

  m <- res$missing
  full <- file.path(path, "drat", "src", "contrib",
                    sprintf("%s_%s.tar.gz", m[, "Package"], m[, "Version"]))
  expect_true(file.exists(full))
})

test_that("re-provision drat", {
  skip_if_not_installed("provisionr")
  path <- tempfile()
  src <- provisionr::package_sources(github = "richfitz/kitten")
  ctx <- context_save(path, packages = "kitten", package_sources = src)
  res <- provision_context(ctx, quiet = TRUE)

  expect_true(file.exists(file.path(path, "drat")))
  expect_equal(provisionr:::drat_storr(file.path(path, "drat"))$list(),
               "github::richfitz/kitten")
  expect_true("kitten" %in% dir(path_library(path)))

  expect_silent(res2 <- provision_context(ctx, quiet = TRUE))
  expect_null(res2)

  ## The src element is _not_ updated with the new location
  expect_null(src$local_drat)
  expect_true(src$needs_build())
})

test_that("provision with additional library", {
  skip_if_not_installed("provisionr")
  path <- tempfile()
  altlib <- tempfile()
  on.exit(unlink(c(path, altlib), recursive = TRUE))
  provisionr::provision_library("ids", altlib, "windows")

  ctx <- context_save(path)
  res <- provision_context(ctx, "windows", additional_libraries = altlib)

  expect_true("context" %in% dir(path_library(path, "windows")))
  expect_false("ids" %in% dir(path_library(path, "windows")))
  expect_true("ids" %in% dir(altlib))
})

test_that("failure on startup", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile()
  tmp <- basename(tempfile("myfuns_", fileext = ".R"))
  file.copy("myfuns.R", tmp)
  ctx <- context::context_save(path, sources = tmp,
                               unique_value = ids::random_id())

  ctx <- context::context_load(ctx, new.env(parent = .GlobalEnv))
  file.remove(tmp)

  ## Now we're ready to see how the scripts do
  expr <- quote(sin(1))
  t <- task_save(expr, ctx)

  full <- file.path(path_bin(path), "task_run")
  res <- suppressWarnings(
    Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE))
  expect_gt(attr(res, "status", exact = TRUE), 0)

  expect_equal(task_status(t, ctx), TASK_ERROR)
  e <- task_result(t, ctx)
  expect_is(e, "context_task_error")
  expect_is(e$trace, "character")
  expect_equal(e$trace[[1]], "context:::main_task_run()")
  expect_is(e$warnings, "list")
  expect_true(length(e$warnings) >= 1)
})

test_that("load packages", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  packages <- c("knitr", "rmarkdown")
  ctx <- context_save(path, packages = packages)
  expr <- quote(sessionInfo())
  t <- task_save(expr, ctx)
  full <- file.path(path_bin(path), "task_run")
  res <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)

  ans <- parse_context_log(res)
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
  full <- file.path(path_bin(path), "task_run")
  res <- Rscript(c(full, path, t), stdout = TRUE, stderr = TRUE)

  ans <- parse_context_log(res)

  i <- which(ans$title == "library")
  expect_equal(length(i), 1)
  expect_equal(trimws(ans$value[[i]]), "")

  i <- which(ans$title == "namespace")
  expect_equal(length(i), 1)
  expect_equal(trimws(ans$value[[i]]), paste(packages, collapse = ", "))

  info <- task_result(t, path)
  expect_true(all(packages %in% names(info$loadedOnly)))
})
