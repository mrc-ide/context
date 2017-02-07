context("context_root")

test_that("basic", {
  path <- tempfile()
  ans <- context_root_init(path, NULL, NULL)
  expect_is(ans, "context_root")
  expect_true(file.exists(ans$path))
  expect_true(is_directory(ans$path))
  expect_is(ans$db, "storr")
  expect_equal(ans$db$driver$type(), "rds")

  expect_true(file.exists(file.path(path_bin(path), "task_run")))
})

test_that("version conflict", {
  path <- tempfile()
  ans <- context_root_init(path, NULL, NULL)
  writeLines("100.0.1", path_version(path))

  expect_error(context_root_init(path, NULL, NULL),
               "context version conflict; local is outdated")
})

test_that("context_root_get", {
  path <- tempfile()
  ans <- context_root_init(path, NULL, NULL)

  expect_is(context_root_get(path), "context_root")
  expect_is(context_root_get(ans), "context_root")
  expect_is(context_root_get(list(root = ans)), "context_root")
  expect_is(context_root_get(list2env(list(root = ans))), "context_root")

  expect_error(context_root_get(NULL), "Invalid context root")
  expect_error(context_root_get(list(path = path)), "Invalid context root")
})

test_that("storage_args", {
  root <- context_root_init(tempfile(), storage_args = list(compress = TRUE))
  expect_true(root$db$driver$compress)
  root <- context_root_init(tempfile(), storage_args = list(compress = FALSE))
  expect_false(root$db$driver$compress)
})

test_that("args override", {
  path <- tempfile()
  root1 <- context_root_init(path)
  expect_false(root1$db$driver$compress)
  expect_warning(
    root2 <- context_root_init(path, storage_args = list(compress = TRUE)),
    "Ignoring incompatible storage_args")
  expect_false(root2$db$driver$compress)
})

test_that("partial storage conflict", {
  args1 <- list(compress = TRUE, hash_algorithm = "sha1")
  args2 <- list(compress = FALSE, hash_algorithm = "sha1")
  root <- context_root_init(tempfile(), storage_args = args1)
  expect_warning(context_root_init(root$path, storage_args = args2),
                 "Ignoring incompatible storage_args")
})

test_that("print", {
  path <- tempfile()
  root <- context_root_init(path)
  on.exit(unlink(path, recursive = TRUE))
  expect_output(print(root), "<context_root>", fixed = TRUE)
})

test_that("library path", {
  path <- tempfile()
  expect_equal(path_library(path),
               file.path(path, "lib", r_platform_name(), r_version(2)))
  expect_equal(path_library(path, "windows"),
               file.path(path, "lib", "windows", r_version(2)))
  expect_equal(path_library(path, version = "1.2.3"),
               file.path(path, "lib", r_platform_name(), "1.2"))
})
