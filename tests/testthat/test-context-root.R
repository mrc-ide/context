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

test_that("driver packages", {
  storage_driver_rds2_create <- function(path, id, args) {
    .GlobalEnv$.test <- c(.GlobalEnv$.test, ids::random_id())
    path_db <- file.path(path, "db")
    storr::storr_rds(path_db, compress = FALSE, mangle_key = FALSE)
  }
  environment(storage_driver_rds2_create) <- .GlobalEnv
  storage_driver_rds2 <- storage_driver("rds2", storage_driver_rds2_create,
                                        "ids")
  .GlobalEnv$.test <- NULL

  path <- tempfile("cluster_")
  on.exit({
    unlink(path, recursive = TRUE)
    rm(".test", .GlobalEnv)
  })

  on.exit(cleanup(path))
  ctx <- context_save(path,
                      packages = list(attached = "ape", loaded = "storr"),
                      storage_type = storage_driver_rds2)

  expect_equal(ctx$db$get("driver_packages", "context_root"), "ids")
  expect_equal(ctx$packages$attached, "ape")
  expect_equal(ctx$packages$loaded, c("storr", "ids"))

  id <- .GlobalEnv$.test
  expect_equal(length(id), 1)
  expect_true(is_id(id))

  tmp <- context_root_get(path)
  id2 <- .GlobalEnv$.test
  expect_equal(length(id2), 2)
  expect_true(is_id(id2[[2]]))
})

test_that("create context root with fixed id", {
  id <- ids::random_id()
  path <- tempfile()

  res <- context_root_init(path, NULL, NULL, id)
  expect_equal(res$id, id)
  expect_equal(res$path, path)

  res <- context_root_init(path, NULL, NULL, id)
  expect_equal(res$id, id)
  expect_equal(res$path, path)
})

test_that("verify id when creating context root with fixed id", {
  path <- tempfile()
  res <- context_root_init(path, NULL, NULL)
  expect_error(
    context_root_init(path, NULL, NULL, ids::random_id()),
    "Given id '[[:xdigit:]]+' and stored id '[[:xdigit:]]+' differ")
})

test_that("verify id format", {
  expect_error(
    context_root_init(tempfile(), NULL, NULL, ids::random_id(bytes = 4)),
    "id, if given, must be a 32 character hex string")
})
