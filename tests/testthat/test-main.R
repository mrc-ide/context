context("main (script)")

test_that("parse_args", {
  expect_error(main_parse_args(character(0)),
               "Exactly two arguments required")
  expect_error(main_parse_args("a"),
               "Exactly two arguments required")
  expect_error(main_parse_args(c("a", "b", "c")),
               "Exactly two arguments required")
  expect_equal(main_parse_args(c("foo", "bar")),
               list(root="foo", id="bar"))
})

test_that("install", {
  full <- install_context(tempdir())
  expect_true(file.exists(full))
  expect_equal(as.character(file.info(full)[["mode"]]), "755")
})

test_that("run", {
  ctx <- context_save(auto=TRUE, root=tempfile("cluster_"))
  handle <- task_save(quote(sin(1)), ctx)
  full <- install_context(tempdir())

  Sys.setenv(R_TESTS="")
  res <- call_system(full, c(handle$root, handle$id))
  expect_null(attr(res, "status", exact=TRUE))

  db <- context_db(handle$root)
  expect_equal(db$get(handle$id, "task_status"), TASK_COMPLETE)
  expect_true(db$exists(handle$id, "task_results"))
  expect_equal(db$get(handle$id, "task_results"), sin(1))
})

test_that("run (locals)", {
  ctx <- context_save(root=tempfile("cluster_"))
  x <- 1:10
  handle <- task_save(quote(sin(x)), ctx)
  full <- install_context(tempdir())

  Sys.setenv(R_TESTS="")
  res <- call_system(full, c(handle$root, handle$id))
  expect_null(attr(res, "status", exact=TRUE))

  db <- context_db(handle$root)
  expect_equal(db$get(handle$id, "task_status"), TASK_COMPLETE)
  expect_true(db$exists(handle$id, "task_results"))
  expect_equal(db$get(handle$id, "task_results"), sin(x))
})

test_that("install", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  ## Picking this package just because it's fairly light and unlikely
  ## to be installed.
  src <- package_sources(github="richfitz/kitten")
  context <- context_save(packages="kitten", package_sources=src, root=root)
  handle <- task_save(quote(sin(1)),
                      context=context)
  full <- install_context(tempdir())

  res <- system2(full, c(handle$root, handle$id), stdout=TRUE, stderr=TRUE)
  expect_null(attr(res, "status", exact=TRUE))

  db <- context_db(handle$root)
  expect_equal(db$get(handle$id, "task_status"), TASK_COMPLETE)
  expect_true(db$exists(handle$id, "task_results"))
  expect_equal(db$get(handle$id, "task_results"), sin(1))
})
