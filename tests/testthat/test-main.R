context("main (script)")

test_that("parse_args", {
  expect_error(main_parse_args(character(0)),
               "Exactly two arguments required")
  expect_error(main_parse_args("a"),
               "Exactly two arguments required")
  expect_error(main_parse_args(c("a", "b", "c")),
               "Exactly two arguments required")
  expect_that(main_parse_args(c("foo", "bar")),
              equals(list(id="foo", root="bar")))
})

test_that("install", {
  full <- install_context(tempdir())
  expect_true(file.exists(full))
  expect_equal(as.character(file.info(full)[["mode"]]), "755")
})

test_that("run", {
  handle <- save_task(quote(sin(1)), root=tempfile("cluster_"))
  full <- install_context(tempdir())

  Sys.setenv(R_TESTS="")
  ## can use call_system here as I set it up to work.  Unfortunately
  ## that does rather create a potential circular dependency if I ever
  ## wanted to get callr to use context.

  res <- system2(full, c(handle$id, handle$root), stdout=TRUE, stderr=TRUE)
  expect_null(attr(res, "status", exact=TRUE))

  result_file <- path_results(handle$root, handle$id)
  expect_true(file.exists(result_file))
  expect_equal(readRDS(result_file), sin(1))
})

test_that("install", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  ## Picking this package just because it's fairly light and unlikely
  ## to be installed.
  src <- package_sources(github="richfitz/kitten")
  context <- save_context(packages="kitten", package_sources=src, root=root)
  handle <- save_task(quote(sin(1)),
                      context=context,
                      root=root)
  full <- install_context(tempdir())

  res <- system2(full, c(handle$id, handle$root), stdout=TRUE, stderr=TRUE)
  expect_null(attr(res, "status", exact=TRUE))

  result_file <- path_results(handle$root, handle$id)
  expect_true(file.exists(result_file))
  expect_equal(readRDS(result_file), sin(1))
})
