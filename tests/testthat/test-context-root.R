context("context_root")

test_that("basic", {
  root <- tempfile()
  ans <- context_root_init(root, NULL, NULL)
  expect_is(ans, "context_root")
  expect_true(file.exists(ans$path))
  expect_true(is_directory(ans$path))
  expect_is(ans$db, "storr")
  expect_equal(ans$db$driver$type(), "rds")
})
