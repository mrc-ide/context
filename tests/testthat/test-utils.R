context("utils")

test_that("capture_log", {
  filename <- tempfile()
  expect_message(capture_log(message("foo"), NULL), "foo")

  capture_log(message("foo"), filename)
  expect_true(file.exists(filename))
  ## This is because of test_that's message muffling; that's
  ## notoriously version dependent unfortunately.
  ##   expect_identical(readLines(filename), "foo")
  ## In comparison see
  ##   local({
  ##     filename <- tempfile()
  ##     capture_log(message("foo"), filename)
  ##     readLines(filename)
  ##   })
  f <- function() {
    cat("foo\n")
    1
  }
  expect_equal(capture_log(f(), filename), 1)
  expect_identical(readLines(filename), "foo")
})

test_that("absolute paths", {
  expect_true(is_absolute_path("/foo/bar"))
  expect_true(is_absolute_path("//network/bar"))
  expect_true(is_absolute_path("\\\\network/bar"))
  expect_true(is_absolute_path("c:/foo/bar"))

  expect_false(is_absolute_path("."))
  expect_false(is_absolute_path("foo/bar"))

  expect_false(is_relative_path("/foo/bar"))
})

test_that("is_directory", {
  expect_true(is_directory("."))
  expect_false(is_directory("noisy.R"))
  expect_false(is_directory(tempfile()))
})
