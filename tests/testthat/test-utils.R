context("utils")

test_that("filter_warnings", {
  f <- function(x) {
    warning(x)
  }
  expect_warning(filter_warnings(f("warning"), character(0)), "warning")
  expect_warning(filter_warnings(f("warning"), "pattern"), "warning")
  expect_silent(filter_warnings(f("warning"), "warning"))
  expect_silent(filter_warnings(f("warning"), c("pattern", "warning")))
  expect_silent(filter_warnings(f("warning"), c("warning", "pattern")))
})

test_that("install.packages2", {
  repos <- c(CRAN="http://cran.rstudio.com")
  expect_warning(install.packages("asdfa", repos=repos))
  ## This is super annoying; really should fail:
  expect_null(suppressWarnings(install.packages("asdfa", repos=repos)))

  expect_error(suppressWarnings(install.packages2("asdfa", repos=repos)),
               "is not available")
})

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
})
