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
