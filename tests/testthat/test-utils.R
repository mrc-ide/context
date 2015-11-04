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
