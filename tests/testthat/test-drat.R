context("drat")

test_that("empty local drat", {
  path <- tempfile()
  res <- build_local_drat(NULL, path)
  expect_null(res)
})
