context("expression")

test_that("missing variables", {
  e <- new.env(parent = emptyenv())
  db <- storr::storr_environment()
  expect_error(store_expression(quote(list(x, y, z)), e, db),
               "not all objects found: x, y, z")
  e$y <- 1
  expect_error(store_expression(quote(list(x, y, z)), e, db),
               "not all objects found: x, z")
  e$x <- 1
  expect_error(store_expression(quote(list(x, y, z)), e, db),
               "not all objects found: z")
})
