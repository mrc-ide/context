context("expression")

test_that("missing variables", {
  e <- new.env(parent = emptyenv())
  db <- storr::storr_environment()
  expect_error(prepare_expression(quote(list(x, y, z)), e, db),
               "not all objects found: x, y, z")
  e$y <- 1
  expect_error(prepare_expression(quote(list(x, y, z)), e, db),
               "not all objects found: x, z")
  e$x <- 1
  expect_error(prepare_expression(quote(list(x, y, z)), e, db),
               "not all objects found: z")
})

test_that("function values", {
  e <- new.env(parent = emptyenv())
  db <- storr::storr_environment()
  e$a <- 1
  dat <- prepare_expression(quote(NULL(a, 2)), e, db, function(a, b) a + b)

  expect_is(dat$function_hash, "character")
  h <- unname(dat$function_hash)
  expect_equal(names(dat$function_hash), h)
  expect_equal(dat$expr[[1]], as.name(h))

  expect_is(dat$objects, "character")

  e2 <- restore_locals(dat, new.env(parent = .GlobalEnv), db)
  expect_equal(sort(names(e2)), sort(c("a", h)))
  expect_is(e2[[h]], "function")

  expect_equal(eval(dat$expr, e2), 3)
})

test_that("find_symbols", {
  expect_equal(find_symbols(quote(foo(x, ids::random_id))), "x")
  expect_equal(find_symbols(quote(foo(x, ids::random_id()))), "x")
  expect_equal(find_symbols(quote(foo(x, ids::random_id(a, b)))),
               c("x", "a", "b"))
})

test_that("namespace qualified functions as arguments", {
  db <- storr::storr_environment()
  e <- new.env(parent = environment())
  e$x <- pi
  expr <- quote(foo(x, ids::random_id))

  dat <- prepare_expression(expr, e, db)
  expect_equal(dat$expr, expr)
  expect_equal(dat$objects, c(x = db$hash_object(e$x)))
})
