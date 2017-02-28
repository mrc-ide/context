context("script support")

test_that("parse_context_args erorrs", {
  expect_error(parse_context_args(character(), "foo", 0L),
               "At least 1 argument required")
  expect_error(parse_context_args(character(), "foo", 1L),
               "At least 2 arguments required")
  expect_error(parse_context_args(character(), "foo", 1L),
               "Usage: foo <root>", fixed = TRUE)

  expect_error(parse_context_args(c("a", "b", "c"), "foo", c(1L, 1L)),
               "At most 2 arguments allowed")
})

test_that("parse_context_args, no args", {
  res <- parse_context_args("aaa", "foo", 0L)
  expect_equal(res$n, 0L)
  expect_equal(res$root, "aaa")
  expect_equal(res$args, character(0))
})

test_that("parse_context_args, one args", {
  res <- parse_context_args(c("aaa", "bbb"), "foo", 1L)
  expect_equal(res$n, 1L)
  expect_equal(res$root, "aaa")
  expect_equal(res$args, "bbb")
})

test_that("parse_context_args, one args", {
  res <- parse_context_args(c("aaa", "bbb", "ccc"), "foo", 2L)
  expect_equal(res$n, 2L)
  expect_equal(res$root, "aaa")
  expect_equal(res$args, c("bbb", "ccc"))
})

test_that("write_context_script", {
  path <- tempfile()
  name <- "foo"
  target <- "package:::entrypoint"
  nargs <- 1:2
  full <- write_context_script(path, name, target, nargs)
  full <- file.path(path, "bin", name)
  expect_true(file.exists(file.path(path, "bin", name)))
  expect_equal(tail(readLines(full), 1), paste0(target, "()"))

  md5 <- tools::md5sum(full)
  write_context_script(path, name, "junk", nargs)
  ## Unchanged:
  expect_equal(md5, tools::md5sum(full))
  expect_equal(tail(readLines(full), 1), paste0(target, "()"))
})

test_that("write_context_script, nargs", {
  path <- tempfile()
  name <- "foo"
  target <- "package:::entrypoint"
  nargs <- 1:2
  expect_error(write_context_script(path, name, target, 1:3),
               "Invalid input for 'nargs'")
})
