context("script support")

test_that("parse_command_args erorrs", {
  expect_error(parse_command_args(character(), "foo", 0L),
               "At least 1 argument required")
  expect_error(parse_command_args(character(), "foo", 1L),
               "At least 2 arguments required")
  expect_error(parse_command_args(character(), "foo", 1L),
               "Usage: foo <root>", fixed = TRUE)

  expect_error(parse_command_args(c("a", "b", "c"), "foo", c(1L, 1L)),
               "At most 2 arguments allowed")
})

test_that("parse_command_args, no args", {
  res <- parse_command_args("aaa", "foo", 0L)
  expect_equal(res$n, 0L)
  expect_equal(res$root, "aaa")
  expect_equal(res$args, character(0))
})

test_that("parse_command_args, one args", {
  res <- parse_command_args(c("aaa", "bbb"), "foo", 1L)
  expect_equal(res$n, 1L)
  expect_equal(res$root, "aaa")
  expect_equal(res$args, "bbb")
})

test_that("parse_command_args, one args", {
  res <- parse_command_args(c("aaa", "bbb", "ccc"), "foo", 2L)
  expect_equal(res$n, 2L)
  expect_equal(res$root, "aaa")
  expect_equal(res$args, c("bbb", "ccc"))
})