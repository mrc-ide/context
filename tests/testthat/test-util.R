context("util")

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

test_that("r_version", {
  v <- unclass(getRversion())[[1]]
  expect_equal(r_version(1), numeric_version(v[[1]]),
               check.attributes = FALSE)
  expect_equal(r_version(2), numeric_version(paste(v[1:2], collapse = ".")),
               check.attributes = FALSE)
  expect_equal(r_version(3), numeric_version(paste(v[1:3], collapse = ".")),
               check.attributes = FALSE)
  expect_error(r_version(0), "Invalid n")
  expect_error(r_version(4), "Invalid n")
})

test_that("trim_calls", {
  f1 <- function(x) f2(x)
  f2 <- function(x) f3(x)
  f3 <- function(x) f4(x)
  f4 <- function(x) {
    if (isTRUE(x)) sys.calls() else call_trace()
  }

  calls <- f1(TRUE)
  expect_equal(trim_calls(calls, 0, 0), calls)

  n <- length(calls)
  m <- floor(n / 2)
  expect_equal(trim_calls(calls, n, 0), list())
  expect_equal(trim_calls(calls, n + 1, 0), list())
  expect_equal(trim_calls(calls, 0, n), list())
  expect_equal(trim_calls(calls, 0, n + 1), list())
  expect_equal(trim_calls(calls, m, n - m), list())
  expect_equal(trim_calls(calls, m + 1, n - m), list())

  expect_equal(trim_calls(calls, 1, 0), calls[-1])
  expect_equal(trim_calls(calls, 0, 1), calls[-n])
  expect_equal(trim_calls(calls, 1, 1), calls[-c(1, n)])
})

test_that("print_ad_hoc", {
  expect_output(print_ad_hoc(list(a = raw(4))),
                "a: raw <4 bytes>", fixed = TRUE)
  expect_output(print_ad_hoc(list(a = 1:2)),
                "a: \n   - 1\n   - 2", fixed = TRUE)
  x <- list(a = raw(4))
  capture.output(xx <- print_ad_hoc(x))
  expect_identical(xx, x)
})

test_that("assertions", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
  expect_error(assert_scalar(character(0)), "must be a scalar")

  expect_error(assert_nonmissing(NA), "must not be NA")
  expect_error(assert_nonmissing(NA_real_), "must not be NA")

  expect_error(assert_character(1L), "must be character")
  expect_error(assert_character(pi), "must be character")

  expect_error(assert_is(1, "R6"), "must inherit from R6")
})

test_that("unlist times", {
  t1 <- Sys.time()
  t2 <- t1 + 2
  expect_equal(unlist_times(list(t1)), t1)
  expect_equal(unlist_times(list(t1, t2)), c(t1, t2))
  expect_equal(unlist_times(list()), empty_time())
})

test_that("df_to_list", {
  df <- data.frame(a = 1:5, b = runif(5))
  cmp <- mapply(list, a = df[[1]], b = df[[2]], SIMPLIFY = FALSE)
  expect_equal(df_to_list(df, TRUE), cmp)
  expect_equal(df_to_list(df, FALSE), lapply(cmp, unname))

  rownames(df) <- LETTERS[1:5]
  expect_equal(df_to_list(df, TRUE), setNames(cmp, LETTERS[1:5]))
  expect_equal(df_to_list(df, FALSE),
               setNames(lapply(cmp, unname), LETTERS[1:5]))
})
