context("logging")

test_that("logging", {
  expect_message(context_log("topic", "value"),
                 "[ topic     ]  value", fixed = TRUE)
  expect_message(context_log("topic", c("a", "b")),
                 "[ topic     ]  a\n[ ...       ]  b", fixed = TRUE)
})


test_that("parse and print context log", {
  msg <- capture_messages({
    context_log("topic1", "value1")
    message("Some message")
    context_log("topic2", "value2")
  })
  obj <- parse_context_log(trimws(msg))
  expect_s3_class(obj, "context_log")

  str_pretty <- withr::with_options(
    list(crayon.enabled = TRUE),
    pretty_context_log(obj))
  expect_true(all(crayon::has_style(str_pretty$str)))
  str_plain <- withr::with_options(
    list(crayon.enabled = FALSE),
    pretty_context_log(obj))
  expect_false(any(crayon::has_style(str_plain$str)))

  withr::with_options(
    list(crayon.enabled = TRUE),
    expect_true(any(crayon::has_style(capture.output(print(obj))))))
  withr::with_options(
    list(crayon.enabled = FALSE),
    expect_false(any(crayon::has_style(capture.output(print(obj))))))
  withr::with_options(
    list(crayon.enabled = TRUE),
    expect_false(any(crayon::has_style(capture.output(print(obj, FALSE))))))
})


test_that("parse failed log", {
  c('Error in bootstrap_context(name = "task_run", n = 1L) : ',
  "'  Could not find context package; aborting startup",
  "Calls: local ... eval.parent -> eval -> eval -> eval -> eval -> bootstrap_context",
  "Execution halted") -> str
  dat <- parse_context_log(str)
  expect_equal(dat$str, "<top level error>", fixed = TRUE)
  expect_output(print(dat), "Error in bootstrap_context")
})
