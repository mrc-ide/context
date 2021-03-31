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

test_that("use_local_library", {
  lp <- .libPaths()
  lu <- Sys.getenv("R_LIBS_USER")
  on.exit({
    .libPaths(lp)
    if (nzchar(lu)) {
      Sys.setenv(R_LIBS_USER = lu)
    } else {
      Sys.unsetenv("R_LIBS_USER")
    }
  })

  path <- tempfile()
  expect_warning(use_local_library(path),
                 "library not found at")
  expect_false(file.exists(path))
  expect_identical(.libPaths(), lp)
  expect_identical(Sys.getenv("R_LIBS_USER"), lu)

  dir.create(path)
  use_local_library(path)
  expect_true(file.exists(path))

  expect_identical(normalizePath(.libPaths()),
                   normalizePath(c(path, lp)))
  expect_identical(Sys.getenv("R_LIBS_USER"), path)
})

test_that("bootstrap_context", {
  skip("Can't mock base any more")
  skip("Also mostly obsolete")

  require_namespace_no <- function(package, ...) {
    FALSE
  }
  require_namespace_yes <- function(package, ...) {
    TRUE
  }
  ull_path <- NULL
  ull <- function(path) {
    ull_path <<- path
  }

  path <- tempfile()

  with_mock(
    `context:::use_local_library` = ull,
    `base:::requireNamespace` = require_namespace_no,
    expect_error(bootstrap_context(path),
                 "Could not find context package"),
    expect_null(ull_path),
    expect_true(getOption("context.log")))

  Sys.setenv("CONTEXT_QUIET" = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_QUIET"), add = TRUE)
  with_mock(
    `context:::use_local_library` = ull,
    `base:::requireNamespace` = require_namespace_no,
    expect_error(bootstrap_context(path),
                 "Could not find context package"),
    ## expect_equal(ull_path, path_library(path)),
    expect_null(getOption("context.log")))

  Sys.setenv("CONTEXT_BOOTSTRAP" = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_BOOTSTRAP"), add = TRUE)
  with_mock(
    `context:::use_local_library` = ull,
    `base:::requireNamespace` = require_namespace_no,
    expect_error(bootstrap_context(path),
                 "Could not find context package"),
    expect_equal(ull_path, path_library(path)),
    expect_null(getOption("context.log")))

  Sys.setenv("CONTEXT_BOOTSTRAP" = "TRUE")
  on.exit(Sys.unsetenv("CONTEXT_BOOTSTRAP"), add = TRUE)
  ull_path <- NULL
  with_mock(
    `context:::use_local_library` = ull,
    `base:::requireNamespace` = require_namespace_yes,
    expect_error(bootstrap_context(path), NA),
    expect_equal(ull_path, path_library(path)),
    expect_null(getOption("context.log")))
})
