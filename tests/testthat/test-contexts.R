context("contexts")

## This needs quite bit more testing, but this will take a while to
## work up; the underlying code is largely drawn from well tested
## packages, but because this affects the global search path (and
## future versions will install packages) this becomes very difficult
## to work with R's `R CMD check` tools.
test_that("simplest case", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))
  handle <- save_context(root=root)
  expect_is(handle, "context_handle")

  expect_is(handle$id, "character")
  expect_equal(nchar(handle$id), 32)
  expect_identical(handle$root, root)

  expect_true(file.exists(file.path(root, "contexts")))
  expect_true(file.exists(file.path(root, "contexts", handle$id)))

  e <- new.env()
  res <- load_context(handle, envir=e)
  expect_identical(names(e), character(0))
  expect_identical(names(res), "root")

  obj <- read_context(handle)
  expect_is(obj, "context")
})

test_that("auto", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  handle <- save_context(root=root, auto=TRUE)
  expect_true(file.exists(file.path(root, "contexts", handle$id)))
  expect_true(file.exists(file.path(root, "environments")))

  dat <- readRDS(file.path(root, "contexts", handle$id))
  expect_true(file.exists(file.path(root, "environments", dat$global)))
  expect_true(file.exists(file.path(root, "environments", dat$local)))
})

test_that("package_sources", {
  Sys.setenv(R_TESTS="")
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  src <- package_sources(github="richfitz/sowsear")
  handle <- save_context(root=root, packages="sowsear",
                         package_sources=src)

  obj <- read_context(handle)
  expect_true(obj$package_sources$use_local_drat)
  expect_equal(obj$package_sources$local_drat, path_drat(handle$root))
  expect_equal(obj$packages, list(attached="sowsear", loaded=character(0)))

  tmp <- load_context(handle, quiet=TRUE)
  on.exit(unloadNamespace("sowsear"), add=TRUE)
  expect_true("sowsear" %in% .packages())
})
