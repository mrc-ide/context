context("contexts")

## This needs quite bit more testing, but this will take a while to
## work up; the underlying code is largely drawn from well tested
## packages, but because this affects the global search path (and
## future versions will install packages) this becomes very difficult
## to work with R's `R CMD check` tools.
test_that("simplest case", {
  root <- tempfile("cluster_")
  on.exit(unlink(root, recursive=TRUE))
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
})

test_that("auto", {
  root <- tempfile("cluster_")
  on.exit(unlink(root, recursive=TRUE))

  handle <- save_context(root=root, auto=TRUE)
  expect_true(file.exists(file.path(root, "contexts", handle$id)))
  expect_true(file.exists(file.path(root, "environments")))

  dat <- readRDS(file.path(root, "contexts", handle$id))
  expect_true(file.exists(file.path(root, "environments", dat$global)))
  expect_true(file.exists(file.path(root, "environments", dat$local)))
})
