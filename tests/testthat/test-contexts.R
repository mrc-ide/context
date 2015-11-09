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

  src <- package_sources(github="richfitz/kitten")
  handle <- save_context(root=root, packages="kitten",
                         package_sources=src)

  obj <- read_context(handle)
  expect_true(obj$package_sources$use_local_drat)
  expect_equal(obj$package_sources$local_drat, path_drat(handle$root))
  expect_equal(obj$packages, list(attached="kitten", loaded=character(0)))

  tmp <- load_context(handle, quiet=TRUE)
  on.exit(unloadNamespace("kitten"), add=TRUE)
  expect_true("kitten" %in% .packages())
})

test_that("source files", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))
  src <- c("example-foo.R", "example-bar.R")
  expect_error(save_context(root=root, sources=src),
               "files do not exist")
  expect_error(save_context(root=root, sources="../testthat.R"),
               "files above working directory")
  expect_error(save_context(root=root, sources=normalizePath("../testthat.R")),
               "files above working directory")

  writeLines(character(0), src[[1]])
  writeLines(character(0), src[[2]])
  on.exit(file.remove(src), add=TRUE)

  ctx <- save_context(root=root, sources=src)
  dat <- read_context(ctx)
  expect_equal(dat$sources, src)
})

## Issues saving global environments: This does not tickle the problem
## unfortunately.
test_that("globals", {
  path <- tempfile()
  on.exit(cleanup(path))
  vars <- setdiff(names(.GlobalEnv), ".Random.seed")

  .GlobalEnv$t <- 1
  on.exit(rm(list="t", envir=.GlobalEnv), add=TRUE)
  id <- save_image(path)
  expect_true(file.exists(path))
  expect_true(is_dir(path))
  filename <- file.path(path, id)

  e <- new.env()
  v <- load(filename, e)
  expect_equal(sort(v), sort(c("t", vars)))
  expect_equal(e$t, 1)

  ## But this *should* do badly in the buggy version I'm looking at
  ## but does not seem to right now:
  f <- function(path) {
    save_image(path)
  }
  environment(f) <- environment(save_image)
  id2 <- f(path)
  expect_equal(id2, id)

  ctx <- save_context(auto=TRUE, root=path)
  expect_equal(read_context(ctx)$global, id)
})
