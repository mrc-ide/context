context("contexts")

## This needs quite bit more testing, but this will take a while to
## work up; the underlying code is largely drawn from well tested
## packages, but because this affects the global search path (and
## future versions will install packages) this becomes very difficult
## to work with R's `R CMD check` tools.
test_that("simplest case", {
  Sys.setenv(R_TESTS="")
  root <- tempfile("cluster_")
  on.exit(cleanup(root))
  handle <- context_save(root=root)
  expect_is(handle, "context_handle")

  expect_is(handle$id, "character")
  expect_equal(nchar(handle$id), 32)
  expect_identical(handle$root, root)
  expect_is(handle$db, "storr")

  db <- context_db(handle)
  expect_true(db$exists(handle$id, "contexts"))
  ## TODO: should be in storr
  expect_true(db$driver$exists_hash(handle$id))

  e <- new.env()
  res <- context_load(handle, envir=e)
  expect_identical(names(e), character(0))
  expect_false(identical(res, environment()))
  expect_identical(names(res), "root")

  obj <- context_read(handle)
  expect_is(obj, "context")
})

test_that("auto", {
  Sys.setenv(R_TESTS="")
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  handle <- context_save(root=root, auto=TRUE)
  db <- context_db(handle)
  expect_true(db$exists(handle$id, "contexts"))
  expect_true(db$driver$exists_hash(handle$id))

  obj <- context_read(handle)
  expect_is(obj$local, "environment")
  expect_is(obj$global, "raw")
})

test_that("package_sources", {
  Sys.setenv(R_TESTS="")
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  src <- package_sources(github="richfitz/kitten")
  handle <- context_save(root=root, packages="kitten",
                         package_sources=src)

  obj <- context_read(handle)
  expect_equal(obj$package_sources$local_drat, path_drat(handle$root))
  expect_equal(obj$packages, list(attached="kitten", loaded=character(0)))

  tmp <- context_load(handle, quiet=TRUE)
  on.exit(unloadNamespace("kitten"), add=TRUE)
  expect_true("kitten" %in% .packages())
})

test_that("source files", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))
  src <- c("example-foo.R", "example-bar.R")
  expect_error(context_save(root=root, sources=src),
               "files do not exist")
  expect_error(context_save(root=root, sources="../testthat.R"),
               "files above working directory")
  expect_error(context_save(root=root, sources=normalizePath("../testthat.R")),
               "files above working directory")

  writeLines(character(0), src[[1]])
  writeLines(character(0), src[[2]])
  on.exit(file.remove(src), add=TRUE)

  ctx <- context_save(root=root, sources=src)
  dat <- context_read(ctx)
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
  dat <- serialise_image()

  e <- new.env()
  v <- deserialise_image(dat, envir=e)
  expect_equal(sort(v), sort(c("t", vars)))
  expect_equal(e$t, 1)

  ## But this *should* do badly in the buggy version I'm looking at
  ## but does not seem to right now:
  f <- function() {
    serialise_image()
  }
  environment(f) <- environment(serialise_image)
  dat2 <- f()
  expect_equal(dat2, dat)

  ctx <- context_save(auto=TRUE, root=path)
  expect_equal(context_read(ctx)$global, dat)
})

test_that("storage type", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))
  expect_error(context_save(root=root, storage_type="redis"),
               "Unsupported storage type")
  expect_false(file.exists(path_config(root)))
  handle <- context_save(root=root, storage_type="rds")
  expect_error(context_save(root=root, storage_type="redis"),
               "Incompatible storage types")
})
