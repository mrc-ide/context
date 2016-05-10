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
  expect_true(db$driver$exists_object(handle$id))

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
  expect_true(db$driver$exists_object(handle$id))

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

test_that("context_db", {
  ctx <- context_save(tempfile())
  db <- ctx$db
  db$set_by_value(runif(10))
  ok <- function(x) {
    inherits(x, "storr") &&
      x$driver$type() == "rds" &&
      identical(x$driver$path, db$driver$path) &&
      identical(x$list_hashes(), db$list_hashes()) &&
      identical(x$list(), db$list())
  }

  expect_true(ok(context_db(ctx)))
  expect_true(ok(context_db(ctx$root)))
  expect_true(ok(context_db(ctx$db)))
  expect_true(ok(context_db(list(root=ctx$root, db=ctx$db))))
  expect_true(ok(context_db(list(root=ctx$root))))
  expect_true(ok(context_db(list(db=ctx$db))))
  expect_error(ok(context_db(character(0))), "Invalid input")
  expect_error(ok(context_db(c(ctx$root, ctx$root))), "Invalid input")
  expect_error(ok(context_db(1L)), "Invalid input")
})

test_that("environment backed context", {
  ctx <- context_save(tempfile(), storage_type="environment")
  db <- context_db(ctx)
  expect_is(db, "storr")
  expect_equal(db$driver$type(), "environment")
  expect_is(context_handle(ctx$root, ctx$id, db), "context_handle")
  expect_error(context_db(context_handle(ctx$root, ctx$id)),
               "Cannot reconnect to environment storage")
})

test_that("list empty context", {
  root <- tempfile()

  expect_error(contexts_list(root),
               "context database not set up at")
  expect_null(contexts_list(root, error=FALSE))

  ctx1 <- context_save(root, auto=TRUE)
  expect_equal(contexts_list(root), ctx1$id)
  expect_equal(context_handle(root)$id, ctx1$id)

  Sys.sleep(.1)
  ctx2 <- context_save(root, auto=TRUE)
  expect_equal(sort(contexts_list(root)), sort(c(ctx1$id, ctx2$id)))
  expect_equal(context_handle(root)$id, ctx2$id)
})

test_that("args", {
  ctx <- context_save(tempfile(), storage_args=list(compress=TRUE))
  expect_true(context_db(ctx)$driver$compress)

  ctx <- context_save(tempfile(), storage_args=list(compress=FALSE))
  expect_false(context_db(ctx)$driver$compress)
})
