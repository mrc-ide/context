context("contexts")

## This needs quite bit more testing, but this will take a while to
## work up; the underlying code is largely drawn from well tested
## packages, but because this affects the global search path (and
## future versions will install packages) this becomes very difficult
## to work with R's `R CMD check` tools.
test_that("simplest case", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx <- context_save(path, envir = .GlobalEnv)
  expect_is(ctx, "context")

  expect_true(is_id(ctx$id), "character")
  expect_is(ctx$db, "storr")
  expect_identical(ctx$root$path, path)
  expect_identical(context_root_get(ctx)$path, path)

  expect_true(ctx$db$exists(ctx$id, "contexts"))
  expect_true(ctx$db$exists_object(ctx$id))

  res <- context_read(ctx$id, path)
  expect_is(res, "context")
  v <- setdiff(names(ctx), c("db", "root", "local"))
  expect_equal(res[v], ctx[v])
  expect_is(res$db, "storr")
  expect_is(res$root$db, "storr")
  expect_equal(res$root$path, path)

  e <- new.env()
  res <- context_load(ctx, envir = e)
  expect_is(e, "environment")
  if (!identical(environment(), .GlobalEnv)) {
    expect_identical(names(e), character(0))
    expect_false(identical(res, environment()))
  }
})

test_that("load contexts", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, envir = .GlobalEnv)
  expect_is(ctx, "context")

  dat <- context_read(ctx$id, path)
  expect_is(dat, "context")
  v <- c("name", "id", "packages", "auto")
  expect_equal(dat[v], ctx[v])

  dat <- context_read(ctx$name, path)
  expect_is(dat, "context")
  v <- c("name", "id", "packages", "auto")
  expect_equal(dat[v], ctx[v])

  e <- context_load(ctx, envir = new.env())
  expect_is(e, "environment")
  expect_equal(ls(e), character(0))
})

test_that("auto", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, auto = TRUE)
  expect_true(ctx$db$exists(ctx$id, "contexts"))
  expect_true(ctx$db$driver$exists_object(ctx$id))
  expect_is(ctx$local, "environment")
  expect_is(ctx$global, "raw")
})

test_that("package_sources", {
  skip("rework")
  Sys.setenv(R_TESTS="")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  src <- package_sources(github="richfitz/kitten")
  handle <- context_save(path, packages="kitten",
                         package_sources=src)

  obj <- context_read(handle)
  expect_equal(obj$package_sources$local_drat, path_drat(ctx$path))
  expect_equal(obj$packages, list(attached="kitten", loaded=character(0)))

  tmp <- context_load(handle, quiet=TRUE)
  on.exit(unloadNamespace("kitten"), add=TRUE)
  expect_true("kitten" %in% .packages())
})

test_that("source files", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  src <- c("example-foo.R", "example-bar.R")
  expect_error(context_save(path, sources = src),
               "files do not exist")
  expect_error(context_save(path, sources = "../testthat.R"),
               "files above working directory")
  expect_error(context_save(path, sources = normalizePath("../testthat.R")),
               "files above working directory")

  writeLines(character(0), src[[1]])
  writeLines(character(0), src[[2]])
  on.exit(file.remove(src), add = TRUE)

  ctx <- context_save(path, sources = src)
  expect_equal(ctx$sources, src)
})

## Issues saving global environments: This does not tickle the problem
## unfortunately.
test_that("globals", {
  path <- tempfile()
  on.exit(cleanup(path))
  vars <- setdiff(names(.GlobalEnv), ".Random.seed")

  .GlobalEnv$t <- 1
  on.exit(rm(list = "t", envir = .GlobalEnv), add = TRUE)
  dat <- serialise_image()

  e <- new.env()
  v <- deserialise_image(dat, envir = e)
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
})

test_that("storage type", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  expect_error(context_save(path, storage_type = "redis"),
               "Unsupported storage type")
  expect_false(file.exists(path_config(path)))
  ctx <- context_save(path, storage_type = "rds")
  expect_error(context_save(path, storage_type = "redis"),
               "Incompatible storage types")
})

test_that("context_db", {
  skip("rework, possibly")
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
  expect_true(ok(context_db(ctx$path)))
  expect_true(ok(context_db(ctx$db)))
  expect_true(ok(context_db(list(path=ctx$path, db=ctx$db))))
  expect_true(ok(context_db(list(path=ctx$path))))
  expect_true(ok(context_db(list(db=ctx$db))))
  expect_error(ok(context_db(character(0))), "Cannot determine context path")
  expect_error(ok(context_db(c(ctx$path, ctx$path))),
               "Cannot determine context path")
  expect_error(ok(context_db(1L)), "Cannot determine context path")
})

test_that("environment backed context", {
  path <- tempfile()
  on.exit(cleanup(path))

  ctx <- context_save(path, storage_type = "environment")
  expect_is(ctx$db, "storr")
  expect_equal(ctx$db$driver$type(), "environment")

  expect_is(context_read(ctx$id, ctx$root), "context")
  expect_is(context_read(ctx$id, ctx$root$path, db = ctx$db), "context")

  expect_error(context_read(ctx$id, ctx$root$path),
               "Cannot reconnect to environment storage")
})

test_that("context_list", {
  path <- tempfile()
  on.exit(cleanup(path))

  expect_error(context_list(path),
               "Context root not set up at")
  expect_equal(context_list(path, error = FALSE), character(0))
  expect_equal(context_list(path, error = FALSE, names = TRUE),
               character(0))

  context_root_init(path)

  expect_equal(context_list(path), character(0))
  expect_equal(context_list(path, names = TRUE), character(0))

  ctx1 <- context_save(path)
  expect_equal(context_list(path), ctx1$id)
  expect_equal(context_list(path, names = TRUE), ctx1$name)

  ctx2 <- context_save(path)
  expect_equal(sort(context_list(path)), sort(c(ctx1$id, ctx2$id)))
  expect_equal(sort(context_list(path, names = TRUE)),
               sort(c(ctx1$name, ctx2$name)))
})

test_that("context_info", {
  path <- tempfile()
  on.exit(cleanup(path))

  expect_error(context_info(path, error = TRUE),
               "Context root not set up")
  info <- context_info(path, error = FALSE)
  expect_equal(nrow(info), 0)
  expect_equal(names(info), c("id", "name", "created"))
  expect_equal(info$id, character(0))
  expect_equal(info$name, character(0))
  expect_equal(info$created, empty_time())

  ctx1 <- context_save(path)
  Sys.sleep(0.1)
  ctx2 <- context_save(path)
  Sys.sleep(0.1)
  ctx3 <- context_save(path)

  info <- context_info(path)

  expect_equal(info$id,
               c(ctx1$id, ctx2$id, ctx3$id))
  expect_equal(info$name,
               c(ctx1$name, ctx2$name, ctx3$name))
  expect_is(info$created, "POSIXt")
  expect_true(all(as.numeric(diff(info$created), "secs") > 0))
})

test_that("compression works", {
  ctx1 <- context_save(tempfile(), storage_args = list(compress = TRUE))
  expect_true(ctx1$db$driver$compress)
  expr <- quote(rep(1:10, each = 100))
  t1 <- task_save(expr, ctx1)
  res1 <- task_run(t1, ctx1, envir = new.env(parent = .GlobalEnv))
  hash1 <- ctx1$db$get_hash(t1, "task_results")
  s1 <- file.size(ctx1$db$driver$name_hash(hash1))

  ctx2 <- context_save(tempfile(), storage_args = list(compress = FALSE))
  expect_false(ctx2$db$driver$compress)
  expr <- quote(rep(1:10, each = 100))
  t2 <- task_save(expr, ctx2)
  res2 <- task_run(t2, ctx2, envir = new.env(parent = .GlobalEnv))
  hash2 <- ctx2$db$get_hash(t2, "task_results")
  s2 <- file.size(ctx2$db$driver$name_hash(hash2))

  expect_gt(s2, s1)
  expect_equal(res1, res2)
  expect_identical(hash1, hash2)
})

test_that("print", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, envir = .GlobalEnv)
  expect_output(print(ctx), "<context>", fixed = TRUE)
})

test_that("name can't be id", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  expect_error(context_save(path, name = ids::random_id(), envir = .GlobalEnv),
               "name cannot be an id")
})

test_that("auto does not allow package listing", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  expect_error(context_save(path, auto = TRUE, packages = "testthat",
                            envir = .GlobalEnv),
               "Do not specify 'packages' or 'sources' if using auto")
  expect_error(context_save(path, auto = TRUE, sources = "noisy.R",
                            envir = .GlobalEnv),
               "Do not specify 'packages' or 'sources' if using auto")
})

test_that("non-attached packages", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, envir = .GlobalEnv,
                      packages = list(loaded = "testthat"))
  expect_equal(ctx$packages,
               list(attached = character(0), loaded = "testthat"))
})

test_that("packages validation", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  expect_error(context_save(path, envir = .GlobalEnv,
                            packages = list(foo = "a")),
               "Unknown names for 'packages': foo", fixed = TRUE)
  expect_error(context_save(path, envir = .GlobalEnv,
                            packages = list(loaded = TRUE)),
               "All elements of 'packages' must be a character", fixed = TRUE)
  expect_error(context_save(path, envir = .GlobalEnv,
                            packages = TRUE),
               "Incorrect type for 'packages'", fixed = TRUE)
})
