context("contexts")

## TODO need to check that resaving a context

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

  expect_null(ctx$local)

  expect_true(is_id(ctx$id), "character")
  expect_is(ctx$db, "storr")
  expect_identical(ctx$root$path, path)
  expect_identical(context_root_get(ctx)$path, path)

  expect_null(ctx$unique_value)

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

  ctx2 <- context_save(path, envir = .GlobalEnv)
  expect_equal(ctx2[v], ctx[v])
})

test_that("load contexts", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  ctx <- context_save(path, envir = .GlobalEnv)
  expect_is(ctx, "context")

  dat <- context_read(ctx$id, path)
  expect_is(dat, "context")
  v <- c("name", "id", "packages")
  expect_equal(dat[v], ctx[v])

  dat <- context_read(ctx$name, path)
  expect_is(dat, "context")
  v <- c("name", "id", "packages")
  expect_equal(dat[v], ctx[v])

  obj <- context_load(ctx, envir = new.env())
  expect_is(obj, "context")
  expect_is(obj$envir, "environment")
  expect_equal(ls(obj$envir), character(0))
})

test_that("package_sources", {
  ## TODO: move into one of the zzz cases?
  skip_if_not_installed("provisionr")
  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  drat <- tempfile()
  src <- provisionr::package_sources(github = "richfitz/kitten")
  handle <- context_save(path, packages = "kitten",
                         package_sources = src)
  expect_is(handle$package_sources, "package_sources")
  expect_true(handle$package_sources$needs_build())
  ## src is unchanged:
  expect_null(src$local_drat)
  expect_true(src$needs_build())

  ## Then check that this is all OK
  obj <- context_read(handle$id, path)
  expect_null(obj$package_sources$local_drat)
  expect_equal(obj$packages, list(attached = "kitten", loaded = character(0)))
})

test_that("invalid package sources", {
  expect_error(
    context_save(tempfile(), storage_type = "environment",
                 package_sources = "http://url.to/mypackage"),
    "package_sources must inherit from package_sources")
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
  expect_equal(context_list(path, error = FALSE, named = TRUE),
               setNames(character(0), character(0)))

  context_root_init(path)

  expect_equal(context_list(path), character(0))
  expect_equal(context_list(path, named = TRUE),
               setNames(character(0), character(0)))

  ctx1 <- context_save(path)
  expect_equal(context_list(path), ctx1$id)
  expect_equal(context_list(path, named = TRUE),
               setNames(ctx1$id, ctx1$name))
  Sys.sleep(0.1)

  ctx2 <- context_save(path, packages = "ape")
  expect_equal(context_list(path), c(ctx1$id, ctx2$id))
  expect_equal(context_list(path, named = TRUE),
               setNames(c(ctx1$id, ctx2$id), c(ctx1$name, ctx2$name)))
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
  ctx2 <- context_save(path, packages = "ape")
  Sys.sleep(0.1)
  ctx3 <- context_save(path, packages = "kitten")

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
  ctx1_run <- context_load(ctx1, envir = new.env(parent = .GlobalEnv))

  expect_true(ctx1$db$driver$compress)
  expr <- quote(rep(1:10, each = 100))
  t1 <- task_save(expr, ctx1)

  res1 <- task_run(t1, ctx1_run)

  hash1 <- ctx1$db$get_hash(t1, "task_results")
  s1 <- file.size(ctx1$db$driver$name_hash(hash1))

  ctx2 <- context_save(tempfile(), storage_args = list(compress = FALSE))
  ctx2_run <- context_load(ctx2, envir = new.env(parent = .GlobalEnv))

  expect_false(ctx2$db$driver$compress)
  expr <- quote(rep(1:10, each = 100))
  t2 <- task_save(expr, ctx2)
  res2 <- task_run(t2, ctx2_run)

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

test_that("name logic", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx1 <- context_save(path, envir = .GlobalEnv)
  expect_is(ctx1, "context")
  expect_is(ctx1$name, "character")

  ctx2 <- context_save(path, envir = .GlobalEnv, name = NULL)
  expect_equal(ctx2$name, ctx2$name)

  ctx3 <- context_save(path, envir = .GlobalEnv,
                       name = ids::adjective_animal())
  expect_true(ctx3$name != ctx2$name)
  expect_equal(ctx3$id, ctx2$id)

  v <- setdiff(names(ctx1), c("db", "root", "local"))

  ## Load by new name:
  expect_equal(context_read(ctx3$name, path)[v], ctx3[v])
  ## Load by old name:
  expect_equal(context_read(ctx1$name, path)[v], ctx1[v])
  ## Load by id, gets new name
  expect_equal(context_read(ctx3$id, path)[v], ctx3[v])
})

test_that("set unique value", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx1 <- context_save(path, sources = "myfuns.R")
  ctx2 <- context_save(path, sources = "myfuns.R")
  expect_equal(ctx1$id, ctx2$id)
  expect_null(ctx1$unique_value)
  expect_null(ctx2$unique_value)

  uva <- "a"
  uvb <- "b"
  ctx3 <- context_save(path, sources = "myfuns.R", unique_value = uva)
  ctx4 <- context_save(path, sources = "myfuns.R", unique_value = uvb)
  ctx5 <- context_save(path, sources = "myfuns.R", unique_value = uvb)
  expect_true(ctx3$id != ctx4$id)
  expect_equal(ctx4$id, ctx5$id)
  expect_equal(ctx3$unique_value, uva)
  expect_equal(ctx4$unique_value, uvb)
  expect_equal(ctx5$unique_value, uvb)
})

test_that("last context", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))

  expect_null(last_loaded_context(FALSE))
  expect_error(last_loaded_context(TRUE), "No context has been loaded")

  ctx1 <- context_save(path, envir = .GlobalEnv)
  ctx1 <- context_load(ctx1, new.env(parent = .GlobalEnv))
  expect_identical(last_loaded_context(), ctx1)
})

test_that("reload", {
  Sys.setenv(R_TESTS = "")
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  ctx1 <- context_save(path, sources = "random.R",
                       storage_type = "environment")
  ctx2 <- context_load(ctx1, new.env(parent = .GlobalEnv))
  expect_is(ctx2$envir$x, "numeric")
  ctx3 <- context_load(ctx2, new.env(parent = .GlobalEnv))
  expect_identical(ctx3, ctx2)
  expect_identical(ctx3$envir$x, ctx2$envir$x)
  ctx4 <- context_load(ctx2, new.env(parent = .GlobalEnv), refresh = TRUE)
  expect_false(identical(ctx4, ctx2))
  expect_false(identical(ctx4$envir$x, ctx2$envir$x))
})

test_that("local", {
  path <- tempfile("cluster_")
  on.exit(cleanup(path))
  e1 <- new.env(parent = environment())
  e1$x <- 1

  ctx <- context_save(path, envir = e1, sources = "myfuns.R")
  expect_is(ctx, "context")
  expect_is(ctx$local, "environment")

  e2 <- new.env(parent = environment())
  res <- context_load(ctx, envir = e2)

  expect_equal(ls(res$envir), "x")
  expect_true("loop" %in% ls(parent.env(res$envir)))
  expect_identical(e2, parent.env(res$envir))
})
