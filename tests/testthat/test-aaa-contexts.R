context("local drat")

## This one is useful to have running first because then we don't have
## to redownload the packages every time.
test_that("local drat", {
  root <- tempfile("context_")
  db <- context_db(root)
  Sys.setenv(R_TESTS="")

  ## Building packages for local sources takes ~0.8s which is
  ## terrible.  I'd love to know how to make that faster.
  sources <- setup_bootstrap_self_sources()
  build_local_drat(sources, root)

  keys <- db$list("drat_timestamp")
  expect_equal(length(keys), 2L)
  expect_true(db$get(keys[[1]], "drat_timestamp") <= Sys.time())

  path <- file.path(path_drat(root), "src", "contrib", "PACKAGES")
  expect_true(file.exists(path))

  ## Running a second time does not fail:
  build_local_drat(sources, root)

  tmp <- file.path(path_drat(root), "src", "contrib")
  path_storr <- dir(tmp, "^storr_", full.names=TRUE)[[1]]
  path_context <- dir(tmp, "^context_", full.names=TRUE)[[1]]
  expect_true(file.exists(path_storr))
  expect_true(file.exists(path_context))
  Sys.setenv("CONTEXT_SOURCE_PATH"=path_context)
  Sys.setenv("STORR_SOURCE_PATH"=path_storr)

  ## Now, try again; it's not tested but this is extremely quick.
  root2 <- tempfile("context_")
  sources2 <- setup_bootstrap_self_sources()
  expect_true(path_storr %in% names(sources2$local))
  expect_true(path_context %in% names(sources2$local))
  on.exit(cleanup(root2))
  db <- context_db(root2)
  build_local_drat(sources2, root2)

  path <- file.path(path_drat(root2), "src", "contrib", "PACKAGES")
  expect_true(file.exists(path))
})
