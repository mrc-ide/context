context("packages")

test_that("no special packages", {
  src <- package_sources()
  expect_false(src$use_local_drat)

  root <- tempfile("cluster_")
  build_local_drat(src, root=root)
  expect_equal(dir(root), character(0))

  expect_null(src$repos)
})

test_that("drat repos", {
  str <- "drat://OutbreakResources"
  src <- package_sources(repos="drat://OutbreakResources")
  expect_equal(src$repos,
               setNames("https://OutbreakResources.github.io/drat/", str))

  str <- "https://whatever/repo"
  src <- package_sources(repos=str)
  expect_equal(src$repos, setNames(str, str))

  expect_error(package_sources(repos="string"),
               "Missing url scheme")
})

test_that("local drat creation", {
  callr <- build_remote(github_url("traitecoevo", "callr", "master"), NA, TRUE)
  src <- package_sources(github="dide-tools/context",
                         bitbucket="dannavarro/lsr-package",
                         local=callr)

  expect_is(src$expire, "difftime")
  expect_true(src$use_local_drat)

  root <- tempfile("cluster_")
  drat_src <- file.path(path_drat(root), "src", "contrib")
  build_local_drat(src, root=root, quiet=TRUE)

  expect_true(file.exists(drat_src))
  pkgs <- read.dcf(file.path(drat_src, "PACKAGES"))
  expect_true(setequal(pkgs[, "Package"], c("context", "lsr", "callr")))

  ## Installation should work from this:
  olp <- .libPaths()
  on.exit(.libPaths(olp))
  expect_false(file.exists(path_library(root)))
  lib <- install_packages("context", src, root, local=TRUE, quiet=TRUE)

  expect_equal(lib, path_library(root))
  expect_true(file.exists(lib))
  expect_equal(.libPaths()[[1]], lib)
  expect_true(file.exists(file.path(lib, "context")))
})
