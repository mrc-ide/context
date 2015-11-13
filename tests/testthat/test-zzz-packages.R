context("packages")

test_that("no special packages", {
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  src <- package_sources()

  build_local_drat(src, root=root)
  expect_equal(dir(root), "drat")
  expect_equal(length(dir(file.path(root, "drat", "src", "contrib"),
                          pattern="^context_.*tar.gz$")), 1L)

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
  root <- tempfile("cluster_")
  on.exit(cleanup(root))

  callr <- build_remote(github_url("traitecoevo", "callr", "master"), NA, TRUE)
  src <- package_sources(github="dide-tools/context",
                         bitbucket="dannavarro/lsr-package",
                         local=callr)

  expect_is(src$expire, "difftime")

  drat_src <- file.path(path_drat(root), "src", "contrib")
  src <- build_local_drat(src, root=root, quiet=TRUE)

  expect_true(file.exists(drat_src))
  pkgs <- read.dcf(file.path(drat_src, "PACKAGES"))
  expect_true(setequal(pkgs[, "Package"], c("context", "lsr", "callr")))

  ## Installation should work from this:
  olp <- .libPaths()
  expect_false(file.exists(path_library(root)))

  ## A local library:
  lib <- use_local_library(path_library(root))
  expect_equal(lib, path_library(root))
  expect_true(file.exists(lib))
  expect_equal(.libPaths()[[1]], lib)
  ## No previously enbled libraries have been removed
  expect_true(all(olp %in% .libPaths()))

  install_packages("context", src, quiet=TRUE)
  expect_true(file.exists(file.path(lib, "context")))
})

test_that("package installation in parallel", {
  skip_if_no_fork()
  lib <- use_local_library(tempfile("context_"))
  on.exit(cleanup(lib))

  f <- function() {
    capture_messages({
      install_packages("digest", quiet=TRUE)
      packageVersion("digest", lib)
    })
  }

  ## Sanity check:
  expect_error(packageVersion("digest", lib),
               "not found")

  context_log_start()
  on.exit(context_log_stop(), add=TRUE)
  t1 <- parallel::mcparallel(f(), "i1")
  t2 <- parallel::mcparallel(f(), "i2")
  res <- parallel::mccollect(list(t1, t2))

  ## Both packages did install so that's nice:
  val <- lapply(res, attr, "result", exact=TRUE)
  expect_true(all(vlapply(val, inherits, "package_version")))

  ## Messages are turned off, but can expect this:
  n <- viapply(res, length)
  expect_true(sum(n == 1L) == 1L)
  expect_true(sum(n > 1L) == 1L)

  res_installed <- res[[which(n == 1)]]
  res_waited <- res[[which(n > 1)]]

  re_install <- "^\\[\\s+install\\s+\\]\\s+digest"
  re_wait <- "^\\[\\s+\\(waiting\\)\\s+\\]\\s+"
  re_resume <- "^\\[\\s+\\(resuming\\)\\s+\\]\\s+"
  expect_true(grepl(re_install, res_installed[[1]]))
  expect_true(grepl(re_install, res_waited[[1]]))
  expect_true(grepl(re_wait, res_waited[[2]]))
  expect_true(grepl(re_resume, res_waited[[length(res_waited)]]))
})
