##' Install packages, using source information from
##' \code{\link{package_sources}}.  In contrast with
##' \code{install.packages} this function can install packages in a
##' new local repository (used to isolate packages from the rest of
##' the system) and can install packages originally found on github
##' and bitbucket by creating a transient drat repository.  In
##' addition, \code{install_packages_missing} only installs
##' \emph{missing} packages only.
##'
##' @section Warning:
##'
##' Because of the possibility of confusion with
##'   \code{\link{install.packages}}, this function may be renamed
##'   shortly.
##'
##' @title Install packages
##' @param packages A character vector of packages to install
##' @param sources Source information from \code{\link{package_sources}}
##'
##' @param root The context root path; used for constructing drat and
##'   library directories.
##'
##' @param local Install packages in a local library (under
##'   \code{root}) rather than in whatever the system library is?
##'
##' @param ... Additional arguments passed through to
##'   \code{\link{install.packages}}.
##'
##' @export
install_packages <- function(packages, sources, root, local=TRUE, ...) {
  ## This attempts to avoid listing CRAN twice which makes
  ## available.packages quite slow.
  r <- getOption("repos")
  r <- r[r != sources$cran]
  r["CRAN"] <- sources$cran
  if (!is.null(sources$repos)) {
    r <- c(r, sources$repos)
  }
  if (!is.null(sources$use_local_drat)) {
    build_local_drat(sources, root)
    local_drat <- normalizePath(path_drat(root), winslash="/")
    r <- c(r, "local_packages"=paste0("file://", local_drat))
  }
  if (local) {
    lib <- path_library(root)
    library_init(lib)
  } else {
    lib <- .libPaths()[[1]]
  }

  context_log("install", paste(packages, collapse=", "))
  install.packages(packages, lib=lib, repos=r, ...)
  invisible(lib)
}

install_packages_missing <- function(packages, ...) {
  install_packages(setdiff(packages, .packages(TRUE)), ...)
}

library_init <- function(path) {
  dir.create(path, FALSE, TRUE)
  .libPaths(path)
}
