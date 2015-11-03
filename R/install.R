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
##'
##' @param package_sources Source information from
##'   \code{\link{package_sources}}.
##'
##' @param ... Additional arguments passed through to
##'   \code{\link{install.packages}}.
##'
##' @export
install_packages <- function(packages, package_sources, ...) {
  if (length(packages) == 0L) {
    return()
  }
  ## This attempts to avoid listing CRAN twice which makes
  ## available.packages quite slow.
  r <- getOption("repos")
  r <- r[r != package_sources$cran]
  r["CRAN"] <- package_sources$cran
  if (!is.null(package_sources$repos)) {
    r <- c(r, package_sources$repos)
  }
  if (!is.null(package_sources$local_drat)) {
    r <- c(r, "local_drat"=file_url(package_sources$local_drat))
  }
  context_log("install", paste(packages, collapse=", "))
  install.packages(packages, repos=r, ...)
  invisible()
}

install_packages_missing <- function(packages, ...) {
  install_packages(setdiff(packages, .packages(TRUE)), ...)
}
