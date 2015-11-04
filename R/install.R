##' Install packages, using source information from
##' \code{\link{package_sources}}.  In contrast with
##' \code{install.packages} this function can install packages in a
##' new local repository (used to isolate packages from the rest of
##' the system) and can install packages originally found on github
##' and bitbucket by creating a transient drat repository.  In
##' addition, \code{install_packages_missing} only installs
##' \emph{missing} packages only.  Also in contrast with
##' \code{install.packages}, these function will throw an error if
##' package installation fails (unless error=FALSE is set).
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
##' @param error Throw an error if package installation fails.
##'
##' @export
install_packages <- function(packages, package_sources, ..., error=TRUE) {
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
  install.packages(packages, repos=r, ..., error=error)
  invisible()
}

install_packages_missing <- function(packages, ...) {
  install_packages(setdiff(packages, .packages(TRUE)), ...)
}

## A version of install.packages() that will *stop on failure*.  Why
## is this needed?
##
## NOTE: rather than stopping on *all* warnings, we'll attempt to
## filter this to be the one that happens when packages are not
## available.
##
## TODO: This might have to route through gettext to work on
## non-English platforms.  Not totally sure how that is meant to be
## set though.
##
## The other option here is to check that all packages are installed
## coming out of this function, which we also attempt to do.
##
## Geting the withCallingHandlers bit correct is super difficult to
## make sure that the warnings are preseved.
install.packages2 <- function(pkgs, ..., error=TRUE) {
  ## capture <- function(e) {
  ##   if (error && grepl("package.*(is|are) not available", e$message)) {
  ##     stop(e) # or stop(e, call.=FALSE)
  ##   }# else clause here should restart so that other messages make it out.
  ## }
  ## withCallingHandlers(install.packages(pkgs), warning=capture)
  ## This is a little extra check that will behave poorly if lib is
  ## passed explicitly to install.packages, or if the packages were
  ## already installed.
  install.packages(pkgs, ...)
  if (!all(pkgs %in% .packages(TRUE))) {
    stop("Failure in install.packages (see above)")
  }
}
