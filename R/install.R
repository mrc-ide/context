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
##' @title Install packages
##' @param packages A character vector of packages to install
##'
##' @param sources Source information from
##'   \code{\link{package_sources}}.
##'
##' @param ... Additional arguments passed through to
##'   \code{\link{install.packages}} (or for
##'   \code{install_packages_missing}, to \code{install_packages}).
##'
##' @param error Throw an error if package installation fails.
##'
##' @param move_in_place Attempt to do the right thing during parallel
##'   installations that affect the same packages.  When \code{TRUE},
##'   installation happens into a temporary directory and then files
##'   are copied into place.  This means that if multiple processes
##'   are installing the same packages they should not get into a
##'   tangle; they'll both install and compile everything (which is
##'   much simpler than trying to negotiate lockfiles and waiting
##'   periods) but the processes will not get into trouble where one
##'   is disallowed from removing the packages used by another.  If
##'   installation fails catastrophically this may leave some files in
##'   \code{.libPaths()[[1]]} beginning \code{CONTEXT_TMP_LIBRARY_};
##'   these can be safely removed.
##'
##' @export
install_packages <- function(packages, sources=package_sources(),
                             lib=NULL, ..., error=TRUE,
                             move_in_place=FALSE) {
  ## TODO: sources -> package_sources?
  if (length(packages) == 0L) {
    return()
  }
  ## This attempts to avoid listing CRAN twice which makes
  ## available.packages quite slow.
  r <- getOption("repos")
  r <- r[r != sources$cran]
  r["CRAN"] <- sources$cran
  if (!is.null(sources$repos)) {
    r <- c(r, sources$repos)
  }
  ## TODO: The dealing with local_drat here is broken; it does not
  ## come out of package_sources() correctly (i.e., is not set).  This
  ## is because the context_read sets the sources generally and this
  ## function does not know about that.
  ##
  ## The resolution needs to be that this function should not take
  ## sources but instead take arguments 'cran' and 'repos'.
  if (!is.null(sources$local_drat)) {
    drat_add_empty_bin(sources$local_drat)
    r <- c(r, "local_drat"=file_url(sources$local_drat))
  }
  context_log("install", paste(packages, collapse=", "))

  lib <- .libPaths()[[1]]

  if (move_in_place) {
    lib_real <- lib
    lib <- tempfile(tmpdir=lib_real, pattern="CONTEXT_TMP_LIBRARY_")
    dir.create(lib, FALSE)
    on.exit(unlink(lib, recursive=TRUE))
    context_log("tmplib", lib_real)
  }

  install.packages2(packages, repos=r, ..., lib=lib, error=error)

  if (move_in_place) {
    installed <- dir(lib)
    try <- setdiff(installed, dir(lib_real))
    ## TODO: It's possible here that 'skip' is a potential problem;
    ## the process running skip might complete before another process
    ## has finished its copy, and then fail to load packages.  It's
    ## possible that we could wait for the packages to be put into
    ## place but in practice that's going to be very difficult to do
    ## (and some connections are super slow).  It might be best to
    ## just try and copy everything.
    if (length(try) > 0L) {
      file.copy(file.path(lib, try), lib_real, overwrite=FALSE, recursive=TRUE)
    }
    skip <- setdiff(installed, try)

    msg <- c(
      if (length(try) > 0L) paste("copied", paste(try, collapse=", ")),
      if (length(skip) > 0L) paste("skipped", paste(skip, collapse=", ")))
    context_log("installed", paste(msg, collapse=" | "))
  }

  invisible()
}

##' @export
##' @rdname install_packages
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
  e <- NULL
  capture <- function(e) {
    if (error) {
      catch <-
        grepl("package.*(is|are) not available", e$message) ||
        grepl("installation of package.*had non-zero exit status", e$message)
      if (catch) {
        e <<- e
      }
    }
  }
  withCallingHandlers(install.packages(pkgs, ...), warning=capture)
  if (!is.null(e)) {
    stop(e$message, call.=FALSE)
  }
  ## Here we *could* check that the package name is installed but for
  ## local packages (e.g., repos=NULL) we need to go into the archive
  ## and get the correct package name!
}
