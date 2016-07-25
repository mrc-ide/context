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
##' @param lib Optional library to install packages into
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
                             lib=NULL, ..., error=TRUE, move_in_place=FALSE) {
  ## TODO: sources -> package_sources?
  if (length(packages) == 0L) {
    return()
  }

  r <- context_repos(sources)
  context_log("install", paste(packages, collapse=", "))

  if (is.null(lib)) {
    lib <- .libPaths()[[1]]
  }

  if (move_in_place) {
    lib_real <- lib
    pat <- sprintf("CONTEXT_%s_%d_TMP", Sys.info()[["nodename"]], Sys.getpid())
    lib <- tempfile(pat, lib_real)
    dir.create(lib, FALSE)
    on.exit(unlink(lib, recursive=TRUE))
    context_log("tmplib", lib)
  }

  install.packages2(packages, repos=r, ..., lib=lib, error=error)

  if (move_in_place) {
    installed <- .packages(TRUE, lib)
    re <- "^CONTEXT_(.*)_TMP(.*)$"
    others <- setdiff(dir(lib_real, pattern=re), basename(lib))
    if (length(others) > 0L) {
      context_log("us", sub(re, "\\1", basename(lib)))
      context_log("others",
                  paste(sub(re, "\\1", others), collapse=", "))
    }

    new <- setdiff(installed, dir(lib_real))
    exist <- setdiff(installed, new)
    file.copy(file.path(lib, installed), lib_real,
              overwrite=FALSE, recursive=TRUE)
    msg <- c(
      if (length(new) > 0L) paste("copied", paste(new, collapse=", ")),
      if (length(exist) > 0L) paste("skipped", paste(exist, collapse=", ")))
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

context_repos <- function(sources) {
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
    drat_add_empty_bin(contrib.url(sources$local_drat, "binary"))
    r <- c(r, "local_drat"=file_url(sources$local_drat))
  }
  r
}
