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
##' @param sources Source information from
##'   \code{\link{package_sources}}.
##'
##' @param ... Additional arguments passed through to
##'   \code{\link{install.packages}}.
##'
##' @param error Throw an error if package installation fails.
##'
##' @param lock_wait Lock the directory while installing, or wait for
##'   a second installation to complete.  In contrast with
##'   \code{\link{install.packages}}, the lock is always library-wide.
##'   Catastrophoic failure (e.g., out of memory error during package
##'   installation) could leave a lockfile present.  In that case,
##'   delete the file \code{.lock_context} from the first entry in
##'   \code{.libPaths()}.
##'
##' @export
install_packages <- function(packages, sources=package_sources(),
                             ..., error=TRUE, lock_wait=TRUE) {
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
  if (!is.null(sources$local_drat)) {
    r <- c(r, "local_drat"=file_url(sources$local_drat))
  }
  context_log("install", paste(packages, collapse=", "))
  if (lock_wait) {
    lockfile <- file.path(.libPaths()[[1]], ".lock_context")
    do_install <- lock(lockfile)
    if (do_install) {
      on.exit(file.remove(lockfile))
    } else {
      lock_wait(lockfile, packages)
    }
  }
  if (do_install) {
    install.packages2(packages, repos=r, ..., error=error)
  }
  invisible()
}

## Create a lockfile, returning TRUE if the lockfile is ours (in an
## effort to avoid race conditions).  Rather than open the file for
## writing (which might be subject to client side caching, we try and
## *copy* the file without overwriting which the underlying
## implementation *should* only allow once.
##
## It's possible that the inverse approch might be better.
##   http://stackoverflow.com/questions/668336/platform-independent-file-locking
## leave a file there always (e.g., during use_local_library) and copy
## that away.  but I don't see that would do better in general.
## See also:
##   http://www.dwheeler.com/secure-programs/Secure-Programs-HOWTO/avoid-race.html
lock <- function(lockfile, ...) {
  ## NOTE: random might not be enough here for processes started at
  ## exactly the same time, but hostname/pid should go a long way.
  str <- sprintf("%s:%s:%s", hostname(), process_id(), random_id())
  tmp <- tempfile()
  writeLines(str, tmp, ...)
  on.exit(file.remove(tmp))
  ok <- !file.exists(lockfile) && file.copy(tmp, lockfile, overwrite=FALSE)
  ok && readLines(lockfile) == str
}

lock_wait <- function(lockfile, packages, wait_minutes=10, check_seconds=1) {
  ## How long seems reasonable? 5 minutes? 10 minutes?  This is
  ## hard to tune because we're deep within a bunch of function
  ## calls here.
  context_log("(waiting)", Sys_time())
  wait <- as.difftime(wait_minutes, units="mins")
  t0 <- Sys.time()
  while (file.exists(lockfile) && Sys.time() - t0 < wait) {
    Sys.sleep(check_seconds)
    message(".", appendLF=FALSE)
  }
  message("")
  if (file.exists(lockfile)) {
    stop("Previous installation failed *badly* or taking too long: aborting")
  }
  msg <- setdiff(packages, .packages(TRUE))
  if (length(msg) > 0L) {
    stop("Previous installation failed; missing: ", paste(msg, collapse=", "))
  }
  context_log("(resuming)", Sys_time())
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
  e <- NULL
  capture <- function(e) {
    if (error && grepl("package.*(is|are) not available", e$message)) {
      e <<- e
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
