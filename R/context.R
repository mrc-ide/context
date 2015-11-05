##' Save and reload contexts.  Contexts consist of packages and
##' sources, or (if \code{auto}) is \code{TRUE}) a copy of the current
##' global environment.  Environments saved with \code{save_context}
##' can be reloaded with \code{load_context}; note that doing this
##' will probably alter the search path by loading any number of
##' packages.
##'
##' The \code{read_context} function simply reads the context, but
##' does not load it.  It is mostly useful for debugging.
##'
##' @title Save and reload contexts
##'
##' @param packages A character vector of packages (or \code{NULL}) if
##'   no packages are to be loaded.
##'
##' @param sources A character vector of source files to load.
##'
##' @param auto Attempt to create the context automatically.  In this
##'   case, do not specify either \code{packages} or \code{sources}.
##'
##' @param package_sources Optional information about where to find
##'   non-CRAN packages.  See \code{\link{package_sources}}.
##'
##' @param envir The current environment.  This is used to copy
##'   \emph{local} enviroments around, as these are needed even in the
##'   case of non-automatic contexts.  For \code{load_context} this
##'   is the environment into which the global environment is copied.
##'   Specify a non-global environment here to avoid clobbering the
##'   workspace, but at the risk that some environments may not
##'   restore exactly as desired.
##'
##' @param root Root directory to store and retrieve files.  Files
##'   will be added to the \code{contexts} subdirectory of this path.
##'   This will change later to support alternative ways of saving
##'   files.  The default puts files into the temp directory of
##'   \emph{this} R instance; this directory will be deleted on
##'   session exit so it not an appropriate place to store files for
##'   later use.
##'
##' @param handle A \code{context_handle} object returned by
##'   \code{save_context}.
##'
##' @export
##' @rdname context
save_context <- function(packages=NULL, sources=NULL, auto=FALSE,
                         package_sources=NULL,
                         envir=parent.frame(), root=tempdir()) {
  setup_bootstrap(root)
  if (auto) {
    if (!is.null(packages) || !is.null(sources)) {
      stop("Do not specify 'packages' or 'sources' if using auto")
    }
    ret <- list(
      packages=detect_packages(),
      global=save_image(path_environments(root)))
  } else {
    if (is.null(packages)) {
      packages <- character(0)
    }
    if (is.character(packages)) {
      packages <- list(attached=packages, loaded=character(0))
    } else if (is.list(packages)) {
      if (!setequal(names(packages), c("loaded", "attached"))) {
        stop("Incorrect names for 'packages'")
      }
    } else {
      stop("Incorrect type for packages")
    }
    ret <- list(packages=packages, sources=sources)
    ## Here, we _do_ need to check that all source files are
    ## *relative* paths, and we'll need to arrange to copy things
    ## around as approriate.  I'll punt on that for now as it's going
    ## to take a little work to get that all happy, and requires some
    ## of the things in pathr that aren't done yet.
  }

  if (is.null(package_sources)) {
    package_sources <- package_sources()
  } else if (inherits(package_sources, "package_sources")) {
    build_local_drat(package_sources, root)
  } else {
    stop("Expected a package_sources object (or NULL)")
  }
  ret$package_sources <- package_sources

  ret$local <- save_object(envir, path_environments(root))
  ret$auto <- auto
  class(ret) <- "context"
  id <- save_object(ret, path_contexts(root))
  context_handle(id, root)
}

##' @rdname context
##' @param install Install missing packages?
##'
##' @param ... Additional arguments passed through to
##'   \code{install_packages} if it is used.
##'
##' @export
load_context <- function(handle, install=TRUE, envir=.GlobalEnv, ...) {
  if (!is.context_handle(handle)) {
    stop("handle must be a context_handle")
  }
  context_log("context", handle$id)
  obj <- read_context(handle)

  use_local_library(handle$root)
  if (install) {
    install_packages_missing(c(obj$packages$attached, obj$packages$loaded),
                             package_sources=obj$package_sources, ...)
  }

  context_log("library", paste0(obj$packages$attached, collapse=", "))
  for (p in rev(setdiff(obj$packages$attached, .packages()))) {
    library(p, character.only=TRUE)
  }
  context_log("namespace", paste0(obj$packages$loaded, collapse=", "))
  for (p in rev(setdiff(obj$packages$loaded, loadedNamespaces()))) {
    loadNamespace(p)
  }

  context_log("source", paste0(obj$sources, collapse=", "))
  for (s in obj$sources) {
    source(s, envir)
  }

  if (!is.null(obj$global)) {
    context_log("global", obj$global)
    load(path_environments(handle$root, obj$global), envir=envir)
  }
  if (!is.null(obj$local)) {
    context_log("local", obj$local)
    readRDS(path_environments(handle$root, obj$local))
  } else {
    envir
  }
}

##' @export
##' @rdname context
read_context <- function(handle) {
  ## TODO: same treatment as read_task where read_task(task) -> task
  ret <- readRDS(path_contexts(handle$root, handle$id))
  ## We'll take responsibility here for setting up the local drat
  ## links because install_packages does not know about ideas of
  ## roots; it just has the set of sources (which by this point has
  ## basically whittled down to a set of repositories which is kind
  ## of cool).
  ##
  ## Because the final drat link needs to be an absolute path, this
  ## means that wherever the context is read will get the correct
  ## local path.
  if (isTRUE(ret$package_sources$use_local_drat)) {
    ret$package_sources$local_drat <- path_drat(handle$root)
  }
  ret
}

## This is going to be horrid to test because it really requires
## tweaking the search path and dealing with that terrible way that
## the R CMD check tests work.  Better would be to mock sessionInfo
## data so that we can create a few sensible mock ups and send that
## through.
detect_packages <- function(obj=sessionInfo()) {
  loaded <- names(obj$loadedOnly)[!vlapply(obj$loadedOnly, function(x)
    identical(x$Priority, "base"))]

  attached <- names(obj$otherPkgs)
  ## This is defensive: ?sessionInfo does not make any guarantees
  ## about package loading.
  ord <- sub("^package:", "", search())
  attached <- attached[rank(match(attached, ord))]

  ## TODO: detect devtools packages so we can get the sources.
  ## TODO: detect if package is *loaded* by devtools.
  ## TODO: append version information and try to guarantee versions on
  ## the other end?
  ## TODO: remove `.packageName` and its dependencies from this list?
  list(loaded=loaded, attached=attached)
}

is.context <- function(x) {
  inherits(x, "context")
}

context_handle <- function(id, root) {
  structure(list(id=id, root=root), class="context_handle")
}
is.context_handle <- function(x) {
  inherits(x, "context_handle")
}

##' @export
print.context_handle <- function(x, ...) {
  print_ad_hoc(x)
}
##' @export
print.context <- function(x, ...) {
  print_ad_hoc(x)
}

context_exists <- function(id, root) {
  file.exists(path_contexts(root, id))
}

use_local_library <- function(root) {
  lib <- path_library(root)
  context_log("lib", lib)
  dir.create(lib, FALSE, TRUE)
  ## This preserves all the previous libPaths; .libPaths(lib) would
  ## nuke all but the system libraries.
  .libPaths(union(lib, .libPaths()))
  invisible(lib)
}
