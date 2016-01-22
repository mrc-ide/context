##' Save and reload contexts.  Contexts consist of packages and
##' sources, or (if \code{auto}) is \code{TRUE}) a copy of the current
##' global environment.  Environments saved with \code{context_save}
##' can be reloaded with \code{context_load}; note that doing this
##' will probably alter the search path by loading any number of
##' packages.
##'
##' The \code{context_read} function simply reads the context, but
##' does not load it.  It is mostly useful for debugging.
##'
##' @title Save and reload contexts
##'
##' @param root Root directory to store and retrieve files.  Files
##'   will be added to the \code{contexts} subdirectory of this path.
##'   This will change later to support alternative ways of saving
##'   files, perhaps into a database instance.
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
##'   case of non-automatic contexts.  For \code{context_load} this
##'   is the environment into which the global environment is copied.
##'   Specify a non-global environment here to avoid clobbering the
##'   workspace, but at the risk that some environments may not
##'   restore exactly as desired.
##'
##' @param storage_type Optional storage type.  Currently only 'rds'
##'   storage is supported, so this is largely ignored.
##'
##' @param storage_args Arguments used to open storage driver
##'   (currently ignored).
##'
##' @param handle A \code{context_handle} object returned by
##'   \code{context_save}.
##'
##' @export
##' @rdname context
context_save <- function(root, packages=NULL, sources=NULL, auto=FALSE,
                         package_sources=NULL, envir=parent.frame(),
                         storage_type=NULL, storage_args=NULL) {
  setup_bootstrap(root)
  db <- setup_context(root, storage_type, storage_args)
  ret <- context_build(packages, sources, auto, package_sources, envir)
  ## NOTE: This is going to give us an _absolute_ path, which we will
  ## tend to rewrite.  It's not enough to assume that we can say
  ## TRUE/FALSE here because install_packages does not know about the
  ## context root.
  ret$package_sources <-
    build_local_drat(package_sources, path_drat(root))
  id <- db$set_by_value(ret, namespace="contexts", use_cache=FALSE)
  context_handle(root, id, db)
}

context_build <- function(packages, sources, auto, package_sources, envir) {
  if (auto) {
    if (!is.null(packages) || !is.null(sources)) {
      stop("Do not specify 'packages' or 'sources' if using auto")
    }
    ret <- list(
      packages=detect_packages(),
      global=serialise_image())
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
    ret <- list(packages=packages)
    if (!is.null(sources)) {
      ## Here, we _do_ need to check that all source files are
      ## *relative* paths, and we'll need to arrange to copy things
      ## around as approriate.  I'll punt on that for now as it's going
      ## to take a little work to get that all happy, and requires some
      ## of the things in pathr that aren't done yet.
      ##
      ## Files must be relative to R's working directory for this to
      ## have any chance of working.
      ret$sources <- relative_paths(sources)
    }
  }

  if (is.null(package_sources)) {
    package_sources <- package_sources()
  } else if (!inherits(package_sources, "package_sources")) {
    stop("Expected a package_sources object (or NULL)")
  }

  ret$package_sources <- package_sources
  ret$local <- if (is.GlobalEnv(envir)) NULL else envir
  ret$auto <- auto
  class(ret) <- "context"
  ret
}

##' @rdname context
##' @param install Install missing packages?
##'
##' @param ... Additional arguments passed through to
##'   \code{install_packages} if it is used.
##'
##' @export
context_load <- function(handle, install=TRUE, envir=.GlobalEnv, ...) {
  if (!is.context_handle(handle) || is.context(handle)) {
    stop("handle must be a context_handle")
  }
  context_log("context", handle$id)
  if (is.context_handle(handle)) {
    obj <- context_read(handle)
  } else {
    obj <- handle
  }

  ## NOTE: This is not scoped.  That's probably not a problem because
  ## the package loading is not scoped either.
  use_local_library(path_library(handle$root))
  if (install) {
    install_packages_missing(c(obj$packages$attached, obj$packages$loaded),
                             sources=obj$package_sources, ...)
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
    context_log("global", "")
    deserialise_image(obj$global, envir=envir)
  }
  if (!is.null(obj$local)) {
    context_log("local", "")
    obj$local
  } else {
    envir
  }
}

##' @export
##' @rdname context
context_read <- function(handle) {
  ret <- context_db(handle)$get(handle$id, namespace="contexts")
  ## TODO: same treatment as task_read where task_read(task) -> task
  ##
  ## We'll take responsibility here for setting up the local drat
  ## links because install_packages does not know about ideas of
  ## roots; it just has the set of sources (which by this point has
  ## basically whittled down to a set of repositories which is kind
  ## of cool).
  ##
  ## Because the final drat link needs to be an absolute path, this
  ## means that wherever the context is read will get the correct
  ## local path.
  if (!is.null(ret$package_sources$local_drat)) {
    ret$package_sources$local_drat <- path_drat(handle$root)
  }
  ret
}

##' @export
##' @rdname context
contexts_list <- function(root) {
  context_db(root)$list(namespace="contexts")
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

##' @export
##' @rdname context
##' @param db The context db (used internally, and not intended for
##'   end-user use)
context_handle <- function(root, id, db=NULL) {
  structure(list(root=root, id=id, db=db), class="context_handle")
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

use_local_library <- function(lib) {
  context_log("lib", lib)
  dir.create(lib, FALSE, TRUE)
  ## This preserves all the previous libPaths; .libPaths(lib) would
  ## nuke all but the system libraries.
  .libPaths(union(lib, .libPaths()))
  invisible(lib)
}

setup_context <- function(root, type, args) {
  f_config <- path_config(root)
  if (file.exists(f_config)) {
    config <- readRDS(f_config)
    if (!is.null(type) && !identical(type, config$type)) {
      stop(sprintf("Incompatible storage types: requested %s, stored: %s",
                   type, config$type))
    }
    context_db_open(root, config, FALSE)
  } else {
    if (is.null(type)) {
      type <- "rds"
    }
    config <- list(type=type, args=args)
    saveRDS(config, f_config)
    withCallingHandlers(context_db_open(root, config, TRUE),
                        error=function(e) file.remove(f_config))
  }
}
