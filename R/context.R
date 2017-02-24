##' Save a context
##' @title Save a context
##'
##' @param path Path to save the context in
##'
##' @param packages Optional character vector of packages to save into
##'   the context.  Alternatively, can be a list with elements
##'   \code{loaded} and \code{attached} if you want to ensure some
##'   packages are loaded but not attached.
##'
##' @param sources Character vector of source files to read in.  These
##'   should define functions and (perhaps) other "global" objects,
##'   but should not do any serious computation.
##'
##' @param package_sources Optional information about where to find
##'   non-CRAN packages.
##'
##' @param envir The current environment.  This is used to copy
##'   \emph{local} variables around.  For \code{context_load} this is
##'   the environment into which the global environment is copied.
##'   Specify a non-global environment here to avoid clobbering the
##'   workspace, but at the risk that some environments may not
##'   restore exactly as desired.
##'
##' @param storage_type Character vector indicating the storage type
##'   to use.  Options are \code{"rds"} (the default) and
##'   \code{"environment"} (for testing and local use).
##'
##' @param storage_args Arguments passed through to the storage driver
##'
##' @param name An optional name for the context.  This will be
##'   printed with the context in some situations (such as
##'   \code{\link{context_info}})
##'
##' @param unique_value A unique value to add to your context to
##'   distinguish it from other contexts with otherwise identical
##'   contents.  This is mostly useful in conjunction with \code{rrq}
##'   where in a multiuser setting you might end up with two people
##'   creating identical contexts and therefore sharing a queue!  By
##'   adding a unique value (your name, a number, whatever) the
##'   generated context ID will be arbitrarily different and you'll
##'   avoid collisions.  Any R object can be used here (string,
##'   number, vector, whatever).  Your username would be a good
##'   choice.
##'
##' @export
context_save <- function(path, packages = NULL, sources = NULL,
                         package_sources = NULL, envir = NULL,
                         storage_type = NULL, storage_args = NULL,
                         name = NULL, unique_value = NULL) {
  root <- context_root_init(path, storage_type, storage_args)
  db <- root$db
  if (!is.null(package_sources)) {
    assert_is(package_sources, "package_sources")
  }
  ret <- context_build(packages, sources, package_sources,
                       unique_value, envir)
  driver_packages <- db$get("driver_packages", "context_root")
  if (!is.null(driver_packages) > 0L) {
    if (!driver_packages %in% unlist(ret$packages)) {
      ret$packages$loaded <- c(ret$packages$loaded, driver_packages)
    }
  }
  id <- db$set_by_value(ret, namespace = "contexts", use_cache = FALSE)

  ## Then we'll create a pair of 1:1 mappings for the context
  ##   context_by_name
  ##   name_by_context
  if (is.null(name) && db$exists(id, "name_by_context")) {
    name <- db$get(id, "name_by_context")
  } else {
    name <- context_name(name)
    db$set(id, name, "name_by_context")
  }
  db$set(name, id, "context_by_name")

  now <- Sys.time()
  db$set(id, now, namespace = "context_date")
  if (!db$exists(id, "context_date_created")) {
    db$set(id, now, namespace = "context_date_created")
    context_log("save:id", id)
    context_log("save:name", name)
  }

  ret$id <- id
  ret$name <- name
  ret$root <- root
  ret$db <- db

  class(ret) <- "context"
  ret
}

##' List saved contexts
##' @title List save contexts
##'
##' @param db Something for which a context database can be created;
##'   this can the the path to the context, a \code{context_root}
##'   object, or a \code{context} object.
##'
##' @param named Logical, indicating if the context name should be used
##'   to name the output vector.
##'
##' @param error Throw an error if the context database cannot be
##'   connected constructed (e.g., if the path given does not exist).
##'
##' @export
##' @author Rich FitzJohn
context_list <- function(db, named = FALSE, error = TRUE) {
  dat <- context_info(db, error)
  id <- dat$id
  if (named) {
    names(id) <- dat$name
  }
  id
}

##' @export
##' @rdname context_list
context_info <- function(db, error = TRUE) {
  ## TODO: it might be nice to list these by time optinally, but that
  ## interacts badly with getting the names too, because those are not
  ## stored as a lookup.
  if (error) {
    db <- context_db_get(db)
    id <- db$list("contexts")
  } else {
    db <- tryCatch(context_db_get(db), error = function(e) NULL)
    if (is.null(db)) {
      id <- character(0)
    }
  }

  if (length(id) == 0L) {
    name <- character(0)
    time <- empty_time()
  } else {
    name <- vcapply(db$mget(id, "name_by_context"), identity)
    time <- unlist_times(db$mget(id, "context_date_created"))
  }
  ret <- data.frame(id = id, name = name, created = time,
                    stringsAsFactors = FALSE)
  ret <- ret[order(ret$created), ]
  rownames(ret) <- NULL
  ret
}

##' @export
print.context <- function(x, ...) {
  print_ad_hoc(x)
}

##' Read a context
##' @title Read a context
##' @param identifier Either the id or name of a context (see
##'   \code{\link{context_list}})
##' @param root Something interpetable as the context root; either
##' @param db Optionally, a database (if known already)
##' @export
context_read <- function(identifier, root, db = NULL) {
  root <- context_root_get(root, db)
  db <- root$db

  if (is_id(identifier)) {
    id <- identifier
    name <- db$get(identifier, "name_by_context")
  } else {
    id <- db$get(identifier, "context_by_name")
    name <- identifier
  }

  dat <- root$db$get(id, "contexts")

  dat$id <- id
  dat$name <- name
  dat$root <- root
  dat$db <- root$db

  dat
}

##' Load a context
##' @title Load a context
##' @param ctx A context object, as read by \code{\link{context_read}}
##'
##' @param envir The environment to source files into
##'
##' @param refresh Refresh the context, even when it has been loaded
##'   already?  Note that this may not always behave as expected
##'   because items not created by sourcing an R file will still be
##'   there from previous runs, and packages loaded will not be
##'   reloaded in a new order.
##' @export
context_load <- function(ctx, envir = .GlobalEnv, refresh = FALSE) {
  assert_is(ctx, "context")
  assert_is(envir, "environment")
  if (!is.null(ctx$envir) && !refresh) {
    ## There's an issue here where refresh will not work quite as
    ## expected because we might slurp into the global environment in
    ## which spare things might be left over if the contexts are not
    ## additive.
    return(ctx)
  }
  context_log("context", ctx$id)

  ## TODO: here, install *missing* packages for the current
  ## environment.  This was originally intended for the case where
  ## we'd spin things up remotely, so this is not very high
  ## priority, really.
  ##
  ## We'll let '...' be used here I think.
  ##
  ## In this case we'd be looking to do a installation into the
  ## default library and do missing packages only by default.

  context_log("library", paste0(ctx$packages$attached, collapse = ", "))
  for (p in rev(setdiff(ctx$packages$attached, .packages()))) {
    library(p, character.only = TRUE)
  }
  context_log("namespace", paste0(ctx$packages$loaded, collapse = ", "))
  for (p in rev(setdiff(ctx$packages$loaded, loadedNamespaces()))) {
    loadNamespace(p)
  }

  context_log("source", paste0(ctx$sources, collapse = ", "))
  for (s in ctx$sources) {
    source(s, envir)
  }

  ## I am really not sure tht this is a sensible thing to be doing.
  ## The context should probably not capture the local environment on
  ## save?
  if (!is.null(ctx$local)) {
    context_log("local", "")
    ctx$envir <- ctx$local
  } else {
    ctx$envir <- envir
  }

  context_cache$last_loaded_context <- ctx

  ctx
}

##' Return the last loaded context
##' @title Return last loaded context
##' @param error Throw an error if no context has been loaded
##' @export
last_loaded_context <- function(error = TRUE) {
  ret <- context_cache$last_loaded_context
  if (error && is.null(ret)) {
    stop("No context has been loaded")
  }
  ret
}

################################################################################
## internals

context_build <- function(packages, sources, package_sources,
                          unique_value, envir) {
  if (is.null(packages)) {
    packages <- character(0)
  }
  if (is.character(packages)) {
    packages <- list(attached = packages, loaded = character(0))
  } else if (is.list(packages)) {
    unk <- setdiff(names(packages), c("loaded", "attached"))
    if (length(unk) > 0L) {
      stop("Unknown names for 'packages': ", paste(unk, collapse = ", "))
    }
    if (!all(vlapply(packages, is.character))) {
      stop("All elements of 'packages' must be a character vector")
    }
    packages <- modifyList(list(attached = character(0),
                                loaded = character(0)),
                           packages)
  } else {
    stop("Incorrect type for 'packages'")
  }
  ret <- list(packages = packages)
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

  if (!is.null(package_sources)) {
    assert_is(package_sources, "package_sources")
  }
  ret$package_sources <- package_sources
  ret$unique_value <- unique_value

  if (!is.null(envir) && !is.GlobalEnv(envir)) {
    ## TODO: this section needs careful testing!
    assert_is(envir, "environment")
    ## TODO: I don't think that this is a good idea, and does not
    ## match how this is being used.  OTOH, some sort of ability to
    ## *properly* export environments will be useful for Jeff's case.
    ret$local <- envir
  }
  class(ret) <- "context"
  ret
}

context_name <- function(name) {
  if (is.null(name)) {
    name <- ids::adjective_animal()
  } else {
    assert_scalar_character(name)
    if (is_id(name)) {
      stop("name cannot be an id")
    }
  }
  name
}

## Shortest hashes in storr are 32 characters
is_id <- function(x) {
  grepl("^[[:xdigit:]]{32,}$", x, perl = TRUE)
}
