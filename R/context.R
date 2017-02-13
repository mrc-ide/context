context_save <- function(path, packages = NULL, sources = NULL, auto = FALSE,
                         package_sources = NULL, envir = parent.frame(),
                         storage_type = NULL, storage_args = NULL,
                         name = NULL) {
  root <- context_root_init(path, storage_type, storage_args)
  db <- root$db
  if (!is.null(package_sources)) {
    assert_is(package_sources, "package_sources")
  }
  ret <- context_build(packages, sources, auto, package_sources, envir, name)
  id <- db$set_by_value(ret, namespace = "contexts", use_cache = FALSE)
  ret$id <- id
  db$set(ret$name, ret, namespace = "contexts_by_name")
  now <- Sys.time()
  db$set(id, now, namespace = "context_date")
  if (!db$exists(id, "context_date_created")) {
    db$set(id, now, namespace = "context_date_created")
    context_log("save:id", id)
    context_log("save:name", ret$name)
  }
  ret$root <- root
  ret$db <- db
  class(ret) <- "context"
  ret
}

context_info <- function(db, error = TRUE) {
  ids <- context_list(db, error = error)
  if (length(ids) == 0L) {
    ret <- data.frame(id = character(0), name = character(0),
                      created = empty_time(),
                      stringsAsFactors = FALSE)
  } else {
    db <- context_db_get(db)
    times <- unlist_times(db$mget(ids, "context_date_created"))
    dat <- db$mget(ids, "contexts")
    names <- vcapply(dat, "[[", "name")
    ret <- data.frame(id = ids, name = names, created = times,
                      stringsAsFactors = FALSE)
    ret <- ret[order(ret$created), ]
    rownames(ret) <- NULL
  }
  ret
}

## TODO: it might be nice to list these by time optinally, but that
## interacts badly with getting the names too, because those are not
## stored as a lookup.
context_list <- function(db, names = FALSE, error = TRUE) {
  if (error) {
    db <- context_db_get(db)
  } else {
    db <- tryCatch(context_db_get(db), error = function(e) NULL)
    if (is.null(db)) {
      return(character(0))
    }
  }
  db$list(if (names) "contexts_by_name" else "contexts")
}

##' @export
print.context <- function(x, ...) {
  print_ad_hoc(x)
}

context_read <- function(identifier, root, ..., db = NULL) {
  root <- context_root_get(root, db)
  ns <- if (is_id(identifier)) "contexts" else "contexts_by_name"
  dat <- root$db$get(identifier, ns)
  if (is_id(identifier)) {
    dat$id <- identifier
  }
  dat$root <- root
  dat$db <- root$db
  dat
}

context_load <- function(ctx, envir = .GlobalEnv, install = FALSE, ...) {
  assert_is(ctx, "context")
  assert_is(envir, "environment")
  context_log("context", ctx$id)

  if (install) {
    ## TODO: here, install *missing* packages for the current
    ## environment.  This was originally intended for the case where
    ## we'd spin things up remotely, so this is not very high
    ## priority, really.
    ##
    ## We'll let '...' be used here I think.
    ##
    ## In this case we'd be looking to do a installation into the
    ## default library and do missing packages only by default.
    stop("FIXME")
  }

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

  if (!is.null(ctx$global)) {
    context_log("global", "")
    deserialise_image(ctx$global, envir = envir)
  }

  if (!is.null(ctx$local)) {
    context_log("local", "")
    ctx$local
  } else {
    envir
  }
}

################################################################################
## internals

context_build <- function(packages, sources, auto, package_sources, envir,
                          name) {
  name <- context_name(name)
  if (auto) {
    if (!is.null(packages) || !is.null(sources)) {
      stop("Do not specify 'packages' or 'sources' if using auto")
    }
    ret <- list(
      name = name,
      packages = detect_loaded_packages(),
      global = serialise_image())
  } else {
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
    ret <- list(name = name,
                packages = packages)
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

  if (!is.null(package_sources)) {
    assert_is(package_sources, "package_sources")
  }
  ret$package_sources <- package_sources

  if (!is.GlobalEnv(envir)) {
    ## TODO: I don't think that this is a good idea, and does not
    ## match how this is being used.  OTOH, some sort of ability to
    ## *properly* export environments will be useful for Jeff's case.
    ret$local <- envir
  }
  ret$auto <- auto
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

## This is going to be horrid to test because it really requires
## tweaking the search path and dealing with that terrible way that
## the R CMD check tests work.  Better would be to mock sessionInfo
## data so that we can create a few sensible mock ups and send that
## through.
detect_loaded_packages <- function(info = sessionInfo()) {
  loaded_only <- info[["loadedOnly"]]

  loaded <- names(loaded_only)[!vlapply(loaded_only, function(x)
    identical(x$Priority, "base"))]
  attached <- names(info[["otherPkgs"]])
  ## This is defensive: ?sessionInfo does not make any guarantees
  ## about package loading.
  ord <- sub("^package:", "", search())
  attached <- attached[rank(match(attached, ord))]
  list(loaded = loaded, attached = attached)
}

## Shortest hashes in storr are 32 characters
is_id <- function(x) {
  grepl("^[[:xdigit:]]{32,}$", x, perl = TRUE)
}
