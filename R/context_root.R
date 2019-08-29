##' Find the context root.  Designed for internal use
##' @title Find context root
##' @param root An object; either a character string (interpreted as a
##'   path), a \code{context_root} object (such as returned by this
##'   function) or a list/environment object with a \code{root}
##'   element that is a \code{context_root} object.
##' @param db Optionally, a copy of the storr database (if already
##'   opened).  Do not specify this unless you \emph{definitely} have
##'   the correct database in hand.
##' @export
context_root_get <- function(root, db = NULL) {
  if (is.character(root)) {
    root <- context_root(root, db)
  } else if (inherits(root, "context_root")) {
  } else if (is.recursive(root) && inherits(root$root, "context_root")) {
    ## who uses this branch now?
    root <- root$root
  } else {
    stop("Invalid context root")
  }
  root
}

context_root_init <- function(path, storage_type = NULL, storage_args = NULL,
                              id = NULL) {
  fv <- path_version(path)
  written <- package_version(if (file.exists(fv)) readLines(fv) else "0.0")
  installed <- packageVersion("context")
  if (is.na(installed) || written < installed) {
    if (!dir.exists(path)) {
      dir.create(path, FALSE, TRUE)
      writeLines(as.character(packageVersion("context")), fv)
    }
    write_scripts(path)
  } else if (written > installed) {
    stop("context version conflict; local is outdated")
  }
  db <- context_db_init(path, storage_type, storage_args, id)
  root <- context_root(path, db)
  if (written > numeric_version("0.0") && written < numeric_version("0.1.0")) {
    context_upgrade_0_1_0(root)
  }
  root
}

context_db_get <- function(root) {
  if (inherits(root, "storr")) {
    root
  } else if (is.recursive(root) && inherits(root$db, "storr")) {
    root$db
  } else {
    context_root_get(root)$db
  }
}

context_db_init <- function(path, type, args, id = NULL) {
  if (!is.null(id)) {
    assert_scalar_character(id)
  }
  f_id <- path_id(path)
  f_config <- path_config(path)
  if (file.exists(f_id)) {
    if (!is.null(id)) {
      prev <- readLines(f_id)
      if (!identical(id, prev)) {
        stop(sprintf("Given id '%s' and stored id '%s' differ", id, prev),
             call. = FALSE)
      }
    }
    config <- readRDS(f_config)
    if (!is.null(type) && !identical(type, config$type)) {
      config_type <- if (is.function(config$type)) "user" else config$type
      stop(sprintf("Incompatible storage types: requested %s, stored: %s",
                   type, config_type))
    }
    if (!is.null(args)) {
      v <- union(names(config$args), names(args))

      f <- function(x) {
        ## TODO: this will not work well on vector arguments if
        ## anything takes them...
        existing <- config$args[[x]] %||% "<NULL>"
        given <- args[[x]] %||% "<NULL>"
        if (identical(existing, given)) {
          ""
        } else {
          sprintf("\n\t%s: existing: %s, given: %s", x, existing, given)
        }
      }
      res <- vcapply(v, f)
      res <- res[nzchar(res)]
      if (length(res) > 0L) {
        warning("Ignoring incompatible storage_args:",
                paste(res, collapse = ""), immediate. = TRUE)
      }
    }
    db <- context_db_open(path, config, FALSE)
  } else {
    id <- id %||% ids::random_id()
    context_log("init:id", id)
    writeLines(id, f_id)
    ## TODO: do some sanity checking here; 'type' must be a function or string
    ##
    ## This odd construction means that if connecting to the database
    ## fails we're not left in an inconsistent state with a corrupt
    ## context configuration that can't be used.
    config <- list(type = type %||% "rds", args = args)
    db <- withCallingHandlers(context_db_open(path, config, TRUE),
                              error = function(e) file.remove(f_id))
    saveRDS(config, f_config)
    context_log("init:path", path)
    if (is.recursive(config$type)) {
      driver_packages <- config$type$packages
    } else {
      driver_packages <- NULL
    }
    db$set("driver_packages", driver_packages, "context_root")
  }
  db
}

context_db_open <- function(path, config, create) {
  if (is.null(config)) {
    create <- FALSE
    config <- readRDS(path_config(path))
  }
  if (is.character(config$type)) {
    if (config$type == "rds") {
      driver <- storage_driver_rds()
    } else if (config$type == "environment") {
      driver <- storage_driver_environment()
    } else {
      stop(sprintf("Unsupported storage type '%s'", config$type))
    }
  } else {
    assert_is(config$type, "context_storage_driver", "storage_type")
    driver <- config$type
  }
  id <- readLines(path_id(path))
  context_log(sprintf("%s:db", if (create) "init" else "open"), driver$name)
  db <- driver$create(path, id, config$args)
  ## This will allow checking when things were created and last used
  k <- c("opened", if (create) "created")
  db$mset(k, rep(list(Sys.time()), length(k)), "context_root")
  db
}

context_root <- function(path, db = NULL) {
  if (!file.exists(path)) {
    stop("Context root not set up at ", path)
  }
  id <- readLines(path_id(path))
  if (is.null(db)) {
    db <- context_db_open(path, NULL, NULL)
  }
  ret <- list(id = id, path = path, db = db)
  class(ret) <- "context_root"
  ret
}

##' @export
print.context_root <- function(x, ...) {
  print_ad_hoc(x)
}

context_upgrade_0_1_0 <- function(root) {
  message("HEY THERE! Your context DB is old!")
  message("I *strongly* recommend stopping now and using a new root path")

  if (length(root$db$list("name_by_context")) == 0L) {
    message("Attempting to upgrade your context directory")
    db <- root$db
    ids <- db$list("contexts")
    if (length(ids > 0L)) {
      nms <- vcapply(seq_along(ids), function(.) context_name(NULL))
      db$mset(ids, nms, "name_by_context")
      db$mset(nms, ids, "context_by_name")

      db$mset(ids, db$mget(ids, "context_date"), "context_date_created")

      tasks <- db$list("tasks")
      if (length(tasks) > 0L) {
        if (length(ids) != 1L) {
          task_context <- vcapply(db$mget(tasks, "tasks"), "[[", "context_id")
        } else {
          task_context <- rep(task_context, length(tasks))
        }
        db$mset(tasks, task_context, "task_context")
      }

      bundles <- db$list("task_bundles")
      if (length(bundles) > 0L) {
        bundles_id <- db$mget(bundles, "task_bundles")
        bundles_homogeneous <- vnapply(bundles_id, function(x)
          length(unique(task_function_name(x, db)))) == 1
        db$mset(bundles, bundles_homogeneous, "bundles_homogeneous")
      }
    }
    message("Things should work now, but treat this as read only please!")
  }
}
