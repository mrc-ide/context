context_root_init <- function(path, storage_type = NULL, storage_args = NULL) {
  fv <- path_version(path)
  written <- package_version(if (file.exists(fv)) readLines(fv) else "0.0")
  installed <- packageVersion("context")
  init <- is.na(installed) || written < installed
  if (is.na(installed) || written < installed) {
    if (!dir.exists(path)) {
      dir.create(path, FALSE, TRUE)
      writeLines(as.character(packageVersion("context")), fv)
    }
  } else if (written > installed) {
    stop("context version conflict; local is outdated")
  }
  db <- context_db_init(path, storage_type, storage_args)
  context_root(path, db)
}

context_root_get <- function(root, db = NULL) {
  if (is.character(root)) {
    root <- context_root(root, db)
  } else if (inherits(root, "context_root")) {
  } else if (is.recursive(root) && inherits(root$root, "context_root")) {
    root <- root$root
  } else {
    stop("Invalid context root")
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

context_db_init <- function(path, type, args) {
  f_config <- path_config(path)
  if (file.exists(f_config)) {
    config <- readRDS(f_config)
    if (!is.null(type) && !identical(type, config$type)) {
      stop(sprintf("Incompatible storage types: requested %s, stored: %s",
                   type, config$type))
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
    ## This odd construction means that if connecting to the database
    ## fails we're not left in an inconsistent state with a corrupt
    ## context configuration that can't be used.
    config <- list(type = type %||% "rds", args = args)
    saveRDS(config, f_config)
    db <- withCallingHandlers(context_db_open(path, config, TRUE),
                              error = function(e) file.remove(f_config))
    ## Need a better name for this; context/environment makes a better
    ## naming system but that's disruptive now.
    id <- ids::random_id()
    db$set("id", id, "_context")
    context_log("init", paste("initialised:", id))
  }
  db
}

context_db_open <- function(path, config, create) {
  if (is.null(config)) {
    create <- FALSE
    config <- readRDS(path_config(path))
  }
  if (config$type == "environment") {
    if (!create) {
      stop("Cannot reconnect to environment storage")
    }
    ret <- storr::storr_environment()
  } else if (config$type == "rds") {
    defaults <- list(compress = FALSE, mangle_key = TRUE)
    v <- intersect(names(defaults), names(config$args))
    args <- c(list(path_db(path)),
              modifyList(defaults, as.list(config$args[v])))
    ret <- do.call(storr::storr_rds, args, quote = TRUE)
  } else {
    ## This is actually a little more difficult than this because
    ## we need to add any required packages (e.g., redux) to the
    ## bootstrap script.  That's also going to generate some
    ## issues with blowing out 'Suggests:' in the package
    ## perhaps.  For now leave this be.
    ## redis = storr::storr_redis_api(redux::redis(config = config$args)),
    ##
    ## TODO: on any non-RDS storage we'll want to add information
    ## about additional dependencies needed
    stop("Unsupported storage type ", config$type)
  }
  ret
}

context_root <- function(path, db = NULL) {
  if (!file.exists(path)) {
    stop("Context root not set up at ", path)
  }
  if (is.null(db)) {
    db <- context_db_open(path, NULL, NULL)
  }
  ret <- list(path = path, db = db)
  class(ret) <- "context_root"
  ret
}

context_root_path <- function(x) {
  if (inherits(x, "context_root")) {
    x$path
  } else if (is.recursive(x) && inherits(x$root, "context_root")) {
    x$root$path
  } else {
    stop("can't find path")
  }
}

##' @export
print.context_root <- function(x, ...) {
  print_ad_hoc(x)
}
