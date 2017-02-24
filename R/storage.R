storage_driver <- function(name, create, packages = NULL) {
  if (!is.null(packages)) {
    assert_character(packages)
  }
  ret <- list(name = name,
              create = create,
              packages = packages)
  class(ret) <- "context_storage_driver"
  ret
}

storage_driver_rds <- function() {
  storage_driver("rds", function(path, id, args) {
    storr::storr_rds(path_db(path),
                     compress = args$compress %||% FALSE,
                     mangle_key = args$mangle_key %||% TRUE)
  })
}

storage_driver_environment <- function() {
  storage_driver("environment", function(path, id, args) {
    if (file.exists(path_config(path))) {
      stop("Cannot reconnect to environment storage")
    }
    storr::storr_environment()
  })
}
