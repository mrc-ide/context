provision_context <- function(ctx, platform = NULL, version = NULL,
                              quiet = FALSE, allow_missing = FALSE) {
  loadNamespace("provisionr")
  ## TODO: this needs to be fixed to get things from the appropriate
  ## place; the dide-tools drat will be a decent spot I think.
  url_context <-
    paste0("file://", normalizePath("~/Documents/Projects/epi/cluster/drat"))
  if (is.null(ctx$package_sources)) {
    src <- provisionr::package_sources(repos = url_context)
  } else {
    src <- ctx$package_sources$clone()
    src$repos <- c(url_context, src$repos)
    stop("FIXME")
  }

  if (!is.null(src) && src$needs_build()) {
    ## Perhaps use an existing drat file if one exists already?
    path_drat <- src$path_drat %||% path_drat(context$root$path)
    src$build(file.path(path_drat, "drat"))
  }

  packages <- c("context", ctx$packages$attached, ctx$packages$loaded)
  path_lib <- path_library(path)
  installed_action <- "skip"

  res <- provisionr::provision_library(
    packages, path_lib, platform = platform, version = version, src = src,
    check_dependencies = TRUE, installed_action = installed_action,
    allow_missing = allow_missing, quiet = quiet)

  res$missing
}
