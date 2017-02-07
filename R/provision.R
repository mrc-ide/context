##' Provision a context
##' @title Provision a context
##'
##' @param ctx A context object.  We'll look in here for packages to install.
##'
##' @param platform Platform to create the library for
##'
##' @param version Version of R to create the library for
##'
##' @param quiet Be quiet when installing source packages
##'
##' @param allow_missing Allow packages requiring compilation to be
##'   skipped over?  If \code{TRUE} you'll need to sort these out
##'   yourself.
##'
##' @param installed_action Action if packages are installed
##'
##' @export
provision_context <- function(ctx, platform = NULL, version = NULL,
                              quiet = FALSE, allow_missing = FALSE,
                              installed_action = "skip") {
  loadNamespace("provisionr")
  path_root <- ctx$root$path
  ## TODO: this needs to be fixed to get things from the appropriate
  ## place; the dide-tools drat will be a decent spot I think.
  url_context <-
    paste0("file://", normalizePath("~/Documents/Projects/epi/cluster/drat"))
  if (is.null(ctx$package_sources)) {
    src <- provisionr::package_sources(repos = url_context)
  } else {
    src <- ctx$package_sources$clone()
    src$repos <- c(url_context, src$repos)
  }

  if (!is.null(src)) {
    src$build(src$local_drat %||% path_drat(path_root))
  }

  ## TODO: when using non-disk storage, queuers, etc, this will need
  ## updating.
  packages <- c("context", ctx$packages$attached, ctx$packages$loaded)

  path_lib <- path_library(path_root, platform, version)
  installed_action <- "skip"

  context_log("provision", sprintf("library at %s", path_lib))
  res <- provisionr::provision_library(
    packages, path_lib, platform = platform, version = version, src = src,
    check_dependencies = TRUE, installed_action = installed_action,
    allow_missing = allow_missing, quiet = quiet)

  invisible(res)
}
