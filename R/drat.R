##' Specify information about locations to find non-CRAN packages.
##' This will typically be saved with a \code{context}.  As
##' part of the context generation process, any packages not in a
##' repository (CRAN or other CRAN-like repository such as a drat
##' reposotory) will be copied into a local drat repository that will
##' be available to a process that is restoring the context.
##'
##' @title Information about non-CRAN packages
##'
##' @param cran Use this to set the CRAN mirror.  The default will use
##'   the Rstudio CRAN mirror which is usually nice and fast.
##'
##' @param repos Character vector of additional reposotories to use.
##'   Specify whatever URL would normally work with
##'   \code{\link{install.packages}}.  In addition, specify URL of the
##'   form \code{drat://<username>} to install from drat repositories.
##'
##' @param local Character vector of paths to packages on your local
##'   computer to make available.  These can be directories or built
##'   \code{tar.gz} files.
##'
##' @param expire Number of days after which to expire packages in the
##'   archive.  After this, packages will be fetched again.
##'   Fractional days or \code{\link{difftime}} objects are fine.
##'
##' @param github,bitbucket Character vector of github or bitbucket
##'   packages to install.  The format is similar to
##'   \code{devtools::install_github}, being
##'   \code{username/repo[/subdir][@ref]} (note that pull requests are
##'   not supported in contrast with devtools).
##'
##' @export
package_sources <- function(cran=NULL, repos=NULL,
                            github=NULL, bitbucket=NULL, local=NULL,
                            expire=1) {
  if (is.null(cran)) {
    cran <- "http://cran.rstudio.com"
  }

  if (!is.null(repos)) {
    names(repos) <- repos
    is_drat <- grepl("^drat://", repos)
    repos[is_drat] <- sprintf("https://%s.github.io/drat/",
                              sub("^drat://", "", repos[is_drat]))
    if (!all(grepl("^[a-z]+://", repos))) {
      stop("Missing url scheme")
    }
  }

  if (!is.null(github)) {
    github <- apply(parse_package_pointer(github), 1, as.list)
  }
  if (!is.null(bitbucket)) {
    bitbucket <- apply(parse_package_pointer(bitbucket), 1, as.list)
  }
  if (!is.null(local)) {
    if (!all(file.exists(local))) {
      stop("Missing local files")
    }
    f <- function(x) {
      list(str=x, path=normalizePath(x, mustWork=TRUE))
    }
    local <- setNames(lapply(local, f), local)
  }

  structure(list(cran=cran, repos=repos,
                 github=github, bitbucket=bitbucket, local=local,
                 expire=as.difftime(1, units="days")),
            class="package_sources")
}

##' Build a local ad-hoc drat from a set of package sources.  This
##' downloads the zipball from github, bitbucket, or locates it
##' locally, builds a \emph{source} package and adds it to an ad-hoc
##' drat repository within \code{path}.  This is designed to be used
##' within \code{install_packages} and \code{context_save}, but can be
##' called directly.
##'
##' @title Build local drat repository
##' @param package_sources A \code{\link{package_sources}} object
##' @param path Path to build the drat repository at
##' @param force Re-fetch all packages, even if they still look fresh.
##' @return The \code{package_sources} object with the
##'   \code{local_drat} element set to the path of the local drat.
##'   This is suitable for passing into \code{\link{install_packages}}
##'   as it will set the drat appropriately.  If no drat repository
##'   was created this element will be \code{NULL}.
##' @export
build_local_drat <- function(package_sources, path, force=FALSE) {
  db <- storr::storr_rds(file.path(path, "timestamp"), mangle_key=TRUE)
  drat_repo_init(path)

  build <- function(t, x) {
    switch(t,
           github=build_github(x),
           bitbucket=build_bitbucket(x),
           local=build_local(x))
  }

  now <- Sys.time()
  for (t in c("github", "bitbucket", "local")) {
    src <- package_sources[[t]]
    for (i in seq_along(src)) {
      key <- names(src)[[i]]
      ok <- !force && db$exists(key) &&
        db$get(key) - now < package_sources$expire
      if (!ok) {
        pkg <- build(t, src[[i]])
        drat::insertPackage(pkg, path, commit=FALSE)
        file.remove(pkg)
        db$set(key, now)
      }
    }
  }

  ## This is needed for the corner case (now quite frequent) where no
  ## packages are added to the drat repository; the options are either
  ## to build an empty PACKAGES file (tools::write_PACKAGES) or to
  ## declare there is no local drat.
  if (file.exists(file.path(path, "src", "contrib", "PACKAGES"))) {
    package_sources$local_drat <- path
  } else {
    package_sources$local_drat <- NULL
  }
  invisible(package_sources)
}

## This comes from drat.builder, and is _largely_ compatible with devtools
parse_package_pointer <- function(x) {
  ## The format is:
  ##   <username>/<repo>[/subdir][@ref]
  re <- "^([^/]+)/([^/@#[:space:]]+)(.*)$"
  if (!all(grepl(re, x))) {
    stop("Invalid package specification")
  }

  user <- sub(re, "\\1", x)
  repo <- sub(re, "\\2", x)
  rest <- sub(re, "\\3", x)

  re_subdir <- "^(/[^@#[:space:]]*)(.*)"
  i <- grepl(re_subdir, rest)
  subdir <- rep(NA_character_, length(x))
  subdir[i] <- sub("^/", "", sub(re_subdir, "\\1", rest[i]))
  rest[i]   <- sub(re_subdir, "\\2", rest[i])

  ## In devtools, either pull request or reference allowed, but let's
  ## just not support PRs yet because I don't see a strong use for
  ## them here.
  re_ref <- "^(@[^#[:space:]]*)(.*)"
  i <- grepl(re_ref, rest)
  ref <- rep("master", length(x))
  ref[i]  <- sub("^@", "", sub(re_ref, "\\1", rest[i]))
  rest[i] <- sub(re_ref, "\\2", rest[i])

  subdir[nchar(subdir) == 0L] <- NA_character_
  ref0 <- !nzchar(ref, FALSE)
  if (any(ref0)) {
    stop("Invalid reference: ", paste(x[ref0], collapse=", "))
  }

  if (any(nzchar(trimws(rest)))) {
    stop("Invalid parse")
  }

  ret <- cbind(user=user, repo=repo, subdir=subdir, ref=ref, str=x)
  rownames(ret) <- x
  ret
}

## This is protection for the case where we read packages from a local
## drat on windows but it tries to read the binary directory and
## fails.  I'm not 100% sure that this should be needed, but it is at
## least on R 3.2.x windows.
drat_add_empty_bin <- function(path) {
  path <- contrib.url(path, "binary")
  path_PACKAGES <- file.path(path, "PACKAGES")
  if (!file.exists(path_PACKAGES)) {
    dir.create(path, FALSE, TRUE)
    writeLines(character(0), path_PACKAGES)
  }
}

drat_repo_init <- function(path) {
  dir.create(file.path(path, "src", "contrib"), FALSE, TRUE)
}

R_build <- function(path) {
  owd <- setwd(dirname(path))
  on.exit(setwd(owd))
  prev <- dir(full.names=TRUE)
  opts <- "--no-build-vignettes"
  cmd <- call_system(file.path(R.home("bin"), "R"),
                     c("--vanilla", "CMD", "build", opts, basename(path)))
  ## The normalizePath here should not be needed according to ?dir
  ## (with full.names=TRUE) but it does seem to be which is sad.
  normalizePath(setdiff(dir(full.names=TRUE), prev))
}

build_github <- function(x) {
  build_remote(github_url(x$user, x$repo, x$ref), x$subdir, x$str)
}
build_bitbucket <- function(x) {
  build_remote(bitbucket_url(x$user, x$repo, x$ref), x$subdir, x$str)
}
build_local <- function(x) {
  path <- x$path
  if (grepl("\\.tar\\.gz", path)) {
    tmp <- tempfile("context_sources_")
    dir.create(tmp)
    file.copy(path, tmp)
    file.path(tmp, basename(path))
  } else if (is_dir(path)) {
    tmp <- tempfile("context_sources_")
    dir.create(tmp)
    file.copy(path, tmp, recursive=TRUE)
    R_build(file.path(tmp, basename(path)))
  } else {
    stop("Invalid local source")
  }
}

build_remote <- function(url, subdir, str=url) {
  path <- tempfile("context_sources_")
  dir.create(path)
  dest <- file.path(path, "context.zip")
  ## TODO: Consider always printing this.
  context_log("download", str)
  download_file(url, dest, quiet=!isTRUE(getOption("context.log", FALSE)))
  unzip(dest, exdir=path)
  file.remove(dest)
  target <- dir(path, full.names=TRUE)
  if (!is.na(subdir)) {
    target <- file.path(target, subdir)
  }
  R_build(target)
}

github_url <- function(user, repo, ref) {
  sprintf("https://github.com/%s/%s/archive/%s.zip", user, repo, ref)
}
bitbucket_url <- function(user, repo, ref) {
  sprintf("https://bitbucket.com/%s/%s/get/%s.zip", user, repo, ref)
}
