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
    local <- setNames(normalizePath(local, mustWork=TRUE), local)
  }

  use_local_drat <- (length(github) > 0 ||
                     length(bitbucket) > 0 ||
                     length(local) > 0)

  structure(list(cran=cran, repos=repos,
                 github=github, bitbucket=bitbucket, local=local,
                 use_local_drat=use_local_drat,
                 expire=as.difftime(1, units="days")),
            class="package_sources")
}

##' Build a local ad-hoc drat from a set of package sources.  This
##' downloads the zipball from github, bitbucket, or locates it
##' locally, builds a \emph{source} package and adds it to an ad-hoc
##' drat repository within \code{root}.  This is designed to be used
##' within \code{install_packages} and \code{save_context}, but can be
##' called directly.
##'
##' @title Build local drat repository
##' @param sources A \code{\link{package_sources}} object
##' @param root Root directory to store and retrieve files.  Files
##'   will be added to the \code{contexts} subdirectory of this path.
##'   This will change later to support alternative ways of saving
##'   files.  The default puts files into the temp directory of
##'   \emph{this} R instance; this directory will be deleted on
##'   session exit so it not an appropriate place to store files for
##'   later use.
##' @param force Re-fetch all packages, even if they still look fresh.
##' @param quiet Quieten the download progress (which can be quite messy)
##' @export
build_local_drat <- function(sources, root, force=FALSE, quiet=TRUE) {
  if (sources$use_local_drat) {
    path <- path_drat(root)
    timestamp_file <- file.path(path, "timestamp.rds")
    if (file.exists(timestamp_file) && !force) {
      src <- timestamp_filter(sources, readRDS(timestamp_file))
    } else {
      src <- sources
      timestamp <- list()
    }
    packages <- c(vcapply(src$github,    build_github,    quiet),
                  vcapply(src$bitbucket, build_bitbucket, quiet),
                  vcapply(src$local,     build_local))
    if (length(packages) > 0L) {
      context_log("drat", path)
      repo_init(path)
      for (p in packages) {
        drat::insertPackage(p, path, commit=FALSE)
      }
      saveRDS(timestamp_merge(timestamp, timestamp_create(src)),
              timestamp_file)
    }
    ## This is needed outside.
    sources$local_drat <- path
  }
  sources
}

## A pretty rubbish attempt at making the package source bits a little
## faster.  It's really tricky because there is no way of knowing if
## upstream has changed; OTOH, if we're firing off lots of jobs then
## this is probably a bit daft.  Better might be to check against the
## *local* version and update if the stored version is behind the
## local version.  That'd be nice and quick actually.  This will do
## for now.
##
## This is mostly complicated because there's no way in general of
## linking the string we are given with the actual package information
## without unzipping the archive again and getting the DESCRIPTION
## out.
timestamp_names <- function() {
  c("github", "bitbucket", "local")
}

timestamp_create <- function(sources) {
  time <- Sys.time()
  f <- function(x) {
    setNames(rep(time, length.out=length(x)), names(x))
  }
  lapply(sources[timestamp_names()], f)
}

timestamp_merge <- function(x, y) {
  for (i in timestamp_names()) {
    x[[i]] <- c(x[[i]], y[[i]])
  }
  x
}

timestamp_filter <- function(obj, t) {
  dt <- obj$expire
  for (i in timestamp_names()) {
    drop <- names(which(t[[i]] > dt))
    obj[[i]] <- obj[[i]][setdiff(names(obj[[i]]), drop)]
  }
  obj
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

repo_init <- function(path) {
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

build_github <- function(x, quiet=FALSE) {
  build_remote(github_url(x$user, x$repo, x$ref), x$subdir, quiet, x$str)
}
build_bitbucket <- function(x, quiet=FALSE) {
  build_remote(bitbucket_url(x$user, x$repo, x$ref), x$subdir, quiet, x$str)
}
build_local <- function(x) {
  if (grepl("\\.tar\\.gz", x)) {
    x
  } else if (is_dir(x)) {
    path <- tempfile("context_sources_")
    dir.create(path)
    file.copy(x, path, recursive=TRUE)
    R_build(file.path(path, basename(x)))
  } else {
    stop("Invalid local source")
  }
}

build_remote <- function(url, subdir, quiet=FALSE, str=url) {
  path <- tempfile("context_sources_")
  dir.create(path)
  dest <- file.path(path, "context.zip")
  context_log("download", str)
  download_file(url, dest, quiet=quiet)
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
