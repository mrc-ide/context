## Cross-install packages
##
## This is not aiming to be comprehensive.  Instead we'll make it easy
## to blow the whole thing away and start again.
##
## I'll also offer an "install on cluster" task that will run a task
## on the cluster that simply runs sessionInfo().

##' Support for cross installing packages (e.g., installing windows
##' packages into a library while running on Linux).  This is
##' experimental and there are rather a lot of corner cases that
##' aren't covered.
##'
##' @title Cross install packages
##'
##' @param lib A library to install into.  \emph{Must} be specified
##'   (you do not want to install these into your usual library!)
##'
##' @param platform The platform to target.  Must be one of "windows",
##'   "macosx" or "macosx/mavericks"
##'
##' @param r_version The R version to target, of form "X.Y" (ignores
##'   patch version).
##'
##' @param repos Repositories to use.  This must be provided at present.
##'
##' @param packages Vector of packages to install
##'
##' @export
cross_install_packages <- function(lib, platform, r_version, repos, packages) {
  ## TODO: Coming here in Windows this does the wrong thing because we
  ## *are* using the library by this point; but that gets skipped over
  ## because we don't normalise the path here.
  if (lib %in% .libPaths()) {
    stop("Do not use cross_install_packages to install into current library")
  }
  ## NOTE: No Linux support because there is no Linux binary
  ## repository.
  platform <- match.arg(platform, c("windows", "macosx", "macosx/mavericks"))
  ok <- is.character(r_version) &&
    length(r_version) == 1L &&
    grepl("^[0-9]+\\.[0-9]+$", r_version)
  if (!ok) {
    stop("r_version must be scalar chararacter of for X.Y")
  }

  packages <- setdiff(packages, base_packages())

  dir.create(lib, FALSE, TRUE)
  installed <- .packages(TRUE, lib)
  to_check <- intersect(packages, .packages(TRUE, lib))
  upgrade <- to_check[vlapply(to_check, function(p)
    packageVersion(p, lib) < packageVersion(p))]
  if (length(upgrade) > 0L) {
    context_log("upgrade", paste(upgrade, collapse=", "))
    unlink(file.path(lib, upgrade), recursive=TRUE)
    installed <- .packages(TRUE, lib)
  }

  if (all(packages %in% installed)) {
    if (length(packages) > 0L) {
      msg <- "Packages already installed"
    } else {
      msg <- "No packages to install"
    }
    context_log("cross", msg)
    return()
  }

  ## This needs help to get the context windows binary; I should
  ## probably just arrange to build it in drat.builder, I guess.  That
  ## could even fire off and use the win.builder interface.

  ## For now, don't stress about context; I'll get that in the second
  ## pass.

  ## OK, this is going to require help.  Now, one thing we *can* do is
  ## inspect the local dependencies because they are the same I hope.
  context_log("cross", "checking available packages")

  ## TODO: probably this should be *local* drat repos only.
  file_repo <- grepl("^file://", repos)
  if (any(file_repo)) {
    url <- contrib_url(repos[file_repo], platform, r_version)
    if (is_windows()) {
      path <- sub("^file:///", "", url)
    } else {
      path <- sub("^file://", "", url)
    }
    drat_add_empty_bin(path)
  }
  pkgs_bin <- available.packages(contrib_url(repos, platform, r_version))
  pkgs_src <- available.packages(contrib_url(repos, "src", NULL))

  extra <- setdiff(rownames(pkgs_bin), rownames(pkgs_src))
  if (length(extra) > 0L) {
    pkgs_cmb <- rbind(pkgs_src, pkgs_bin[extra, ])
  } else {
    pkgs_cmb <- pkgs_src
  }

  msg <- setdiff(packages, pkgs_cmb[, "Package"])
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for: %s",
                 paste(msg, collapse=", ")))
  }

  deps <- recursive_deps(packages, pkgs_cmb)
  packages <- setdiff(deps, installed)
  msg <- setdiff(packages, pkgs_cmb[, "Package"])
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for dependencies: %s",
                 paste(msg, collapse=", ")))
  }

  ## Install all the binary packages
  packages_bin <- intersect(packages, pkgs_bin[, "Package"])
  packages_src <- setdiff(packages, packages_bin)
  if (length(packages_bin) > 0L) {
    context_log("cross",
                paste("Downloading binary packages for:",
                      paste(packages_bin, collapse=", ")))
    lapply(packages_bin, cross_install_package, pkgs_bin, lib, TRUE, platform)
  }

  if (length(packages_src) > 0L) {
    context_log("cross",
                paste("Cross installing source packages for:",
                      paste(packages_src, collapse=", ")))
    j <- match(packages_src, pkgs_src[, "Package"])
    k <- pkgs_src[j, "NeedsCompilation"] == "yes"
    if (any(k)) {
      stop("Packages need compilation; cannot cross-install: ",
           paste(packages_src[k], collapse=", "))
    }
    lapply(packages_src, cross_install_package, pkgs_src, lib, FALSE, platform)
  }
}

##' @export
##' @param root Context root
##' @rdname cross_install_packages
cross_install_bootstrap <- function(lib, platform, r_version, root=NULL) {
  repos <- c(CRAN="https://cran.rstudio.com")
  path_local_drat <- path_drat(root)
  context_local <-
    !is.null(root) &&
    length(dir(file.path(path_local_drat, "src/contrib"),
               "^context_.*\\.tar\\.gz")) > 0L
  if (context_local) {
    repos[["context"]] <- path_local_drat
  } else {
    repos[["context"]] <- "https://richfitz.github.io/drat/"
  }

  cross_install_packages(lib, platform, r_version, repos, "context")
}

##' @export
##' @rdname cross_install_packages
##' @param context A context handle
cross_install_context <- function(lib, platform, r_version, context) {
  root <- context::context_root(context)
  cross_install_bootstrap(lib, platform, r_version, root)
  packages <- unlist(context$packages, use.names=FALSE)
  repos <- context_repos(context$package_sources)
  cross_install_packages(lib, platform, r_version, repos, packages)
}

cross_install_package <- function(package, dat, lib, binary, platform) {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE))

  x <- as.list(dat[match(package, dat[, "Package"]), ])
  ext <- if (!binary) "tar.gz" else if (platform == "windows") "zip" else "tgz"
  url <- sprintf("%s/%s_%s.%s", x$Repository, x$Package, x$Version, ext)
  path <- file.path(tmp, basename(url))
  download.file(url, path)
  if (binary) {
    unzip(path, exdir=lib)
  } else {
    untar(path, exdir=tmp)
    dir.create(lib, FALSE, TRUE)
    file.remove(path)
    path <- file.path(tmp, x$Package)
    lib <- normalizePath(lib, "/")
    ## We need to install the package into a temporary library and
    ## move it into place because otherwise preparing the package for
    ## lazy loading will cause issues if the package triggers loading
    ## a package with compiled dependencies.
    lib_tmp <- tempfile("cross_tmp_lib", tmpdir=lib)
    dir.create(lib_tmp)
    on.exit(try(unlink(lib_tmp, recursive=TRUE), silent=TRUE))
    env <- c(R_LIBS_USER=paste(c(.libPaths()), collapse=.Platform$path.sep),
             CYGWIN = "nodosfilewarning")
    env <- sprintf("%s=%s", names(env), unname(env))
    args <- c("CMD", "INSTALL", "--no-test-load",
              paste0("--library=", shQuote(lib_tmp)),
              shQuote(normalizePath(path)))
    ok <- system2(file.path(R.home(), "bin", "R"), args, env=env)
    if (ok != 0L) {
      stop(sprintf("Command failed (code: %d)", ok))
    }
    file.rename(file.path(lib_tmp, x$Package), file.path(lib, x$Package))
    ## This is causing problems that seem surprising to me, possibly
    ## due to things like the way that resource releasing is happening
    ## on windows over SMB?
    if (!is_windows()) {
      file.remove(lib_tmp)
      on.exit()
    }
  }
}

base_packages <- function() {
  rownames(installed.packages(priority=c("base", "recommended")))
}

parse_deps <- function(x) {
  ## TODO: This does not support returning version numbers (so
  ## depending on particular versions of packages is not going to work
  ## here).
  ##
  ## Somewhere I had the version parsing thing; I will need that back
  ## soon.  For now this just strips version information entirely.
  val <- unlist(strsplit(x, ","), use.names=FALSE)
  val <- gsub("(\\s|\\().*", "", trimws(val))
  val[val != "R"]
}

contrib_url <- function(repo, platform, r_version) {
  ## platform should be:
  ##   src
  ##   windows
  ##   macosx
  ##   macosx/mavericks
  if (platform == "src") {
    path <- "src/contrib"
  } else {
    path <- file.path("bin", platform, "contrib", r_version)
  }
  file.path(sub("/$", "", repo), path)
}

recursive_deps <- function(x, db) {
  done <- character()
  base <- base_packages()
  cols <- c("Depends", "Imports", "LinkingTo")

  while (length(x) > 0L) {
    done <- c(done, x)
    deps <- parse_deps(na.omit(c(db[match(x, db[, "Package"]), cols])))
    x <- setdiff(deps, c(x, base))
  }

  sort(unique(done))
}
