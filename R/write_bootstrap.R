## This attempts to write out enough of the package to bootstrap the
## package into working.  It uses the actual package functions rather
## than anything else so that's nice.
##
## One issue is that libcurl is going to be required to satisfy the
## curl dependency.  That's not very pleasant to deal with.  In the
## docker case we could attempt to fix that with an
##   apt-get update && apt-get -y libcurl4-openssl-dev
## but that seems to be asking a little too much.
##
##   platform <- function() {
##     name <- tolower(Sys.info()[["sysname"]])
##     if (name == "linux") {
##       tmp <- strsplit(readLines("/proc/self/cgroup") ":", fixed=TRUE)
##       if (any(grepl("docker", vapply(tmp, "[[", character(1), 3L)))) {
##         name <- "docker"
##       }
##     }
##     name
##   }
##
## But for now, I'll just require it.
##
## It would probably be better to stick the files into a drat and
## install off that; that removes any separate package installation
## (github urls etc).  and we could actually install it into a
## separate local library.  Only downside is that during development
## it's then difficult to get the most recent version.
write_bootstrap <- function(root) {
  bootstrap <- function(root, version) {
    options(context.log=TRUE)
    context_log("bootstrap", normalizePath(root))
    if ("context" %in% .packages(TRUE)) {
      version <- package_version(readLines(file.path(root, "context_version")))
      if (packageVersion("context") >= version) {
        context_log("ok", "")
        return()
      }
    }
    use_local_library(root)
    deps <- c("curl", "drat")
    packages <- setdiff(deps, .packages(TRUE))
    if (length(packages) > 0L) {
      context_log("install", paste(packages, collapse=", "))
      install.packages2(packages)
    }
    url <- github_url("dide-tools", "context", "master")
    filename <- download_file(url, tempfile("context_"))
    path <- tempfile("path")
    unzip(filename, exdir=path)
    path <- dir(path, full.names=TRUE)
    install.packages2(path, repos=NULL)
  }
  main <- function(args=commandArgs(TRUE)) {
    if (length(args) != 1L) {
      stop("Usage: context_bootstrap.R <root>")
    }
    bootstrap(args[[1]])
  }

  fun_to_str <- function(x, env) {
    paste0(x, " <- ",
           paste(deparse(get(x, env, inherits=FALSE)), collapse="\n"))
  }
  env <- environment(write_bootstrap)
  funs_context <- vcapply(find_funcs(bootstrap, env),
                          fun_to_str, env, USE.NAMES=FALSE)
  funs_bootstrap <- vcapply(c("bootstrap", "main"),
                            fun_to_str, environment(), USE.NAMES=FALSE)
  code <- c("#!/usr/bin/env Rscript",
            "local({",
            funs_context,
            funs_bootstrap,
            "main()",
            "})")

  dir.create(root, FALSE, TRUE)
  writeLines(as.character(packageVersion(.packageName)), path_version(root))

  dest <- path_bootstrap(root)
  writeLines(code, dest)
  Sys.chmod(dest, "0755")
  invisible(dest)
}

find_funcs <- function(fun, env) {
  ours <- names(env)
  seen <- character(0)
  test <- list(fun)
  while (length(test) > 0L) {
    new <- setdiff(intersect(codetools::findGlobals(test[[1]]), ours), seen)
    seen <- c(seen, new)
    test <- c(test[-1], lapply(new, get, env, inherits=FALSE))
  }
  sort(seen)
}
