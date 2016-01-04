context_db <- function(x, warn=TRUE) {
  if (is.list(x) && inherits(x$db, "storr")) {
    x$db
  } else {
    if (is.list(x)) {
      x <- x$root
    }
    if (!is.character(x)) {
      stop("Invalid input; cannot determine context root")
    }
    context_db_open(x, readRDS(path_config(x)))
  }
}

context_db_open <- function(root, config) {
  switch(config$type,
         rds=storr::storr_rds(path_db(root)),
         ## This is actually a little more difficult than this because
         ## we need to add any required packages (e.g., redux) to the
         ## bootstrap script.  That's also going to generate some
         ## issues with blowing out 'Suggests:' in the package
         ## perhaps.  For now leave this be.
         ## redis=storr::storr_redis_api(redux::redis(config=config$args)),
         stop("Unsupported storage type ", config$type))
}
