## This is related to functions of similar names in rrqueue, but is
## decoupled from the storage end.  Here we'll just serialise to disk
## so it's not a big deal.  Or would using a storr cache (but
## rds-backed) make more sense here perhaps?  Probably not as storr
## pulls in httr, rappdirs, etc.
store_expression <- function(expr, envir) {
  ret <- list(expr=expr)

  fun <- expr[[1]]
  args <- expr[-1]

  is_call <- vlapply(args, is.call)
  is_symbol <- vlapply(args, is.symbol)

  if (any(is_call)) {
    ## In theory, this is not that nasty; what we'd need to do is to
    ## scan through all expressions and pull out local variables, I
    ## think.  The other option is that because we're going to
    ## serialise the entire local environment we could possibly just
    ## defer this step until the cluster tries to do anything with it.
    ## In any case it can wait.
    stop("complex expressions not yet supported")
  }

  if (any(is_symbol)) {
    object_names <- vcapply(args[is_symbol], as.character)
    if (!all(ok <- exists(object_names, envir, inherits=FALSE))) {
      stop("not all objects found: ",
           paste(object_names[!ok], collapse=", "))
    }
    ret$objects <- lapply(object_names, get, envir, inherits=FALSE)
    names(ret$objects) <- object_names
  }

  ret
}

restore_expression <- function(dat, envir) {
  objects <- dat$objects
  object_names <- names(dat$objects)
  for (i in seq_along(objects)) {
    envir[[object_names[[i]]]] <- objects[[i]]
  }
  dat$expr
}

## Not really sure why this is separate.  I think I'd planned on using
## it in a slightly more predictable way.
restore_locals <- function(dat, parent) {
  e <- new.env(parent=parent)
  objects <- dat$objects
  object_names <- names(dat$objects)
  for (i in seq_along(objects)) {
    envir[[object_names[[i]]]] <- objects[[i]]
  }
  e
}
