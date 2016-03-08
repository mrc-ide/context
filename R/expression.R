## like rrqueue:::prepare_expression() followed by
## rrqueue::save_expression().
store_expression <- function(expr, envir, db) {
  id <- random_id()
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

  ret <- list(expr=expr, id=id)

  if (any(is_symbol)) {
    object_names <- vcapply(args[is_symbol], as.character)
    if (!all(ok <- exists(object_names, envir, inherits=FALSE))) {
      stop("not all objects found: ",
           paste(object_names[!ok], collapse=", "))
    }
    ## NOTE: The advantage of saving these via the store is we can do
    ## deduplicated storage (which would be good if we had large
    ## objects and we get lots of duplicate objects with things like
    ## qlapply) but the (big) disadvantage is that it leads to a lot
    ## of files kicking around which is problematic from a cleanup
    ## perspective.
    ret$objects <- vcapply(object_names, function(i)
      db$set_by_value(get(i, envir, inherits=FALSE), namespace="objects"))
  }

  ret
}

## Mostly similar to rrqueue::restore_expression
restore_locals <- function(dat, parent) {
  e <- new.env(parent=parent)
  if (!is.null(dat$objects)) {
    context_db(dat)$export(e, dat$objects, "objects")
  }
  e
}
