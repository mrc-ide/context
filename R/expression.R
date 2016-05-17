## like rrqueue:::prepare_expression() followed by
## rrqueue::save_expression().
store_expression <- function(expr, envir, db) {
  id <- random_id()
  fun <- expr[[1]]
  args <- expr[-1]

  is_call <- vlapply(args, is.call)
  is_symbol <- vlapply(args, is.symbol)

  symbols <- vcapply(unname(as.list(args))[is_symbol], as.character)
  if (any(is_call)) {
    symbols <- union(symbols,
                     unname(unlist(lapply(args[is_call], find_symbols))))
  }

  ret <- list(expr=expr, id=id)

  if (length(symbols) > 0L) {
    local <- exists(symbols, envir, inherits=FALSE)
    if (any(!local)) {
      test <- symbols[!local]
      ## TODO: Doing this *properly* requires that we know what was
      ## created in the context.  So this is going to probably copy
      ## too much over I think.  But distinguishing between Global
      ## environment variables that were created when the context was
      ## set up and from variables that have been changed is
      ## challenging.
      global <- exists(test, parent.env(.GlobalEnv))
      if (any(!global)) {
        stop("not all objects found: ",
             paste(test[!global], collapse=", "))
      }
    }

    ## NOTE: The advantage of saving these via the store is we can do
    ## deduplicated storage (which would be good if we had large
    ## objects and we get lots of duplicate objects with things like
    ## qlapply) but the (big) disadvantage is that it leads to a lot
    ## of files kicking around which is problematic from a cleanup
    ## perspective.
    if (any(local)) {
      ret$objects <- vcapply(symbols[local], function(i)
        db$set_by_value(get(i, envir, inherits=FALSE), namespace="objects"))
    }
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

find_symbols <- function(expr, hide_errors=TRUE) {
  symbols <- character(0)

  f <- function(e) {
    if (!is.recursive(e)) {
      if (!is.symbol(e)) { # A literal of some type
        return()
      }
      symbols <<- c(symbols, deparse(e))
    } else {
      for (a in as.list(e[-1])) {
        if (!missing(a)) {
          f(a)
        }
      }
    }
  }

  f(expr)
  unique(symbols)
}
