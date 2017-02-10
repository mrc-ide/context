## Consider also looking in the *context* environment for variables
## that we can skipped over?  See the note below.
prepare_expression <- function(expr, envir, db) {
  args <- expr[-1L]

  is_call <- vlapply(args, is.call)
  is_symbol <- vlapply(args, is.symbol)

  symbols <- vcapply(unname(as.list(args))[is_symbol], as.character)
  if (any(is_call)) {
    symbols <- union(symbols,
                     unname(unlist(lapply(args[is_call], find_symbols))))
  }

  ret <- list(expr = expr)

  if (length(symbols) > 0L) {
    local <- vlapply(symbols, exists, envir, inherits = FALSE,
                     USE.NAMES = FALSE)
    if (any(!local)) {
      test <- symbols[!local]
      ## TODO: Doing this *properly* requires that we know what was
      ## created in the context.  So this is going to probably copy
      ## too much over I think.  But distinguishing between Global
      ## environment variables that were created when the context was
      ## set up and from variables that have been changed is
      ## challenging.
      global <- vlapply(test, exists, parent.env(.GlobalEnv), USE.NAMES = FALSE)
      if (any(!global)) {
        stop("not all objects found: ",
             paste(test[!global], collapse = ", "))
      }
    }

    ## NOTE: The advantage of saving these via the store is we can do
    ## deduplicated storage (which would be good if we had large
    ## objects and we get lots of duplicate objects with things like
    ## qlapply) but the (big) disadvantage is that it leads to a lot
    ## of files kicking around which is problematic from a cleanup
    ## perspective.
    if (any(local)) {
      objects <- lapply(symbols[local], get, envir, inherits = FALSE)
      h <- db$mset_by_value(objects, "objects")
      ret$objects <- setNames(h, symbols[local])
    }
  }

  ret
}

## Mostly similar to rrqueue::restore_expression
restore_locals <- function(dat, parent) {
  e <- new.env(parent = parent)
  if (!is.null(dat$objects)) {
    context_db_get(dat)$export(e, dat$objects, "objects")
  }
  e
}

find_symbols <- function(expr, hide_errors = TRUE) {
  symbols <- character(0)

  f <- function(e) {
    if (!is.recursive(e)) {
      if (!is.symbol(e)) {
        ## A literal of some type
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
