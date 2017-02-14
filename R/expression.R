## TODO Consider also looking in the *context* environment for
## variables that we can skipped over?  See the note below.

##' Prepare expression for evaluation in context
##'
##' The \code{function_value} argument here is used where \code{expr} is
##' going to take a function that is not addressable by \emph{name}; in
##' that case we take a function itself (as "function_value"),
##' serialise it and replace the function call with the hash.  The
##' function will be serialised into the calling environment on
##' deserialisation.
##'
##' This includes the remote possibility of a collision, but with the
##' size of the keyspace used for hashes hopefully it's negligable.
##'
##' Because of the approach used here, \code{expr} can contain
##' anything; I'd suggest not saving the contents of the function
##' itself, but something like \code{NULL} will work just fine:
##'
##' \preformatted{
##'   as.call(list(NULL, quote(a)))
##'   # NULL(a)
##' }
##'
##' @title Prepare expression
##'
##' @param expr A quoted expression consisting of a single function
##'   call.
##'
##' @param envir An environment to find variables local to the expression
##'
##' @param db A database to store locals
##'
##' @param function_value Optionally, the \emph{value} of a function
##'   where the expression should involve an anonymous function.  In
##'   this case the function in \code{expr} will be replaced.
##'
##' @export
prepare_expression <- function(expr, envir, db, function_value = NULL) {
  args <- expr[-1L]

  is_call <- vlapply(args, is.call)
  is_symbol <- vlapply(args, is.symbol)

  symbols <- vcapply(unname(as.list(args))[is_symbol], as.character)
  if (any(is_call)) {
    symbols <- union(symbols,
                     unname(unlist(lapply(args[is_call], find_symbols))))
  }

  ret <- list(expr = expr)

  if (!is.null(function_value)) {
    assert_is(function_value, "function")
    hash <- db$set_by_value(function_value, "objects")
    ret$function_hash <- setNames(hash, hash)
    ret$expr[[1L]] <- as.name(hash) # small chance of collision, but unlikely
  }

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

##' Restore locals created by \code{\link{prepare_expression}}.
##' @title Restore locals
##'
##' @param dat An expression that has been through
##'   \code{prepare_expression}.  Key elements are
##'   \code{function_hash} and \code{objects}
##'
##' @param parent The parent environment to restore locals to
##'
##' @param db The database used to prepare the expression
##'
##' @export
restore_locals <- function(dat, parent, db) {
  ## TODO: task_read is not exported, so this does not make a huge
  ## amount of sense.  rrq shares the same interface, so uses this.
  ## It might be easiest just to copy this over instead.
  e <- new.env(parent = parent)
  restore <- c(dat$function_hash, dat$objects)
  if (length(restore) > 0L) {
    db$export(e, restore, "objects")
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
