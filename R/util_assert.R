assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call. = FALSE)
  }
}

assert_nonmissing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("%s must not be NA", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call. = FALSE)
  }
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
  assert_nonmissing(x, name)
}

assert_is <- function(x, type, name = deparse(substitute(x))) {
  if (!(inherits(x, type))) {
    stop(sprintf("%s must inherit from %s",
                 name, paste(type, collapse = " / ")))
  }
}

assert_function <- function(x, name = deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call. = FALSE)
  }
}

match_value <- function(x, choices, name = deparse(substitute(x))) {
  assert_scalar_character(x, name)
  i <- match(x, choices)
  if (is.na(i)) {
    stop(sprintf("%s must be one of {%s}",
                 name, paste(choices, collapse = ", ")), call. = FALSE)
  }
  choices[[i]]
}
