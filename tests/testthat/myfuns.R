f <- function(x) {
  g(x) * 2
}

g <- function(x) {
  if (x < 0) {
    stop("Need positive x")
  }
  x
}
