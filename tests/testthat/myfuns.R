f <- function(x) {
  g(x) * 2
}

g <- function(x) {
  if (x < 0) {
    stop("Need positive x")
  }
  x
}

loop <- function(a, n) {
  for (i in seq_len(n)) {
    message(i)
    a <- a + a
  }
  a
}
