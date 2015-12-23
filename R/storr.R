context_db <- function(root) {
  storr::storr_rds(path_db(root), mangle_key=TRUE)
}
