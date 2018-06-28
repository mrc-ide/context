storage_driver_psql_create <- function(path, id, args) {
  host <- Sys.getenv("CONTEXT_PGHOST", "localhost")
  tbl_data <- sprintf("context_%s_data", id)
  tbl_keys <- sprintf("context_%s_keys", id)
  con <- tryCatch(
    DBI::dbConnect(RPostgres::Postgres(), host = host, user = "postgres"),
    error = function(e) {
      testthat::skip("postgres not available")
    })
  storr::storr_dbi(con = con, tbl_data = tbl_data, tbl_keys = tbl_keys,
                   binary = FALSE, hash_algorithm = "sha1")
}
## This avoids a warning about package:context not guaranteed to be
## available
environment(storage_driver_psql_create) <- .GlobalEnv

storage_driver_psql <-
  storage_driver("postgres", storage_driver_psql_create, "RPostgres")
