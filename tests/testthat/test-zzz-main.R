context("main (bootstrap)")

test_that("run", {
  root <- tempfile("context_")
  ctx <- context_save(auto=TRUE, root=tempfile("cluster_"))
  handle <- task_save(quote(sin(1)), ctx)

  ## Now, what I'd like to do is start an R session that has no
  ## packages and have them try to run this.  This is *definitely* a
  ## don't run on CRAN job but will be useful here I think.  That's
  ## really hard to do actually.  Will continue testing that with the
  ## Docker image I think.

  ## This version *will* install at least context though so that's
  ## good.

  ## NOTE: There is a super slow delay in here because of calling
  ## available.packages(); nothing can be done about it :(

  root <- handle$root
  res <- call_system(file.path(root, "bin", "context_runner"),
                     c(root, handle$id))

  log <- parse_context_log(res)
  i <- match("install", log$title)[[1]]
  expect_match(log$value[[i]], "context")

  expect_equal(context_db(handle)$get(handle$id, "task_results"), sin(1))
})
