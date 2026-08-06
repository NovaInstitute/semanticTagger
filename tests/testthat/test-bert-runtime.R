test_that("stabilize_bertopic_runtime sets defaults only when unset", {
  vars <- c(
    "OMP_NUM_THREADS",
    "MKL_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "NUMEXPR_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "BLIS_NUM_THREADS",
    "NUMBA_NUM_THREADS",
    "TOKENIZERS_PARALLELISM"
  )

  old <- Sys.getenv(vars, unset = NA_character_)
  on.exit({
    for (i in seq_along(vars)) {
      if (is.na(old[[i]])) {
        do.call(Sys.unsetenv, list(vars[[i]]))
      } else {
        do.call(Sys.setenv, setNames(list(old[[i]]), vars[[i]]))
      }
    }
  }, add = TRUE)

  do.call(Sys.unsetenv, list(vars))
  out <- stabilize_bertopic_runtime()

  expect_equal(out$OMP_NUM_THREADS, "1")
  expect_equal(out$TOKENIZERS_PARALLELISM, "false")

  Sys.setenv(OMP_NUM_THREADS = "8")
  out2 <- stabilize_bertopic_runtime()
  expect_equal(out2$OMP_NUM_THREADS, "8")
})
