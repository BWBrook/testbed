testthat::test_that("manifest_missing_files returns empty tibble when all files exist", {
  mf <- read_manifest("metadata/data_manifest.csv")
  miss <- manifest_missing_files(mf)
  testthat::expect_s3_class(miss, "tbl_df")
  testthat::expect_equal(nrow(miss), 0L)
})

testthat::test_that("summarize_numeric_columns computes summary stats", {
  df <- tibble::tibble(a = c(1, 2, 3, NA), b = c(10, 20, 30, 40), c = letters[1:4])
  prof <- summarize_numeric_columns(df)
  testthat::expect_true(all(c("column", "n", "mean", "sd", "min", "max") %in% names(prof)))
  testthat::expect_setequal(prof$column, c("a", "b"))
  a_stats <- prof[prof$column == "a", ]
  testthat::expect_equal(a_stats$n, 3)
  testthat::expect_equal(a_stats$mean, mean(c(1, 2, 3)))
})

testthat::test_that("write_combined_data writes CSV to path", {
  tmpdir <- file.path(tempdir(), "outputs-test")
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)
  df <- tibble::tibble(a = 1:2, b = c("x", "y"))
  path <- file.path(tmpdir, "nested", "out.csv")
  invisible(write_combined_data(df, path))
  testthat::expect_true(file.exists(path))
  roundtrip <- utils::read.csv(path, stringsAsFactors = FALSE)
  testthat::expect_equal(roundtrip$a, df$a)
})
