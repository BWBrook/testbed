testthat::test_that("combine_manifest_csvs returns common columns and rows", {
  mf <- read_manifest("metadata/data_manifest.csv")
  cmb <- combine_manifest_csvs(mf)
  testthat::expect_s3_class(cmb, "tbl_df")
  testthat::expect_true(all(c("col1", "col2") %in% names(cmb)))
  testthat::expect_gt(nrow(cmb), 0)
})

testthat::test_that("combine_manifest_csvs keeps only intersecting columns", {
  tmp1 <- tempfile(fileext = ".csv")
  tmp2 <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)

  utils::write.csv(tibble::tibble(a = 1, b = 2, c = 3), tmp1, row.names = FALSE)
  utils::write.csv(tibble::tibble(b = 4, c = 5, d = 6), tmp2, row.names = FALSE)

  manifest <- tibble::tibble(
    id = c("one", "two"),
    path = basename(c(tmp1, tmp2)),
    abs_path = c(tmp1, tmp2)
  )

  cmb <- combine_manifest_csvs(manifest)
  testthat::expect_s3_class(cmb, "tbl_df")
  testthat::expect_equal(sort(names(cmb)), c("b", "c"))
  testthat::expect_equal(nrow(cmb), 2L)
})
