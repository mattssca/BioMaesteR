library(testthat)

test_that("purify_chr throws error with no projection", {
  expect_error(purify_chr(), "You must provide a valid projection")
})

test_that("purify_chr throws error with no incoming_table", {
  expect_error(purify_chr(projection = "hg38"), "You must provide a data table with `incoming_table`")
})

test_that("purify_chr throws error with invalid projection", {
  expect_error(purify_chr(projection = "invalid", incoming_table = data.frame(chrom = c("1", "2", "3"))), "This function supports the following projections")
})

test_that("purify_chr adds 'chr' prefix for hg38 projection", {
  df = data.frame(chrom = c("1", "2", "3"))
  result = purify_chr(projection = "hg38", incoming_table = df)
  expect_equal(result$chrom, c("chr1", "chr2", "chr3"))
})

test_that("purify_chr removes 'chr' prefix for grch37 projection", {
  df = data.frame(chrom = c("chr1", "chr2", "chr3"))
  result = purify_chr(projection = "grch37", incoming_table = df)
  expect_equal(result$chrom, c("1", "2", "3"))
})
