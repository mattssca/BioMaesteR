#load packages
library(testthat)

test_that("bin_splitter returns correct number of bins", {
  result <- bin_splitter(qchrom = c("chr1"), qstart = c(1), qend = c(1001), bin_size = 100)
  expect_equal(nrow(result), 10)
})

test_that("bin_splitter returns correct bin size", {
  result <- bin_splitter(qchrom = c("chr1"), qstart = c(1), qend = c(1000), bin_size = 100)
  expect_equal(result$bin_end[1] - result$bin_start[1], 100)
})

test_that("bin_splitter handles multiple regions", {
  result <- bin_splitter(qchrom = c("chr1", "chr2"), qstart = c(1, 1001), qend = c(1001, 2001), bin_size = 100)
  expect_equal(nrow(result), 20)
})

test_that("bin_splitter handles regions data frame", {
  my_regions <- data.frame(chrom = c("chr1", "chr2"), start = c(1, 1000), end = c(1001, 2001))
  result <- bin_splitter(these_regions = my_regions, bin_size = 100)
  expect_equal(nrow(result), 20)
})

test_that("bin_splitter returns correct chromosome names", {
  result <- bin_splitter(qchrom = c("chr1", "chr2"), qstart = c(1, 1001), qend = c(1000, 2000), bin_size = 100)
  expect_equal(unique(result$bin_chr), c("chr1", "chr2"))
})

