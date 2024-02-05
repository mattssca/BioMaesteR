library(testthat)

test_that("purify_regions handles single region string", {
  result <- purify_regions(these_regions = "chr1:100-500")
  expect_equal(nrow(result), 1)
  expect_equal(result$chrom, "chr1")
  expect_equal(result$start, 100)
  expect_equal(result$end, 500)
})

test_that("purify_regions handles multiple region strings", {
  result <- purify_regions(these_regions = c("chr1:100-500", "chr2:100-500"))
  expect_equal(nrow(result), 2)
  expect_equal(result$chrom, c("chr1", "chr2"))
  expect_equal(result$start, c(100, 100))
  expect_equal(result$end, c(500, 500))
})

test_that("purify_regions handles individual region parameters", {
  result <- purify_regions(qchrom = "chr1", qstart = 100, qend = 500)
  expect_equal(nrow(result), 1)
  expect_equal(result$chrom, "chr1")
  expect_equal(result$start, 100)
  expect_equal(result$end, 500)
})

test_that("purify_regions handles multiple individual region parameters", {
  result <- purify_regions(qchrom = c("chr1", "chr2"), qstart = c(100, 200), qend = c(500, 600))
  expect_equal(nrow(result), 2)
  expect_equal(result$chrom, c("chr1", "chr2"))
  expect_equal(result$start, c(100, 200))
  expect_equal(result$end, c(500, 600))
})

test_that("purify_regions handles data frame input", {
  my_regions <- data.frame(chrom = c("chr1", "chr2"), start = c(100, 200), end = c(500, 600))
  result <- purify_regions(these_regions = my_regions)
  expect_equal(nrow(result), 2)
  expect_equal(result$chrom, c("chr1", "chr2"))
  expect_equal(result$start, c(100, 200))
  expect_equal(result$end, c(500, 600))
})

test_that("purify_regions throws error with incorrect input", {
  expect_error(purify_regions(qchrom = "chr1", qstart = 100))
  expect_error(purify_regions(these_regions = data.frame(chrom = c("chr1", "chr2"), start = c(100, 200), end = c(50, 100))))
})
