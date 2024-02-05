library(testthat)

test_that("sanity_check_regions throws error with no regions", {
  expect_error(sanity_check_regions(), "No regions provided")
})

test_that("sanity_check_regions throws error with invalid projection", {
  my_regions <- data.frame(chrom = c("chr1"), start = c(100), end = c(200))
  expect_error(sanity_check_regions(incoming_regions = my_regions, projection = "invalid"), "Invalid projection specified")
})

test_that("sanity_check_regions throws error when start is greater than end", {
  my_regions <- data.frame(chrom = c("chr1"), start = c(200), end = c(100))
  expect_error(sanity_check_regions(incoming_regions = my_regions, projection = "hg38"), "start is greater than or equal to end")
})

test_that("sanity_check_regions throws error when start or end is outside chromosomal range", {
  my_regions <- data.frame(chrom = c("chr1"), start = c(100), end = c(300000000))
  expect_error(sanity_check_regions(incoming_regions = my_regions, projection = "hg38"), "Specified start or end coordinates fall outside the actual chromosomal range")
})

test_that("sanity_check_regions runs successfully with valid input", {
  my_regions <- data.frame(chrom = c("chr1"), start = c(100), end = c(200))
  expect_silent(sanity_check_regions(incoming_regions = my_regions, projection = "hg38"))
})

test_that("sanity_check_regions runs successfully with valid input", {
  my_regions <- data.frame(chrom = c("1"), start = c(100), end = c(200))
  expect_silent(sanity_check_regions(incoming_regions = my_regions, projection = "grch37"))
})

