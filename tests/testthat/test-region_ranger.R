library(testthat)

test_that("region_ranger throws error with no regions", {
  expect_error(region_ranger())
})

test_that("region_ranger throws error with invalid projection", {
  expect_error(region_ranger(these_regions = "chr8:127735434-127742951", projection = "invalid"), "This function supports the following projections")
})

test_that("region_ranger returns correct number of columns when raw is FALSE", {
  result = region_ranger(these_regions = "chr8:127735434-127742951")
  expect_equal(ncol(result), 11)
})

test_that("region_ranger returns all columns when raw is TRUE", {
  result = region_ranger(these_regions = "chr8:127735434-127742951", raw = TRUE)
  expect_true(ncol(result) > 11)
})

test_that("region_ranger returns warning when no genes found", {
  expect_warning(region_ranger(these_regions = "chr1:1-10"))
})
