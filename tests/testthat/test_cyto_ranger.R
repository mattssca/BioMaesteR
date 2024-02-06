library(testthat)

test_that("cyto_ranger throws error with invalid projection", {
  expect_error(cyto_ranger(these_regions = "chr8:127735434-127742951", projection = "invalid"), "This function supports the following projections")
})

test_that("cyto_ranger returns correct columns", {
  result = cyto_ranger(these_regions = "chr8:127735434-127742951")
  expect_equal(colnames(result), c("chrom", "region_start", "region_end", "cytoBand", "cytoband_start", "cytoband_end"))
})

test_that("cyto_ranger returns correct cytoband information", {
  result = cyto_ranger(these_regions = "chr8:127735434-127742951")
  expect_equal(result$cytoBand, "q24.21")
})
