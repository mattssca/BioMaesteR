library(testthat)

test_that("gene_ranger throws error with no genes", {
  expect_error(gene_ranger())
})

test_that("gene_ranger throws error with invalid projection", {
  expect_error(gene_ranger(these_genes = "MYC", projection = "invalid"))
})

test_that("gene_ranger throws error with invalid return format", {
  expect_error(gene_ranger(these_genes = "MYC", return_as = "invalid"))
})

test_that("gene_ranger throws error when write_to_bed is TRUE but bed_path, track_name, or track_description is not provided", {
  expect_error(gene_ranger(these_genes = "MYC", write_to_bed = TRUE), "Provide a path for output bed file")
  expect_error(gene_ranger(these_genes = "MYC", write_to_bed = TRUE, bed_path = "my_bed"), "Provide a track name for output bed file")
  expect_error(gene_ranger(these_genes = "MYC", write_to_bed = TRUE, bed_path = "my_bed", track_name = "MYC"), "Provide a track description for output bed file")
})

test_that("gene_ranger runs successfully with valid input", {
  expect_silent(gene_ranger(these_genes = "MYC"))
})
