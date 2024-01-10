#load pacakges
library(testthat)


test_that("Check for rows and column consistencies", {
  expect_equal(nrow(get_gene_info(these_genes = "MYC", 
                                  projection = "grch37", 
                                  raw = TRUE)), 1)
  
  expect_equal(nrow(get_gene_info(these_genes = "MYC", 
                                  projection = "hg38", 
                                  raw = TRUE)), 1)
  
  expect_equal(ncol(get_gene_info(these_genes = "MYC", 
                                  projection = "grch37", 
                                  raw = TRUE)), 26)
  
  expect_equal(ncol(get_gene_info(these_genes = "MYC", 
                                  projection = "hg38", 
                                  raw = TRUE)), 26)
  
  expect_equal(ncol(get_gene_info(these_genes = "MYC", 
                                  projection = "grch37", 
                                  raw = FALSE)), 19)
  
  expect_equal(ncol(get_gene_info(these_genes = "MYC", 
                                  projection = "hg38", 
                                  raw = FALSE)), 19)
})


test_that("Expected to fail", {
  expect_error(get_gene_info(raw = TRUE))
  expect_error(get_gene_info(these_genes = "MYC",
                             projection = "hg19"))
})


test_that("Check the type of the return", {
  expect_true(is.data.frame(get_gene_info(these_genes = c("BCL2", "MYC"))))
})
