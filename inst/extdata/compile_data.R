#get chromosome arms
library(dplyr)
chromosome_arms_grch37 = system.file("extdata","chromosome_arms_grch37.tsv", package = "BioMaesteR") %>%
  read.table(sep = "\t", header = 1)

usethis::use_data(chromosome_arms_grch37, overwrite = TRUE)
