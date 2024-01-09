#' @title Region Ranger.
#'
#' @description Return genes residing in defined region(s).
#'
#' @details Query a region and return all genomic events residing inside the
#' specified region. This function accepts a variety of incoming regions.
#' Either, regions can be provided as a data frame with `these_regions`.
#' If so, the following columns must exist; chrom, start, end.
#' This parameter (`these_regions`) also accept a region in "region" format,
#' (i.e chr:start-end). This can be a region or a vector of characters with
#' multiple regions. The user can also individually specify region(s) with;
#' `qchrom` (string), `qstart` (string, or integer), and `qend` (string or integer).
#' These parameters can also accept a vector of characters for multiple regions.
#' The function also handles chromosome prefixes in the returned object,
#' based on the selected `projection`.
#'
#' @param these_regions these_regions The region(s) to be queried. Can be a data frame with
#' regions with the following columns; chrom, start, end.
#' Or in a string in the following format chr:start-end.
#' @param qchrom Query chromosome (prefixed or un-prefixed),
#' Required if `these_regions` is not provided.
#' @param qstart Query start position. Required if `these_regions` is not provided.
#' @param qend Query end position. Required if `these_regions` is not provided.
#' @param projection The desired projection you want back coordinates for.
#' Available projections are hg38 and grch37. Default is hg38.
#' @param raw Set to TRUE to return all columns. Default is FALSE.
#'
#' @return A data frame with genomic events residing in the specified region(s).
#'
#' @rawNamespace import(data.table, except = c("last", "first", "between", "transpose"))
#' @import dplyr
#'
#' @export
#'
#' @examples
#' #Example 1 - Give the function one region as a string
#' my_region = region_ranger(these_regions = "chr8:127735434-127742951")
#'
#' #Example 2 - Give the function multiple regions as a string
#' my_regions = region_ranger(these_regions = c("chr8:128747680-128753674",
#'                                              "chr18:60790579-60987361"),
#'                             projection = "grch37")
#'
#' #Example 3 - Individually specify the chromosome, start and end coordinates
#' this_region = region_ranger(qchrom = "chr8",
#'                             qstart = 127735434,
#'                             qend = 127742951)
#'
#' #Example 4 - Individually specify multiple regions with the query parameters
#' these_regions = region_ranger(qchrom = c("chr8", "chr18"),
#'                               qstart = c(128747680, 60790579),
#'                               qend = c(128753674, 60987361),
#'                               projection = "grch37")
#'
#'
region_ranger <- function(these_regions = NULL,
                          qchrom = NULL,
                          qstart = NULL,
                          qend = NULL,
                          projection = "hg38",
                          raw = FALSE) {

  #call helper to wrangle regions
  region_table = BioMaesteR::purify_regions(these_regions = these_regions,
                                            qchrom = qchrom,
                                            qstart = qstart,
                                            qend = qend,
                                            projection = projection)

  #deal with projections
  if(projection == "hg38"){
    gene_table = gene_coordinates_hg38
  }else if(projection == "grch37"){
    gene_table = gene_coordinates_grch37
  }else{
    stop(paste0("This function supports the following projections; hg38 and
                grch37. The provided projection is: ", projection))
  }

  #convert to data table object
  gene_table = as.data.table(gene_table)

  #set keys
  data.table::setkey(region_table, chrom, start, end)
  data.table::setkey(gene_table, chrom, start, end)

  #intersect regions
  intersect = data.table::foverlaps(region_table, gene_table, nomatch = 0)
  colnames(intersect)[26] = "region_start"
  colnames(intersect)[27] = "region_end"

  #transform object to data frame
  inter_df = as.data.frame(intersect)

  inter_df = as.data.frame(inter_df) %>%
    dplyr::arrange(chrom, start) %>%
    distinct(.keep_all = TRUE)

  if(!raw){
    inter_df = dplyr::select(inter_df, region_start, region_end, type,
                             gene_biotype, hugo_symbol, ensembl_gene_id,
                             chrom, start, end, width, strand)
  }

  #add checks for 0 genes in the region
  if(nrow(inter_df) == 0){
    warning("No genes found within the specified region(s),
            check the provided regions.")
    }
  return(inter_df)
}
