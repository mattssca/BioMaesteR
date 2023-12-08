#' @title Gene to region
#'
#' @description Return coordinates for a given gene or a set of genes.
#'
#' @details This function returns the genomic coordinates for a given gene or a 
#' set of genes. 
#' The coordinates are returned in the format chr:start-end. 
#' The function can also return the coordinates in the BED format or as a data 
#' frame. 
#'
#' @family utilities
#'
#' @param gene_symbol A vector of one or more gene symbols.
#' @param ensembl_id A vector of one or more Ensembl IDs.
#' @param genome_build Reference genome build. Possible values are "grch37" (
#' default) or "hg38".
#' @param return_as Specify the type of return. Default is "region" 
#' (chr:start-end), other acceptable arguments are "bed" and "df".
#' @param sort_regions A Boolean parameter (TRUE is the default) indicating 
#' whether regions should be sorted by chromosome and start location.
#'
#' @return
#'
#' @export
#' @import dplyr
#'
#' @examples
gene_to_region = function(gene_symbol,
                          ensembl_id,
                          genome_build = "grch37",
                          return_as = "region",
                          sort_regions = TRUE){

}
