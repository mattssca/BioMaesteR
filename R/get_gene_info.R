#' @title Get Gene Info
#'
#' @description Convenience function for returning information about a gene or
#' a set of genes. This function is internally called by
#' [BioMaesteR::get_gene_region].
#'
#' @details Give the function a gene or a set of genes (as a vector of characters),
#' specify the projection (if not, hg38 is the default projection) and return
#' gene information based on the bundled data. By default this function is
#' run with `raw = FALSE`, this returns a subset of columns. If instead the user
#' wants everything back (i.e all available columns) toggle `raw` to `TRUE`.
#'
#' @param these_genes Required argument. The gene or genes of interest.
#' @param projection The desired projection, default is hg38.
#' @param raw Default is FALSE, set to TRUE for keeping all columns.
#'
#' @return A data frame with gene information.
#'
#' @import dplyr stringr
#'
#' @export
#'
#' @examples
#' #Example 1 - Query one gene (in Hugo format) and with default parameters.
#' get_gene_info(these_genes = "MYC")
#'
#' #Example 2 - Same as example 1 but MYC is here specified as Ensembl ID.
#' get_gene_info(these_genes = "ENSG00000136997")
#'
#' #Example 3 - Request multiple genes with non-default parameters
#' get_gene_info(these_genes = c("MYC", "BCL2"),
#'               projection = "grch37")
#'
#' #Example 4 - Request multiple Ensembl IDs and return all columns.
#' get_gene_info(these_genes = c("ENSG00000136997", "ENSG00000171791"),
#'               raw = TRUE)
#'
get_gene_info <- function(these_genes = NULL,
                          projection = "hg38",
                          raw = FALSE) {

  #check if genes are provided
  if(is.null(these_genes)){
    stop("Please pass one gene (or multiple genes as a vector of characters)
         to `these_genes` parameter...")
  }

  #deal with projections
  if(projection == "hg38"){
    gene_table = gene_coordinates_hg38
  }else if(projection == "grch37"){
    gene_table = gene_coordinates_grch37
  }else{
    stop(paste0("This function supports the following projections; hg38 and
                grch37. The provided projection is: ", projection))
  }

  #Hugo symbol or Ensembl ID
  if(all(str_detect(these_genes, "ENSG"))){
    gene_format = "Ensembl"
  }else if(all(!str_detect(these_genes, "ENSG"))){
    gene_format = "Hugo"
  }else{
    stop("Mixed gene formats are not supported, please specify the gene(s) of
         interest as either Hugo Symbols or Ensembl IDs")
  }

  #add the input format as a new column
  gene_table$input_format = gene_format

  #filter on the selected genes
  if(gene_format == "Ensembl"){
    gene_table = dplyr::filter(gene_table, ensembl_gene_id %in% these_genes)
  }else if(gene_format == "Hugo"){
    gene_table = dplyr::filter(gene_table, hugo_symbol %in% these_genes)
  }

  if(raw){
    return(gene_table)
  }else{
    if(gene_format == "Ensembl"){
      gene_table = dplyr::select(gene_table, ensembl_gene_id, hugo_symbol,
                                 type, gene_biotype, source, gene_version,
                                 gene_source, tag, ccds_id, score, transcript_id,
                                 transcript_version, transcript_name,
                                 transcript_source, transcript_biotype,
                                 exon_number, exon_id, protein_id, protein_version)
    }else if(gene_format == "Hugo"){
      gene_table = dplyr::select(gene_table, hugo_symbol, ensembl_gene_id, type,
                                 gene_biotype, source, gene_version, gene_source,
                                 tag, ccds_id, score, transcript_id,
                                 transcript_version, transcript_name,
                                 transcript_source, transcript_biotype,
                                 exon_number, exon_id, protein_id, protein_version)
    }
    return(gene_table)
  }
}
