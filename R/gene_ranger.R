#' @title Gene Ranger.
#'
#' @description Return the coordinates in different formats
#' for one gene or a set of genes.
#'
#' @details This function takes gene or a set of genes with the `these_genes`
#' parameter and returns the genomic coordinates in the selected projection.
#' Genes can be formatted in one of the following formats;
#' Hugo Symbol or Ensembl Gene ID. The user gets to decide the projection with
#' `projection` parameter (hg38 and grch37 are available). Furthermore, this
#' function has flexible return formats available. For more information see the
#' parameter description `return_as`. Note if the user sets `write_to_bed = TRUE`,
#' the function only writes to bed file (i.e nothing is returned).
#'
#' @param these_genes Specify genes of interest. This function accepts multiple
#' genes. Genes can be specified either in Hugo Format or Ensembl IDs. The
#' function will figure out what format you are requesting.
#' @param projection The desired projection you want back coordinates for.
#' Available projections are hg38 and grch37. Default is hg38.
#' @param return_as Speciy the format for the returned object. Possible values
#' are; data.frame (default). in this mode, the following columns are kept;
#' chrom, start, end, width, strand, gene_biotype, and gene symbol (in Hugo or
#' Ensembl format, depending on what the user specifies under `these_genes`).
#' With `return_as = "bed"`, the function returns the standard bed columns chrom,
#' start, end. It also attaches the following optional columns; score and strand.
#' The user can also request the return in "region" format (chr:start-end).
#' @param write_to_bed Boolean parameter. Set to TRUE (default is FALSE) to
#' write output to a bed file.
#' @param bed_path If `write_to_bed = TRUE`, this parameter is required.
#' Specify the path  of the generated ebd file.
#' @param track_name If `write_to_bed = TRUE`, this parameter is required.
#' Specify the track name in the generated bed file.
#' @param track_description If `write_to_bed = TRUE`, this parameter is required.
#' Specify the track description in the generated bed file.
#'
#' @return A data frame with coordinates for the requested gene. See `return_as`
#' for more information on controlling the return of this function.
#'
#' @rawNamespace import(stats, except = c("lag", "filter"))
#' @import dplyr stringr utils
#'
#' @export
#'
#' @examples
#' #Example 1 - Request one gene (in Hugo format) and with default parameters
#' hugo_myc = gene_ranger(these_genes = "MYC")
#'
#' #Example 2 - Same as example one but MYC is here specified as Ensembl ID
#' ensembl_myc = gene_ranger(these_genes = "ENSG00000136997")
#'
#' #Example 3 - Request multiple genes with non-default parameters
#' my_genes = gene_ranger(these_genes = c("MYC", "BCL2"),
#'                        projection = "grch37",
#'                        return_as = "region")
#'
#' #Example 4 - Request multiple Ensembl IDs and return in bed format
#' my_bed = gene_ranger(these_genes = c("ENSG00000136997", "ENSG00000171791"),
#'                      return_as = "bed")
#'
#' #Example 5 - Write to bed file
#' gene_ranger(these_genes = c("BCL2", "MYC"),
#'             projection = "grch37",
#'             write_to_bed = TRUE,
#'             bed_path = "my_bed",
#'             track_name = "MYC and BCL2",
#'             track_description = "Genes of interest")
#'
gene_ranger <- function(these_genes = NULL,
                        projection = "hg38",
                        return_as = "data.frame",
                        write_to_bed = FALSE,
                        bed_path = NULL,
                        track_name = NULL,
                        track_description = NULL) {

  #run helper
  gene_table = BioMaesteR::get_gene_info(these_genes = these_genes,
                                         projection = projection,
                                         raw = TRUE)

  #get the input gene format from the helper
  gene_format = unique(gene_table$input_format)

  #sort the gene table
  chrm_num = factor(gene_table$chrom, levels = c(1:22, "X", "Y"), ordered = TRUE)
  gene_table = dplyr::arrange(gene_table, chrm_num, start)

  #write bed
  if(write_to_bed){
    if(is.null(bed_path)){
      stop("Provide a path for output bed file")}
    else if(is.null(track_name)){
      stop("Provide a track name for output bed file")
    }else if(is.null(track_description)){
      stop("Provide a track description for output bed file")
    }else{
      #subset gene table to bed format
      gene_table = dplyr::rename(gene_table, chromStart = start,
                                 chromEnd = end) %>%
        dplyr::select(chrom, chromStart, chromEnd, score, strand)

      #write bed with header
      cat(paste0('track name="', track_name, '"',
                 ' description="', track_description, '"\n'),
          file = paste0(bed_path, ".bed"))

      write.table(gene_table,
                  file = paste0(bed_path, ".bed"),
                  row.names = FALSE,
                  col.names = FALSE,
                  quote = FALSE,
                  append = TRUE,
                  sep = "\t")

      message(paste0("SUCCESS! bed file written to ", bed_path))
      return()
    }
  }

  #format the return
  if(return_as == "data.frame"){
    if(gene_format == "Ensembl"){
      gene_table = dplyr::select(gene_table, chrom, start, end, width,
                                 strand, gene_biotype, ensembl_gene_id)
    }else if(gene_format == "Hugo"){
      gene_table = dplyr::select(gene_table, chrom, start, end, width,
                                 strand, gene_biotype, hugo_symbol)
    }
  }else if(return_as == "bed"){
    gene_table = dplyr::select(gene_table, chrom, start, end, score, strand)
  }else if(return_as == "region"){
    gene_table = setNames(paste0(gene_table$chrom, ":", gene_table$start, "-",
                                 gene_table$end, recycle0 = TRUE), these_genes)
  }else{
    stop(paste0("A non-valid return format was requested (", return_as, "),
                please request one of the following; data.frame, bed, or region."))
  }

  return(gene_table)
}
