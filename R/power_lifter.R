#' @title Power Lifter
#' 
#' @description A function to convert genomic regions from one assembly to another.
#' 
#' @details This function is a wrapper for the [rtracklayer::liftOver] function.
#' Specify the original assembly and the target assembly, and the function will
#' convert the regions accordingly. Regions can be provided as a data frame with
#' `these_regions`, or as a string with `qchrom`, `qstart`, and `qend`. 
#'
#' @param these_regions The region(s) to be queried. Can be a data frame with
#' regions with the following columns; chrom, start, end.
#' Or in a string in the following format chr:start-end.
#' @param qchrom Query chromosome (prefixed or un-prefixed),
#' Required if `these_regions` is not provided.
#' @param qstart Query start position. Required if `these_regions` is not provided.
#' @param qend Query end position. Required if `these_regions` is not provided.
#' @param original_assembly The original assembly of the regions. Default is hg38
#' @param target_assembly The target assembly of the regions. Default is grch37.
#'
#' @return A data frame with the regions in the selected target assembly.
#' 
#' @rawNamespace import(GenomicRanges, except = c("start", "end", "shift", 
#' "union", "intersect", "setdiff", "update"))
#' @rawNamespace import(rtracklayer, except = c("start", "end", "offset"))
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' #Example 1 - Convert MYC region from hg38 to grch37
#' power_lifter(these_regions = "chr8:127735434-127742951")
#'
#' #Example 2 - Convert MYC region from grch37 to hg38
#' power_lifter(these_regions = "18:60790579-60987361", 
#'              original_assembly = "grch37", 
#'              target_assembly = "hg38")
#'
#' #Example 3 - Same as Example 1, but use the `qchrom`, `qstart`, and `qend`.
#' power_lifter(qchrom = "chr8", 
#'              qstart = 127735434, 
#'              qend = 127742951)
#' 
power_lifter <- function(these_regions = NULL,
                         qchrom = NULL,
                         qstart = NULL,
                         qend = NULL,
                         original_assembly = "hg38",
                         target_assembly = "grch37"){
  
  #format incoming regions accordingly
  region_table = BioMaesteR::purify_regions(these_regions = these_regions,
                                            qchrom = qchrom,
                                            qstart = qstart,
                                            qend = qend,
                                            projection = original_assembly)
  
  #convert incoming regions to GRanges object
  incoming = GenomicRanges::makeGRangesFromDataFrame(region_table, 
                                                     keep.extra.columns = TRUE)
  
  #load the correct liftOver chains
  if(target_assembly == "grch37"){
    chain_file = rtracklayer::import.chain(system.file("extdata", 
                                                       "hg38ToHg19.over.chain", 
                                                       package = "BioMaesteR"))
  }else if(target_assembly == "hg38"){
    chain_file = rtracklayer::import.chain(system.file("extdata", 
                                                       "hg19ToHg38.over.chain", 
                                                       package = "BioMaesteR"))
  }else{
    stop("Target assembly not recognized. Please use either 'grch37' or 'hg38'.")
  }
  
  #liftOver
  lifted = rtracklayer::liftOver(incoming, chain = chain_file)
  
  #revert object to data frame
  lifted = data.frame(lifted@unlistData) %>%
    dplyr::rename(chrom = seqnames)
  
  #deal with chr prefixes based on target assembly
  lifted = purify_chr(incoming_table = lifted, 
                      projection = target_assembly)
  
  return(lifted)
}
