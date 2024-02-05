#' @title Bin Splitter
#' 
#' @description This function takes a region and splits it into bins of a 
#' specified size.
#' 
#' @details This function internally calls the purify_regions function to 
#' properly format the incoming regions.Thus, this function can accept either a 
#' data frame of regions or individual region coordinates.
#'
#' @param these_regions The region(s) to be queried. Can be a data frame with
#' regions with the following columns; chrom, start, end.
#' Or in a string in the following format chr:start-end.
#' @param qchrom Query chromosome (prefixed or un-prefixed),
#' Required if `these_regions` is not provided.
#' @param qstart Query start position. Required if `these_regions` is not provided.
#' @param qend Query end position. Required if `these_regions` is not provided.
#' @param bin_size The size of the bins to split the regions into, default is 1000.
#'
#' @return A data frame with input regions binned into the specified bin size.
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' #Example 1 - Call function with regions specified individually
#' my_bins = bin_splitter(qchrom = c("chr1", "chr2"), 
#'                        qstart = c(100, 200), 
#'                        qend = c(200, 300),
#'                        bin_size = 10)
#'
#' #Example 2 - Call the funciton with regions from a data frame
#' my_regions = purify_regions(these_regions = c("chr7:1000-500000", "chr8:50000-7000000"))
#' these_bins = bin_splitter(these_regions = my_regions, bin_size = 100000)
#' 
bin_splitter = function(these_regions = NULL, 
                        qchrom = NULL,
                        qstart = NULL,
                        qend = NULL,
                        bin_size = 1000){
  
  #call helper to format regions
  my_regions = purify_regions(these_regions = these_regions,
                              qchrom = qchrom,
                              qstart = qstart,
                              qend = qend)
  
  bin_region = function(my_regions, 
                        bin_size){
    
    bin_df = data.frame(bin_chr = my_regions$chrom, 
                        bin_start = seq(my_regions$start, my_regions$end, bin_size))
    
    bin_df = mutate(bin_df, bin_end = bin_start + bin_size) %>%
      dplyr::filter(bin_end <= my_regions$end)
    
    return(bin_df)
  }
  
  #call helper to bin the regions
  binned_regions = lapply(1:nrow(my_regions), function(i) bin_region(my_regions[i,], bin_size))
  binned_regions = do.call(rbind, binned_regions)
  
  return(binned_regions)
}
