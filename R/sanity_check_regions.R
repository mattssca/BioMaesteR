#' @title Sanity Check Regions.
#'
#' @description Helper function ensuring the provided regions are valid.
#'
#' @details This function checks if the provided start coordinate is equal or greater
#' to the end coordinate for the same chromosome. It also ensures that specified ranges
#' are within the actual chromosomal range.
#'
#' @param incoming_regions A data frame with regions with the following columns;
#' chrom, start, end.
#' @param projection The projection.
#'
#' @return Nothing.
#'
#' @import dplyr 
#' 
#' @export
#'
#' @examples
#' #Example 1 - Give the function one region as a string
#' my_regions = data.frame(chrom = c("chr1", "chr2"), start = c(100, 100), end = c(200, 200))
#' sanity_check_regions(incoming_regions = my_regions, projection = "hg38")
#' 
sanity_check_regions <- function(incoming_regions = NULL, 
                                 projection = NULL){
  
  if(is.null(incoming_regions)){
    stop("No regions provided")
  }
  
  if(projection == "hg38"){
    chromosome_ranges = chromosome_arms_hg38 %>% 
      dplyr::filter(arm == "q")
  }else if(projection == "grch37"){
    chromosome_ranges = chromosome_arms_grch37 %>% 
      dplyr::filter(arm == "q")
  }else{
    stop("Invalid projection specified")
  }
  
  #check if start is greater than end
  for (i in 1:nrow(incoming_regions)){
    if (incoming_regions$start[i] >= incoming_regions$end[i]) {
      stop(paste("Row", i, "does not meet the condition: start is greater than or equal to end"))
    }
  }
  
  #check if start or end is outside the actual chromosomal range
  result <- incoming_regions %>%
    inner_join(chromosome_ranges, by = "chrom") %>%
    filter(start.x > end.y | end.x > end.y)
  
  if(nrow(result) > 0){
    stop("Specified start or end coordinates fall outside the actual chromosomal range")
  }
  return()
}
