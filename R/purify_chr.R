#' @title Purify Chromosomes.
#'
#' @description Helper function for dealing with chromosome prefixes based on 
#' the selected projection.
#'
#' @details This function accepts a data frame with a column to check for
#' prefixes. The function will add or remove prefixes based on the selected
#' projection. This function expects a column named "chrom" to be in the 
#' incoming data frame.
#'
#' @param projection Required parameter, needed to determine if chromosomes 
#' should be prefixed or not.
#' @param incoming_table Required parameter, a data frame to check for prefixes.
#'
#' @return A data frame with the same columns as the incoming data frame, but
#' with the prefixes added or removed based on the selected projection.
#'
#' @import dplyr stringr
#'
#' @export
#'
#' @examples
#' #Example 1 - Add prefixes to a data frame
#' my_data = data.frame(chrom = c("1", "2", "3"))
#' my_data = purify_chr(projection = "hg38", incoming_table = my_data)
#'
purify_chr <- function(projection = NULL,
                       incoming_table = NULL) {
  
  #checks
  if(is.null(projection)){
    stop("You must provide a valid projection. 
         Available projections are hg38 and grch37.")
  }
  
  if(is.null(incoming_table)){
    stop("You must provide a data table with `incoming_table`.")
  }

    #deal with prefixes
  if(projection == "hg38"){
    if(all(!str_detect(incoming_table$chrom, "chr"))){
      incoming_table =  dplyr::mutate(incoming_table, chrom = paste0("chr", chrom))
    }
  }else if(projection == "grch37"){
    if(all(str_detect(incoming_table$chrom, "chr"))){
      incoming_table = dplyr::mutate(incoming_table, chrom = gsub("chr", "", chrom))
    }
  }else{
    stop(paste0("This function supports the following projections; hg38 and
                grch37. The provided projection is: ", projection))
  }
  
  return(incoming_table)
}
