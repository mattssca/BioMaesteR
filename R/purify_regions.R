#' @title Purify Regions.
#'
#' @description Helper function for cleaning and standardize regions.
#'
#' @details This function accepts a variety of incoming regions.
#' Either, regions can be provided as a data frame with `these_regions`.
#' If so, the following columns must exist; chrom, start, end.
#' This parameter (`these_regions`) also accept a region in "region" format,
#' (i.e chr:start-end). This can be a region or a vector of characters with
#' multiple regions. The user can also individually specify region(s) with;
#' `qchrom` (string), `qstart` (string, or integer), and `qend` (string or integer).
#' These parameters can also accept a vector of characters for multiple regions.
#' The function also handles chromosome prefixes in the returned object,
#' based on the selected `projection`. In addition, this function also checks if
#' the provided start coordinate is equal or greater to the end coordinate for 
#' the same chromosome. It also ensures that specified ranges are within the 
#' actual chromosomal range.
#'
#' @param these_regions The region(s) to be queried. Can be a data frame with
#' regions with the following columns; chrom, start, end.
#' Or in a string in the following format chr:start-end.
#' @param qchrom Query chromosome (prefixed or un-prefixed),
#' Required if `these_regions` is not provided.
#' @param qstart Query start position. Required if `these_regions` is not provided.
#' @param qend Query end position. Required if `these_regions` is not provided.
#' @param projection The desired projection you want back coordinates for.
#' Available projections are hg38 and grch37. Default is hg38.
#'
#' @return A data table with three columns, chrom, start, end.
#'
#' @rawNamespace import(data.table, except = c("last", "first", "between", "transpose"))
#' @import dplyr stringr
#'
#' @export
#'
#' @examples
#' #Example 1 - Give the function one region as a string
#' my_region = purify_regions(these_regions = "chr1:100-500")
#'
#' #Example 2 - Give the function multiple regions as a string
#' my_regions = purify_regions(these_regions = c("chr1:100-500", "chr2:100-500"),
#'                             projection = "grch37")
#'
#' #Example 3 - Individually specify the chromosome, start and end coordinates
#' this_region = purify_regions(qchrom = "chr1",
#'                              qstart = 100,
#'                              qend = 500)
#'
#' #Example 4 - Individually specify multiple regions with the query parameters
#' these_regions = purify_regions(qchrom = c("chr1", "chr2"),
#'                                qstart = c(100, 200),
#'                                qend = c(500, 600),
#'                                projection = "grch37")
#'
purify_regions <- function(these_regions = NULL,
                           qchrom = NULL,
                           qstart = NULL,
                           qend = NULL,
                           projection = "hg38") {

  #check if user provided too much
  if(!is.null(these_regions) && !is.null(qchrom) && !is.null(qstart) && !is.null(qend)){
    stop("You have provided multiple inputs for regions. Provide either only
         `these_regions` or a combination of `qchrom`, `qstart`, and `qend`")
  }

  #wrangle the regions provided
  if(!is.null(these_regions)){
    if(is.data.frame(these_regions)){
      #ensure column names are what's expected
      if(!all(colnames(these_regions) == c("chrom", "start", "end"))){
        stop("The column names in the `these_regions` does not match the expected format.
             The required format is chrom, start, end")
      }
      #convert to data table
      region_table = as.data.table(these_regions)

    }else if(is.character(these_regions)){
      #if the these_regions is provided as a string (region format)
      #deconvolute into a data frame with expected column names
      these_regions = data.frame(these_regions)

      split_chr = data.frame(str_split_fixed(these_regions$these_regions, ":", 2)) %>%
        dplyr::rename(chrom = X1, start_end = X2)

      split_start_end = data.frame(str_split_fixed(split_chr$start_end, "-", 2)) %>%
        dplyr::rename(start = X1, end = X2)

      regions = cbind(split_chr, split_start_end) %>%
        dplyr::select(!start_end)

      region_table = as.data.table(regions)
    }
  }else if(!is.null(qchrom) && !is.null(qstart) && !is.null(qend)){
    region = cbind(qchrom, qstart, qend) %>%
      as.data.frame() %>%
      dplyr::rename(chrom = qchrom, start = qstart, end = qend)

    region_table = as.data.table(region)
  }else{
    stop("You must provide region(s) either with the `these_regions` parameter,
         or individually specify the chromosome, start and end positions with;
         `qchrom`, `qstart`, and `qend`")
  }
  
  #enforce data types
  region_table = region_table %>%
    dplyr::mutate(chrom = as.character(chrom),
                  start = as.integer(start),
                  end = as.integer(end))
  
  #run helper function to deal with prefixes
  region_table = purify_chr(projection = projection,
                            incoming_table = region_table)
  
  #check if regions make sense 
  #is start equal or greater then end?
  for (i in 1:nrow(region_table)){
    if (region_table$start[i] >= region_table$end[i]) {
      print(paste("Row", i, "does not meet the condition: start is greater than or equal to end"))
    }
  }
  
  #check if regions are within the chromosomal range
  commpare_ranges <- function(df1, df2){
    result <- df1 %>%
      inner_join(df2, by = "chrom") %>%
      filter(start.x > end.y | end.x > end.y)
    
    if(nrow(result) > 0){
      stop("Specified start or end coordinates fall outside the actual chromosomal range")
    }
  }
  
  if(projection == "hg38"){
    commpare_ranges(region_table, chromosome_arms_hg38 %>% 
                      dplyr::filter(arm == "q"))
  }else if(projection == "grch37"){
    compare_ranges(region_table, chromosome_arms_grch37 %>% 
                     dplyr::filter(arm == "q"))
  }else{
    stop("The projection you have provided is not supported. Please choose either hg38 or grch37")
  }

  #enforce data types
  region_table$chrom = as.character(region_table$chrom)
  region_table$start = as.integer(region_table$start)
  region_table$end = as.integer(region_table$end)

  return(region_table)
}

