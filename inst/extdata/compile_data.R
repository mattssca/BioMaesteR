#global variables
chr_table = c("chrom", "chromStart", "chromEnd", "cytoBand" ,"gieStain")
ucsc_golden_path = "http://hgdownload.cse.ucsc.edu/goldenpath/"
autosomes = paste0("chr", c(1:22))

#load packages
library(data.table)
library(stringr)
library(curl)
library(rtracklayer)

#helper function for untangling chromosome arms
untangle_chrarms = function(cyto_bands = NULL,
                            this_chr,
                            mut_start = 1,
                            padding = 0,
                            only_centromeres = FALSE){

  if(!only_centromeres){
    #remove the centromeres
    cyto_bands = cyto_bands %>%
      dplyr::filter(gieStain != "acen")
  }else{
    #keep the centromeres
    cyto_bands = cyto_bands %>%
      dplyr::filter(gieStain == "acen")
  }

  #subset to chromosome arms
  q_arms = cyto_bands %>%
    dplyr::filter(str_detect(cytoBand, "q"))

  p_arms = cyto_bands %>%
    dplyr::filter(str_detect(cytoBand, "p"))

  chr_q = q_arms %>%
    dplyr::filter(chrom == this_chr)

  chr_p = p_arms %>%
    dplyr::filter(chrom == this_chr)

  #subset to start and end of each chr arm
  chr_q_start = min(chr_q$chromStart)
  chr_q_end = max(chr_q$chromEnd)
  chr_p_start = min(chr_p$chromStart)
  chr_p_end = max(chr_p$chromEnd)

  #build a table with new data
  if(!only_centromeres){
    chr_table = tibble(chrom = c(this_chr, this_chr),
                       start = c(as.integer(chr_p_start), chr_q_start),
                       end = c(chr_p_end, chr_q_end),
                       arm = c("p", "q"))

    #mutate start coordinates
    chr_table$start = chr_table$start + mut_start

    #padding
    chr_table$start = chr_table$start + padding
    chr_table$end = chr_table$end - padding
  }else{
    chr_table = tibble(chrom = this_chr,
                       start = as.integer(chr_p_start),
                       end = as.integer(chr_q_end),
                       arm = "centromere")
  }

  #return object
  return(chr_table)
}

#hg19
#get chromosome cytobands
cytobands_hg19 = fread(paste0(ucsc_golden_path, "hg19/database/cytoBand.txt.gz"),
                       col.names = chr_table)

#run helper function in a loop to get chr arms for all selected chromosomes
chr_arms_hg19_list = lapply(autosomes, function(x){untangle_chrarms(this_chr = x,
                                                                    cyto_bands = cytobands_hg19)})

#run helper function in a loop to get centromeres
centromeres_hg19_list = lapply(autosomes, function(x){untangle_chrarms(this_chr = x,
                                                                       cyto_bands = cytobands_hg19, only_centromeres = TRUE)})

#convert list to data frame
chromosome_arms_hg19 = bind_rows(chr_arms_hg19_list)
centromeres_hg19 = bind_rows(centromeres_hg19_list)

#remove chr prefixes
chromosome_arms_grch37 = chromosome_arms_hg19 %>%
  dplyr::mutate(chrom = gsub("chr", "", chrom))

cytobands_grch37 = cytobands_hg19 %>%
  dplyr::mutate(chrom = gsub("chr", "", chrom))

centromeres_grch37 = centromeres_hg19 %>%
  dplyr::mutate(chrom = gsub("chr", "", chrom))

#enforce data frame types and column names
cytobands_grch37 = cytobands_grch37 %>%
  dplyr::rename(start = chromStart) %>%
  dplyr::rename(end = chromEnd) %>%
  dplyr::mutate(chrom = as.factor(chrom)) %>%
  dplyr::mutate(start = as.integer(start)) %>%
  dplyr::mutate(end = as.integer(end)) %>%
  dplyr::mutate(cytoBand = as.character(cytoBand)) %>%
  dplyr::mutate(gieStain = as.factor(gieStain))

chromosome_arms_grch37 = chromosome_arms_grch37 %>%
  dplyr::mutate(chrom = as.factor(chrom)) %>%
  dplyr::mutate(start = as.integer(start)) %>%
  dplyr::mutate(end = as.integer(end)) %>%
  dplyr::mutate(arm = as.factor(arm))

centromeres_grch37 = centromeres_grch37 %>%
  dplyr::mutate(chrom = as.factor(chrom)) %>%
  dplyr::mutate(start = as.integer(start)) %>%
  dplyr::mutate(end = as.integer(end)) %>%
  dplyr::mutate(arm = as.factor(arm))

#export data set
usethis::use_data(chromosome_arms_grch37, overwrite = TRUE)
usethis::use_data(cytobands_grch37, overwrite = TRUE)
usethis::use_data(centromeres_grch37, overwrite = TRUE)

#hg38
#get chromosome cytobands
cytobands_hg38 = fread(paste0(ucsc_golden_path, "hg38/database/cytoBand.txt.gz"),
                       col.names = chr_table)

#run helper function in a loop to get chr arms for all selected chromosomes
chr_arms_hg38_list = lapply(autosomes, function(x){untangle_chrarms(this_chr = x,
                                                                    cyto_bands = cytobands_hg38)})

#run helper function in a loop to get chr arms for all selected chromosomes
centromeres_hg38_list = lapply(autosomes, function(x){untangle_chrarms(this_chr = x,
                                                                       cyto_bands = cytobands_hg38,
                                                                       only_centromeres = TRUE)})

#convert list to data frame and enforce integers
chromosome_arms_hg38 = bind_rows(chr_arms_hg38_list)
centromeres_hg38 = bind_rows(centromeres_hg38_list)

#enforce data frame types and column names
cytobands_hg38 = cytobands_hg38 %>%
  dplyr::rename(start = chromStart) %>%
  dplyr::rename(end = chromEnd) %>%
  dplyr::mutate(chrom = as.factor(chrom)) %>%
  dplyr::mutate(start = as.integer(start)) %>%
  dplyr::mutate(end = as.integer(end)) %>%
  dplyr::mutate(cytoBand = as.character(cytoBand)) %>%
  dplyr::mutate(gieStain = as.factor(gieStain))

chromosome_arms_hg38 = chromosome_arms_hg38 %>%
  dplyr::mutate(chrom = as.factor(chrom)) %>%
  dplyr::mutate(start = as.integer(start)) %>%
  dplyr::mutate(end = as.integer(end)) %>%
  dplyr::mutate(arm = as.factor(arm))

centromeres_hg38 = centromeres_hg38 %>%
  dplyr::mutate(chrom = as.factor(chrom)) %>%
  dplyr::mutate(start = as.integer(start)) %>%
  dplyr::mutate(end = as.integer(end)) %>%
  dplyr::mutate(arm = as.factor(arm))

#export data set
usethis::use_data(chromosome_arms_hg38, overwrite = TRUE)
usethis::use_data(cytobands_hg38, overwrite = TRUE)
usethis::use_data(centromeres_hg38, overwrite = TRUE)

#gene coordinates
#grch37
curl::curl_download(url = "ftp.ensembl.org/pub/grch37/release-110/gtf/homo_sapiens/Homo_sapiens.GRCh37.87.gtf.gz", #note, this object is not bundled with this package
                    destfile = "inst/extdata/Homo_sapiens.GRCh37.87.gtf.gz",
                    quiet = FALSE)

#read gtf into R
gtf_grch37 = rtracklayer::import('inst/extdata/Homo_sapiens.GRCh37.87.gtf.gz')

#tidy
gene_coordinates_grch37 = as.data.frame(gtf_grch37) %>% #convert to data frame
  dplyr::filter(type == "gene") %>% #only keep genes
  dplyr::distinct(gene_id, .keep_all = TRUE) %>% #only keep unique elements (based on gene_id column)
  dplyr::mutate(seqnames = as.factor(seqnames)) %>% #convert chromosome (seqnames) variable to factor
  dplyr::rename(ensembl_gene_id = gene_id, chrom = seqnames) %>% #rename columns to match expected format
  dplyr::rename(hugo_symbol = gene_name) %>% #duplicate gene_name and rename it to hugo_symbol
  dplyr::arrange(chrom, start) %>% #sort data frame on chromosome, then start coordinates
  dplyr::select(chrom, start, end, width, strand, type, tag, ccds_id,
                ensembl_gene_id, hugo_symbol, source, score,
                gene_version, gene_source, gene_biotype,
                transcript_id, transcript_version, transcript_name,
                transcript_source, transcript_biotype,exon_number,
                exon_id, exon_version, protein_id, protein_version) #select columns

#export data set
usethis::use_data(gene_coordinates_grch37, overwrite = TRUE)

#hg38
curl::curl_download(url = "ftp.ensembl.org/pub/release-110/gtf/homo_sapiens/Homo_sapiens.GRCh38.110.gtf.gz", #note, this object is not bundled with this package
                    destfile = "inst/extdata/Homo_sapiens.GRCh38.110.gtf.gz",
                    quiet = FALSE)

#read gtf into R
gtf_hg38 = rtracklayer::import('inst/extdata/Homo_sapiens.GRCh38.110.gtf.gz')

#tidy
gene_coordinates_hg38 = as.data.frame(gtf_hg38) %>% #convert to data frame
  dplyr::filter(type == "gene") %>% #only keep genes
  dplyr::distinct(gene_id, .keep_all = TRUE) %>% #only keep unique elements (based on gene_id column)
  dplyr::mutate(seqnames = paste0("chr", seqnames)) %>% #add chromosome (seqnames) prefix
  dplyr::mutate(seqnames = as.factor(seqnames)) %>% #convert chromosome (seqnames) variable to factor
  dplyr::rename(ensembl_gene_id = gene_id, chrom = seqnames) %>% #rename columns to match expected format
  dplyr::rename(hugo_symbol = gene_name) %>% #duplicate gene_name and rename it to hugo_symbol
  dplyr::arrange(chrom, start) %>% #sort data frame on chromosome, then start coordinates
  dplyr::select(chrom, start, end, width, strand, type, tag, ccds_id,
                ensembl_gene_id, hugo_symbol, source, score,
                gene_version, gene_source, gene_biotype,
                transcript_id, transcript_version, transcript_name,
                transcript_source, transcript_biotype,exon_number,
                exon_id, exon_version, protein_id, protein_version) #select columns

#export data set
usethis::use_data(gene_coordinates_hg38, overwrite = TRUE)
