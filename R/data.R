#' Chromosome Arms grch37.
#'
#' A data frame with the chromosome arm coordinates in respect to grch37.
#'
#' @format ## `chromosome_arms_grch37`
#' A data frame with 44 rows and 4 columns.
#' \describe{
#'   \item{chrom}{Factor. Chromosomes without chr-prefix, 1:22.}
#'   \item{start}{Integer. Start coordinates for the specified chromosome arm.}
#'   \item{end}{Integer. End coordinates for the specified chromosome arm.}
#'   \item{arm}{Factor. Chromosome arm, either p or q.}
#' }
"chromosome_arms_grch37"


#' Chromosome Arms hg38.
#'
#' A data frame with the chromosome arm coordinates in respect to hg38.
#'
#' @format ## `chromosome_arms_hg38`
#' A data frame with 44 rows and 4 columns.
#' \describe{
#'   \item{chrom}{Factor. chr-prefixed chromosomes, chr1-chr22.}
#'   \item{start}{Integer. Start coordinates for the specified chromosome arm.}
#'   \item{end}{Integer. End coordinates for the specified chromosome arm.}
#'   \item{arm}{Factor. Chromosome arm, either p or q.}
#' }
"chromosome_arms_hg38"


#' Centromeres grch37
#'
#' A data frame with the centromeres coordinates in respect to grch37.
#'
#' @format ## `centromeres_grch37`
#' A data frame with 22 rows and 4 columns.
#' \describe{
#'   \item{chrom}{Factor. Chromosomes without chr-prefix, 1:22.}
#'   \item{start}{Integer. Start coordinates for the specified chromosome arm.}
#'   \item{end}{Integer. End coordinates for the specified chromosome arm.}
#'   \item{arm}{Factor. Chromosome arm, (always centromere).}
#' }
"centromeres_grch37"


#' Centromeres hg38
#'
#' A data frame with the centromeres coordinates in respect to hg38.
#'
#' @format ## `centromeres_hg38`
#' A data frame with 22 rows and 4 columns.
#' \describe{
#'   \item{chrom}{Factor. chr-prefixed chromosomes, chr1-chr22.}
#'   \item{start}{Integer. Start coordinates for the specified chromosome arm.}
#'   \item{end}{Integer. End coordinates for the specified chromosome arm.}
#'   \item{arm}{Factor. Chromosome arm (always centromere).}
#' }
"centromeres_hg38"


#' Cytobands grch37
#'
#' A data frame with the cytobands coordinates in respect to grch37.
#'
#' @format ## `cytobands_grch37`
#' A data frame with 862 rows and 5 columns.
#' \describe{
#'   \item{chrom}{Factor. Chromosomes without chr-prefix, 1:22.}
#'   \item{start}{Integer. Start coordinates for the specified chromosome arm.}
#'   \item{end}{Integer. End coordinates for the specified chromosome arm.}
#'   \item{cytoBand}{Character. Cytoband name for the specified range.}
#'   \item{gieStain}{Factor. Giemsa Stain proporties for the described cyto band.}
#' }
"cytobands_grch37"


#' Cytobands hg38
#'
#' A data frame with the cytobands coordinates in respect to hg38.
#'
#' @format ## `cytobands_hg38`
#' A data frame with 1549 rows and 5 columns.
#' \describe{
#'   \item{chrom}{Factor. chr-prefixed chromosomes, chr1-chr22.}
#'   \item{start}{Integer. Start coordinates for the specified chromosome arm.}
#'   \item{end}{Integer. End coordinates for the specified chromosome arm.}
#'   \item{cytoBand}{Character. Cytoband name for the specified range.}
#'   \item{gieStain}{Factor. Giemsa Stain proporties for the described cyto band.}
#' }
"cytobands_hg38"


#' Gene Coordinates grch37.
#'
#' All gene coordinates in respect to grch37. This data set is downloaded from ensembl.
#'
#' @format ## `gene_coordinates_grch37`
#' A data frame with 57905 rows and 5 columns.
#' \describe{
#'   \item{chrom}{Factor. The chromosome (un-prefixed) that the gene is residing on.}
#'   \item{start}{Integer. The start coordinates for the gene.}
#'   \item{end}{Integer. The end coordinates for the gene.}
#'   \item{width}{Integer. The width for the gene.}.
#'   \item{strand}{Defined as + forward or - reverse.}.
#'   \item{type}{Feature type name. Current allowed features are; gene, transcript, exon, CDS, Selenocysteine, start_codon, stop_codon and UTR}.
#'   \item{tag}{A collection of additional key value tags}.
#'   \item{ccds_id}{CCDS identifier linked to this transcript}.
#'   \item{ensembl_gene_id}{Character. Ensembl gene ID.}
#'   \item{hugo_symbol}{Character. Gene symbol in Hugo format.}
#'   \item{source}{Name of the program that generated this feature, or the data source (database or project name)}.
#'   \item{score}{A floating point value indiciating the score of a feature}.
#'   \item{gene_version}{The stable identifier version for the gene}.
#'   \item{gene_source}{The annotation source for this gene}.
#'   \item{gene_biotype}{The biotype of this gene}.
#'   \item{transcript_id}{The stable identifier for this transcript}.
#'   \item{transcript_version}{The stable identifier version for this transcript}.
#'   \item{transcript_name}{The symbold for this transcript derived from the gene name}.
#'   \item{transcript_source}{The annotation source for this transcript}.
#'   \item{transcript_biotype}{The biotype for this transcript}.
#'   \item{exon_number}{Position of this exon in the transcript}.
#'   \item{exon_id}{The stable identifier for this exon}.
#'   \item{exon_version}{The stable identifier version for this exon}.
#'   \item{protein_id}{Stable identifier for this transcript's protein}.
#'   \item{protein_version}{Stable identifier version for this transcript's protein}.
#' }
"gene_coordinates_grch37"


#' Gene Coordinates hg38.
#'
#' All gene coordinates in respect to hg38. This data set is downloaded from ensembl.
#'
#' @format ## `gene_coordinates_hg38`
#' A data frame with 62754 rows and 5 columns.
#' \describe{
#'   \item{chrom}{Factor. The chromosome (un-prefixed) that the gene is residing on.}
#'   \item{start}{Integer. The start coordinates for the gene.}
#'   \item{end}{Integer. The end coordinates for the gene.}
#'   \item{width}{Integer. The width for the gene.}.
#'   \item{strand}{Defined as + forward or - reverse.}.
#'   \item{type}{Feature type name. Current allowed features are; gene, transcript, exon, CDS, Selenocysteine, start_codon, stop_codon and UTR}.
#'   \item{tag}{A collection of additional key value tags}.
#'   \item{ccds_id}{CCDS identifier linked to this transcript}.
#'   \item{ensembl_gene_id}{Character. Ensembl gene ID.}
#'   \item{hugo_symbol}{Character. Gene symbol in Hugo format.}
#'   \item{source}{Name of the program that generated this feature, or the data source (database or project name)}.
#'   \item{score}{A floating point value indiciating the score of a feature}.
#'   \item{gene_version}{The stable identifier version for the gene}.
#'   \item{gene_source}{The annotation source for this gene}.
#'   \item{gene_biotype}{The biotype of this gene}.
#'   \item{transcript_id}{The stable identifier for this transcript}.
#'   \item{transcript_version}{The stable identifier version for this transcript}.
#'   \item{transcript_name}{The symbold for this transcript derived from the gene name}.
#'   \item{transcript_source}{The annotation source for this transcript}.
#'   \item{transcript_biotype}{The biotype for this transcript}.
#'   \item{exon_number}{Position of this exon in the transcript}.
#'   \item{exon_id}{The stable identifier for this exon}.
#'   \item{exon_version}{The stable identifier version for this exon}.
#'   \item{protein_id}{Stable identifier for this transcript's protein}.
#'   \item{protein_version}{Stable identifier version for this transcript's protein}.
#' }
"gene_coordinates_hg38"
