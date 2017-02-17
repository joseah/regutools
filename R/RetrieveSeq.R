#' @title Retrieve sequences from RSAT
#' @description Returns upstream, downstream or coding DNA sequences for list of query genes from RSAT.
#' @author José Alquicira Hernández
#' @param organism Organism. Words need to be underscore separated (example: Escherichia_coli_K12).
#' @param genes A list of genes
#' @param all Return sequences for all the genes of the organism if TRUE. Incompatible with `genes`.
#' @param noorf Prevent overlap with upstream open reading frames (ORF) if value TRUE.
#' @param from Inferior limit of the region to retrieve. Default is organism dependant (example: Saccharomyces cerevisiae = -800).
#' @param to Superior limit of the region to retrieve. Default is '-1'.
#' @param feattype Type of genome features to load. Supported: CDS, mRNA, tRNA, rRNA.
#' @param type Sequence type. Supported: 'upstream', 'downstream', 'ORF' (unspliced open reading frame).
#' @param format Sequence format. Supported: IG (Intelligenetics), WC (wconsensus), raw, fasta
#' @param lw Line width (0 for whole sequence on one line).
#' @param label Field(s) to be used in the sequence label. Multiple fields can be specified, separated by commas. Supported: id, name, organism_name, sequence_type, current_from, current_to, ctg, orf_strand, reg_left, reg_right. Default: name.
#' @param label.sep Separator between the label fields. Default: '|' (pipe character).
#' @param nocom No comments if TRUE. Only the identifier and the sequence are returned. By default, the comment indicates the ORF and upstream sequence coordinates.
#' @param repeat.mask Use the repeat masked version of the genome if TRUE. Attention: repeated regions are annotated for some genomes only.
#' @param imp.pos Admit imprecise positions if value TRUE
#' @return an string with results retrieved from RSAT
#' @examples
#' # Retrieve sequence for araC gene in *E.coli* from -70 to +70.
#' res <- RetrieveSeq(organism = "Escherichia_coli_K12", genes = 'araC')
#' cat(res)
#' @export



RetrieveSeq <- function(organism, genes = NULL, all = FALSE, noorf = FALSE, from = NULL , to = NULL,
                         feattype = NULL, type = NULL, format = NULL, lw = NULL, label = NULL,
                         label.sep = NULL, nocom = FALSE, repeat.mask = FALSE, imp.pos = FALSE){

  if(organism == '' | is.na(organism) | is.null(organism)){
    stop("An organism is required to retrieve sequences")
  }

  all <- ifelse(all, 1, 0)
  nocom <- ifelse(nocom, 1, 0)
  repeat.mask <- ifelse(repeat.mask, 1, 0)
  imp.pos <- ifelse(imp.pos, 1, 0)


  parameters <- list(organism = organism,
                     query = genes,
                     all = all,
                     noorf = noorf,
                     from = from,
                     to = to,
                     feattype = feattype,
                     type = type,
                     format = format,
                     lw = lw,
                     label = label,
                     label_sep = label.sep,
                     nocom = nocom,
                     'repeat' = repeat.mask,
                     imp_pos = imp.pos)

  res <- RSAT(method = 'retrieve_seq', parameters = parameters)
  return(res)
}
