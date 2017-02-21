#' @title Extract sequences from a set of Transcription Factor Binding Sites (TFBSs)
#' @description Returns the sequences for a set of binding sites.
#' @author José Alquicira Hernández & Jacques van Helden
#' @param tfbs A collection of sites previously retrieved from RegulonDB with the function \code{GetSitesForTF()}
#' @param seq.format Default: \code{fasta} output format. Supported: \code{fasta}, \code{wconsensus}.
#' @return a string with the sequences
#' @examples
#' ## Get the binding sites for AraC and extract their sequence in fasta format
#' tfbs <- GetSitesForTF(tf = "AraC")
#' tfbs.seq <- GetSiteSequences(tfbs) # This returns a single string with "\n" breakline character
#' cat(tfbs.seq)
#' @export


GetSiteSequences <- function(tfbs, seq.format = "fasta") {

  if(!(is.data.frame(tfbs) & all(names(tfbs) %in% c("ID", "left", "right", "strand", "sequence")))){
    stop("Must provide a valid colletion of sites retrieved with GetSitesForTF() function")
  }

  if (seq.format == "fasta") {

    header <- paste0(">", tfbs$ID, "_", tfbs$left, ":", tfbs$right, ":", tfbs$strand)
    seq.string <- paste(header, as.vector(tfbs$sequence), collapse = "\n", sep = "\n")

  } else {

    stop(paste(seq.format, " is not a valid format for GetSiteSequences()."))

  }

  return(seq.string)

}
