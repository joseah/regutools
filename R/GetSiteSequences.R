#' @title Extract sequences from a set of sites.
#' @description Return the sequences for a set of binding sites.
#' @author José & Jacques van Helden
#' @param tfbs a collection of sites previously retrieved from RegulonDB with the function GetSitesForTF()
#' @param seq.format="fasta" output format. Supported: "fasta", "wconsensus".
#' @return a string with the sequences
#' @examples
#' ## Get the binding sites for AraC and extract their sequence in fasta format
#' tfbs <- GetSitesForTF(TF = "AraC")
#' tfbs.seq <- GetSiteSequences(tfbs) # This returns a single string with "\n" return character
#' cat(tfbs.seq)
#' @export
GetSiteSequences <- function(tfbs,
                             seq.format="fasta") {
  if (seq.format == "fasta") {
    tfbs$sites$header <- paste(sep="", ">", tfbs$sites$ID, "_", tfbs$sites$left, ":", tfbs$sites$right, ":", tfbs$sites$strand)
    seq.string <- paste(sep="\n", header, as.vector(tfbs$sites$sequence), collapse = "\n")
  } else {
    stop(paste(seq.format, " is not a valid format for GetSiteSequences()."))
  }

  return(seq.string)
}
