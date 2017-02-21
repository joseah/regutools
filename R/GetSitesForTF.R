#' @title Get binding sites for a selected Transcription Factor
#' @description Retrieve the binding sites and genome location for a given transcription factor.
#' @author José Alquicira Hernández & Jacques van Helden
#' @param tf name of the transcription factor of interest
#' @return a dataframe with the TFBS descriptions. If the TF does not exist, return a NA.
#' @examples
#' ## Extract the binding sites for AraC
#' tfbs <- GetSitesForTF(tf = "AraC")
#' @export
#'

GetSitesForTF <- function(tf) {

  tfbs.raw <- tryCatch({
    getAttr(attributes = c("name", "tfbs_unique"),
            filters = "name",
            values = tf,
            dataset = "TF")
  },error = function(cond) return(NULL))

  if(is.null(tfbs.raw)){
    return(NA)
  }else{
    # convert raw info into a table with 1 row per TFBS
    # and 1 col per attribute
    tfbs.csv <- unlist(strsplit(x = tfbs.raw$tfbs_unique, split =  ";"))
  tfbs.table <- as.data.frame(
    Reduce(rbind, strsplit(x = tfbs.csv, split = ",")), row.names = FALSE)

  ## TO DO:
  ## - add chromosome (for future multi-organisms RegulonDB)
  ## - add evidence
  names(tfbs.table) <- c("ID", "left", "right", "strand", "sequence")

  return(tfbs.table)
  }
}
