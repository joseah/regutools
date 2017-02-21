#' @title Get sites for a selected TF.
#' @description Retrieve the binding sites for a given transcription factor.
#' @author José Alquicira HErnández & Jacques van Helden
#' @param TF name of the transcription factor of interest
#' @return a list comprising the query TF name, the number of binding sites, and a table with the TFBS descriptions.
#' @examples
#' ## Extract the binding sites for AraC
#' tfbs <- GetSitesForTF(TF = "AraC")
#' message("Got ", tfbs$nb.sites, " binding sites for ", tfbs$TF)
#' head(tfbs$sites) # Print the 10 first binding sistes
#' @export
GetSitesForTF <- function (TF) {
  tfbs.raw <- getAttr(
    attributes = c(
      "name",
      "tfbs_unique"),
    filters = "name",
    values = TF,
    dataset = "TF")

  # convert raw info into a table with 1 row per TFBS
  # and 1 col per attribute
  tfbs.csv <- unlist(strsplit(x = tfbs.raw$tfbs_unique, split =  ";"))
  tfbs.table <- as.data.frame(
    Reduce(rbind,strsplit(x = tfbs.csv, split = ",")))
  row.names(tfbs.table) <- NULL

  ## TO DO:
  ## - add chromosome (for future multi-organisms ReuglonDB)
  ## - add evidence
  names(tfbs.table) <- c("ID", "left", "right", "strand", "sequence")

  result <- list(
    "TF" = TF,
    "nb.sites" = nrow(tfbs.table),
    "sites" = tfbs.table
  )
  return(result)
}
