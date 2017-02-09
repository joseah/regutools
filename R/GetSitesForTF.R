#' @title Get sites for a selected TF.
#' @description Retrieve the binding sites for a given transcripiton factor.
#' @author José & Jacques van Helden
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
      "transcription_factor_name",
      "tf_tfbs_unique"),
    filters = "transcription_factor_name",
    values = TF,
    dataset = "TF_DM")

  # convert raw info into a table with 1 row per TFBS
  # and 1 col per attribute
  tfbs.csv <- unlist(strsplit(x = tfbs.raw$tf_tfbs_unique, split =  ";"))
  tfbs.table <- as.data.frame(
    Reduce(rbind,strsplit(x = tfbs.csv, split = ",")))
  row.names(tfbs.table) <- NULL

  ## TEMPORARY: this should be replaced by the site ID
  tfbs.table$ID <- paste(sep="_", TF, 1:nrow(tfbs.table))

  ## TO DO:
  ## - add site ID
  ## - add chromosome (for future multi-organisms ReuglonDB)
  ## - add evidence
  names(tfbs.table) <- c("left", "right", "strand", "sequence", "ID")

  result <- list(
    "TF" = TF,
    "nb.sites" = nrow(tfbs.table),
    "sites" = tfbs.table
  )
  return(result)
}
