#' List attributes from dataset
#'
#' Lists attributes of datasets from RegulonDB. The result of this function may
#' be used as parameter values in `getAttr` function.
#' @param
#' dataset Dataset to retrieve from. Defaults to NULL.
#' @keywords data retrieval, attributes,
#' @export
#' @author 
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' listAttributes("TF_DM")
#' listAttributes("OPERON_DM")

listAttributes <- function(dataset){

  #Validate mart
  if(!all(dataset %in% listDatasets())){
    cat("Dataset is invalid. Here is a list of all available datasets: \n")
    cat(paste(listDatasets(), collapse = "\n"))
    stop("Please check listDatasets() function.")
  }

  #Connection
  regulon <- RSQLite::dbConnect(RSQLite::SQLite(), 
                                system.file("extdata", "regulondb_sqlite3.db", 
                                            package = "regutools"))
  result <- RSQLite::dbListFields(regulon, dataset)
  RSQLite::dbDisconnect(regulon)
  return(result)
}
