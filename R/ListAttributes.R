#' @title List attributes from dataset
#' @description Lists all attributes and their descriptions of a dataset from RegulonDB. The result of this function may
#' be used as parameter values in \code{GetAttr} function.
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @keywords data retrieval, attributes,
#' @param
#' dataset Dataset of interest.
#' @return A data frame with two columns:
#' \itemize{
#' \item \code{attribute}. Name of the attribute
#' \item \code{description}. Description of attribute
#' }
#' @examples
#' ListAttributes("TF")
#' ListAttributes("OPERON")
#' @export

ListAttributes <- function(dataset){

  # Validate mart
  if(!all(dataset %in% ListDatasets())){
    cat("Dataset is invalid. These are all available datasets:\n")
    cat(paste(ListDatasets(), collapse = "\n"))
    stop("Please check ListDatasets() function.")
  }

  # Query REGULONDB_OBJECTS table
  query <- paste0("SELECT column_name, comments FROM REGULONDB_OBJECTS WHERE table_name = '", dataset, "';")

  # Connect to database
  regulon <- dbConnect(SQLite(),
                       system.file("extdata", "regulondb_sqlite3.db", package = "regutools"))

  # Retrieve data
  result <- dbGetQuery(regulon, query)
  dbDisconnect(regulon)

  # Temporary solution for attributes
  result$column_name <- tolower(result$column_name)

  return(result)
}
