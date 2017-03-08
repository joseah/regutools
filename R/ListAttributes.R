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
  if(!all(dataset %in% listDatasets())){
    cat("Dataset is invalid. These are all available datasets:\n")
    cat(paste(ListDatasets(), collapse = "\n"))
    stop("Please check listDatasets() function.")
  }

  # Get attributes and descriptions
  result <- getAttr(attributes = c("column_name", "comments"),
                    filter = "table_name",
                    values = dataset,
                    dataset = "REGULONDB_OBJECTS")

  return(result)
}
