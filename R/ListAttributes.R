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

ListAttributes <- function(dataset, comments=FALSE){

  # Check if data base exists
  if(system.file("extdata", "regulondb_sqlite3.db",
                 package = "regutools")==""){
    stop("Please download the data base using the GetDatabase() function.",call.=FALSE)
  }

  # Validate mart
  if(!all(dataset %in% ListDatasets())){
    cat("Dataset is invalid. These are all available datasets:\n")
    cat(paste(ListDatasets(), collapse = "\n"))
    stop("Please check ListDatasets() function.")
  }

  # Connect to database
  regulon <- dbConnect(SQLite(),
                       system.file("extdata", "regulondb_sqlite3.db", package = "regutools"))

  # Query REGULONDB_OBJECTS table
  if (comments){
    query <- paste0("SELECT attribute, description FROM REGULONDB_OBJECTS WHERE table_name = '", dataset, "';")
    # Retrieve data
    result <- dbGetQuery(regulon, query)
    dbDisconnect(regulon)
    # Temporary solution for attributes
    result$attribute <- tolower(result$attribute)
    if(dim(result)[1]==0){
      stop("Attribute descriptions for this dataset are not currently available.")
    }

    return(result)

    }else{
      #query <- paste0("SELECT attribute FROM REGULONDB_OBJECTS WHERE table_name = '", dataset, "';"
      result <- dbListFields(conn = regulon, name = dataset)
      result <- data.frame("attribute"=result)
      return(result)
    }




}
