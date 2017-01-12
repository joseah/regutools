#' Get Attributes
#'
#' Retrieve information based on attributes and filters from datasets.
#' @param attributes Attributes to be retrieved.
#' @param filters   Filters to be used.
#' @param values Values to filter with.
#' @param dataset Dataset to retrieve from.
#' @param operator  Logical operator for the filters: AND / OR.  Defaults to AND.
#' @keywords data retrieval, attributes, filters,
#' @export
#' @author 
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples 
#' # Retrieve all genes included in all operons 
#' # regulated by CRP transcriptional factor.
#' 
#' getAttr(attributes = c("operon_name", "op_gene_names"), 
#' filters = c("op_tf_names"), 
#' values = "CRP", 
#' dataset = "OPERON_DM")

getAttr <- function(attributes = NULL, filters = NULL, values = NULL, dataset = NULL, operator = 'AND'){
  # Validate dataset
  if(!all(dataset %in% listDatasets())){ #
    stop("Non-existing dataset used. Please check listDatasets() function.", call.= FALSE)
  }

  # Validate attributes
  if(!all(attributes %in% listAttributes(dataset))){
    stop("The attributes to be retrieved do not exist. Please check listAttributes() function.", call.= FALSE)
  }

  # Validate if filters and values are the same size. Note: Also validates if either filters or values are null.
  if(length(filters) != length(values)){
    stop("'filters' and 'values' must be vectors of the same length.", call.= FALSE)
  }

  # Query database
  if(is.null(filters)){
    query <- paste("SELECT ", paste(attributes, collapse=" , ")," FROM ", dataset, "; ",sep = "")
  } else {

    # Validate filters
    if(!all(filters %in% listAttributes(dataset))){
      stop("Provided filters do not exist. Please check listAttributes() function.", call.= FALSE)}

    # Query database
    query <- paste(" ", filters[1], " = '", values[1], "'", sep = "")
    if(length(filters) > 1){
      query <- c(query, apply(cbind(filters,values), 1, function(x){
        result <- paste(" ", x[1], " = '", x[2],"\"", sep = "") # x can be either a filter: x[1] or a value: x[2]
        return(result)
      })
      )
      query <- paste(query, collapse = operator)
    }
    query <- paste("SELECT ", paste(attributes, collapse = " , "), " FROM ", dataset, " WHERE ", query, sep = "") #Construct query

  }

  # Connect to database
  regulon<-RSQLite::dbConnect(RSQLite::SQLite(), system.file("extdata", "regulondb_sqlite3.db", package = "regutools"))

  # Retrieve data
  result <- RSQLite::dbGetQuery(regulon, query)
  RSQLite::dbDisconnect(regulon)

  # Check if results exist
  if(!nrow(result)){
    stop("Your query produced no results. Try changing values, filters or attributes.", call.= FALSE)
  }
  return(result)
}
