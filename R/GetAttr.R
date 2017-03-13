#' @title Extract data from RegulonDB
#' @description This function retrieves data from RegulonDB. Data can be filtered using the filter parameter.
#' @param attributes A list or vector of attributes to be retrieved
#' @param filters A list of filters to be used. The name of the elements should correspond to the attribute used as
#' filter and the elements as the values. See details in the section of examples
#' @param dataset Dataset of interest. See ListDataset function to choose a dataset and ListAttributes() to see all
#' attributes and their descriptions available in each dataset
#' @param operator Logical operator for the filters: AND / OR.  Defaults to AND. See details in the section of examples
#' @keywords data retrieval, attributes, filters,
#' @author
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' # From "GENE" dataset, get the gene name and genome location (left position, right position and strand) of all genes regulated exclusively by Sigma Factor 19
#'
#' GetAttr(attributes = c("name", "posleft", "posright", "strand"),
#' filters = list(sigma_factor = "Sigma19"),
#' dataset = "GENE")
#' @export

GetAttr <- function(attributes = NULL, filters = NULL, dataset = NULL, and = TRUE){

  # Validate if attributes is a list or vector
  if(!is.null(attributes) & (!is.vector(attributes))){
    if(is.list(attributes) & is.data.frame(attributes))
      stop('Parameter "attributes" should be a list or vector', call.= FALSE)
  }

  # Validate if filters is a list
  if(!is.null(filters) & !is.list(filters)){
    stop('Parameter "filters" should be a list', call.= FALSE)
  }

  # Validate dataset
  if(!all(dataset %in% ListDatasets())){
    stop("Non-existing dataset used. Please check ListDatasets() function.", call.= FALSE)
  }

  # Validate attributes
  if(!all(attributes %in% ListAttributes(dataset)[["column_name"]])){
    non.existing.attrs.index <- attributes %in% ListAttributes(dataset)[["column_name"]]
    non.existing.attrs <- attributes[!non.existing.attrs.index]
    stop("Provided attribute(s) ", paste0('"',paste(non.existing.attrs, collapse = ", "), '"'),
         " do not exist. Please check ListAttributes() function.", call.= FALSE)
  }

  # Sets logical operator
  if(and){
    operator <- "AND"
  }else{
    operator <- "OR"
  }

  if(is.null(filters) & is.null(attributes)){
    query <- paste0("SELECT * FROM ", dataset, ";")
  }else if (is.null(attributes) & !is.null(filters) ) {
    cond <- BuildCondition(filters, dataset, operator)
    query <- paste0("SELECT * FROM ", dataset, " WHERE ", cond, ";")
  }else if (!is.null(attributes) & is.null(filters)){
    query <- paste0("SELECT ", paste(attributes, collapse=" , ")," FROM ", dataset, ";")
  } else {
    cond <- BuildCondition(filters, dataset, operator)
    query <- paste("SELECT ", paste(attributes, collapse = " , "), "FROM ", dataset, " WHERE ", cond , ";") #Construct query
  }
  # Connect to database
  regulon <- dbConnect(SQLite(), system.file("extdata", "regulondb_sqlite3.db", package = "regutools"))

  # Retrieve data
  result <- dbGetQuery(regulon, query)
  dbDisconnect(regulon)

  #Check if results exist
  if(!nrow(result)){
    stop("Your query produced no results. Try changing values, filters or attributes.", call.= FALSE)
  }
  return(result)
}
