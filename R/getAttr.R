#' Get data from RegulonDB
#'
#' Retrieve information based on attributes and filters from datasets in RegulonDB.
#' @param attributes Attributes to be retrieved.
#' @param filters   Filters to be used.
#' @param values Values to filter with.
#' @param dataset Dataset to retrieve from.
#' @param operator  Logical operator for the filters: AND / OR.  Defaults to AND.
#' @keywords data retrieval, attributes, filters,
#' @export
#' @author
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' # Retrieve all genes included in all operons
#' # regulated by CRP transcriptional factor.
#'
#' getAttr(attributes = c("operon_name", "op_gene_names"),
#' filters = c("op_tf_names"),
#' values = "CRP",
#' dataset = "OPERON_DM")

getAttr <- function(attributes = NULL, filters= NULL, dataset = NULL, and = TRUE){
  if (and){
    operator<-"AND"
  }else{
    operator<-"OR"
  }
  # Validate dataset
  if(!all(dataset %in% regutools::listDatasets())){
    stop("Non-existing dataset used. Please check listDatasets() function.", call.= FALSE)
  }

  # Validate attributes
  if(!all(attributes %in% regutools::listAttributes(dataset))){
    At_NotEx<-(attributes %in% regutools::listAttributes(dataset))
    Names_AtNotEx<-attributes[which(!At_NotEx)]
    #Faltasepararlosnombre
    stop("The attribute(s) ", Names_AtNotEx , " not exist in the data set . Please check listAttributes() function.", call.= FALSE)
  }

  if(is.null(filters) & is.null(attributes)){
      query <- paste0("SELECT * FROM ", dataset, "; ")
  }else if (is.null(attributes) & !is.null(filters) ) {
    cond<-BuildCondition(filters,dataset,operator)
    query<- paste0("SELECT * FROM ", dataset, " WHERE ", cond, "; ")
  }else if (!is.null(attributes) & is.null(filters)){
    query <- paste0("SELECT ", paste(attributes, collapse=" , ")," FROM ", dataset, "; ")
  } else {
    cond<-BuildCondition(filters,dataset,operator)
    query <- paste("SELECT ", paste(attributes, collapse = " , "), "FROM ", dataset, " WHERE ", cond , ";") #Construct query
  }
  # Connect to database
  regulon <- RSQLite::dbConnect(RSQLite::SQLite(), system.file("extdata", "regulondb_sqlite3.db", package = "regutools"))
  # return(query)
  # Retrieve data
  result <- RSQLite::dbGetQuery(regulon, query)
  RSQLite::dbDisconnect(regulon)

  #Check if results exist
  if(!nrow(result)){
    stop("Your query produced no results. Try changing values, filters or attributes.", call.= FALSE)
  }
  return(result)
}
