#' Get Attributes
#'
#' Retrieve attributes from datasets.
#' @param
#' attributes Attributes to be retrieved.  Defaults to NULL.
#' filters   Filters to be used.  Defaults to NULL.
#' values Values to filter with.  Defaults to NULL.
#' dataset Dataset to retrieve from. Defaults to NULL.
#' operator  Logical operator for the filters: AND / OR.  Defaults to AND.
#' @keywords data retrieval, attributes,
#' @export
#' @examples
#' getAttr(attributes=c("gene_id"), dataset="GENE")

getAttr<-function(attributes=NULL, filters=NULL, values=NULL, dataset=NULL, operator='AND'){
  #Validate dataset
  if(!all(dataset %in% listDatasets())){ #
    stop("The dataset you use dont exist in the database. Check function listDatasets()",call.= FALSE)
  }
  
  #Validate attributes
  if(!all(attributes %in% listAttributes(dataset))){
    stop("The attributes to be retrieved do not exist. Please check listAttributes() function.",call.= FALSE)
  }
  
  #Validate if filters and values are the same size. Note: Also validates if either filters or values are null. 
  if(length(filters)!=length(values)){ 
    stop("Parameters 'filters' and 'values' must be vectors of the same length.",call.= FALSE)
  }
  
  #Make query
  if(is.null(filters)){
    query<-paste("SELECT ", paste(attributes, collapse=" , ")," FROM ", dataset, "; ",sep="")
  } else {
    
    #Validate filters
    if(!all(filters %in% listAttributes(dataset))){
      stop("The filters do not exist. Please check listAttributes() function.",call.= FALSE)}
    
    #Make Query
    query <- paste(" ",filters[1]," = '",values[1],"'", sep="") 
    if(length(filters) > 1){
      query<-c(query, apply(cbind(filters,values),1,function(x){ # Is x a meaningful name?
        result<-paste(" ", x[1], " = '", x[2],"\"",sep="") # x can be either a filter: x[1] or a value: x[2]  
        return(result)
      })
      )
      query<-paste(query,collapse=operator)
    }
    query<-paste("SELECT ", paste(attributes, collapse=" , ")," FROM ", dataset, " WHERE ", query, sep="") #Construct query
    
  }
  
  #Connect to database
  regulon<-RSQLite::dbConnect(RSQLite::SQLite(), system.file("extdata", "regulondb_92_sqlite3.db", package = "regutools"))
  
  #Retrieve data
  result <-RSQLite::dbGetQuery(regulon, query)
  RSQLite::dbDisconnect(regulon)
  
  #Check if results are empty 
  if(!nrow(result)){
    stop("Your query produced no results. Try changing the filters, values or attributes to be retrieved.",call.= FALSE)
  }
  return(result)
}