#' List Attributes
#'
#' List attributes from datasets.
#' @param
#' dataset Dataset to retrieve from. Defaults to NULL.
#' @keywords data retrieval, attributes,
#' @export
#' @examples
#' listAttributes("GENE")

listAttributes<-function(dataset){
    
  #Validate mart
  if(!all(dataset %in% listDatasets())){
    print("Mart is invalid. Here is a list of all available Datasets: ")
    print(listDatasets())
    stop("Please check listDatasets() function.")
  }
  
  #Connection
  regulon<-dbConnect(SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
  result<-dbListFields(regulon,dataset)
  dbDisconnect(regulon)
  return(result)
}