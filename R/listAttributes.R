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
    print("Dataset is invalid. Here is a list of all available Datasets: ")
    print(listDatasets())
    stop("Please check listDatasets() function.")
  }
  
  #Connection
  regulon<-RSQLite::dbConnect(RSQLite::SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
  result<-RSQLite::dbListFields(regulon,dataset)
  RSQLite::dbDisconnect(regulon)
  return(result)
}