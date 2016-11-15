#' List Datasets
#'
#' List datasets available.
#' @param
#' No parameters are used.
#' @keywords data retrieval, attributes,
#' @export
#' @examples
#' listAttributes("GENE")

listDatasets<-function(){
  regulon<-dbConnect(SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
  result<-dbListTables(regulon)
  dbDisconnect(regulon)
  return(result)
}