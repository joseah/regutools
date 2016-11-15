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
    #Connect to database
    regulon<-RSQLite::dbConnect(RSQLite::SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
    #List tables
    result<-RSQLite::dbListTables(regulon)
    RSQLite::dbDisconnect(regulon)
    return(result)
}