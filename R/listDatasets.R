#' List available datasets in RegulonDB database
#'
#' This function returns a vector of all available datasets.
#' @param
#' No parameters are used.
#' @keywords data retrieval, datasets, database
#' @export
#' @author 
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, Jose Alquicira Hernandez
#' @examples
#' listDatasets()

listDatasets <- function(){
    #Connects to database
    regulon <- RSQLite::dbConnect(RSQLite::SQLite(), 
                                  system.file("extdata", "regulondb_sqlite3.db", 
                                              package = "regutools"))
    #Lists tables
    result <- RSQLite::dbListTables(regulon)
    RSQLite::dbDisconnect(regulon)
    return(result)
}
