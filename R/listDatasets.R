#' List available datasets in RegulonDB database
#'
#' This function returns a vector of all available datasets. No parameters are provided.
#' @keywords data retrieval, datasets, database,
#' @export
#' @author 
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, Josá Alquicira Hernández
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
