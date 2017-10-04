#' @title List available datasets in RegulonDB database
#' @description  This function returns a vector of all available datasets. No parameters are provided.
#' @keywords data retrieval, datasets, database,
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, Josá Alquicira Hernández
#' @examples
#' listDatasets()
#' @export

ListDatasets <- function(){

  # Check if data base exists
  if(system.file("extdata", "regulondb_sqlite3.db",
                 package = "regutools")==""){
    stop("Please download the database using the GetDatabase() function.",call.=FALSE)
  }

  # Connect to database
  regulon <- dbConnect(SQLite(),
                       system.file("extdata", "regulondb_sqlite3.db",
                                   package = "regutools"))
  # List tables
  result <- dbListTables(regulon)
  dbDisconnect(regulon)
  return(result)
}
