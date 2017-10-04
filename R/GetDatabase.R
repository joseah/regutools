#' @title Download up to date RegulonDB lite from GitHub
#' @description  This function connects to GitHub to download the most up to date version of the Regulon Database. No parameters are provided. The database is stored in the inst/extdata directory of the package.
#' @keywords data retrieval, datasets, database
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, Josá Alquicira Hernández
#' @examples
#' GetDatabase()
#' @export


GetDatabase<-function(){

  #Set path
  path <- paste0(path.package( package = "regutools"),"/extdata/regulondb_sqlite3.db")

  #Set database url
  db_url<-"https://www.dropbox.com/s/vhba4c2s7gsdm80/regulondb_sqlite3.db"

  #Use httr::GET to retrieve database from GitHub
  r <- httr::GET(db_url,
                 write_disk(path, overwrite = TRUE))
  #Check response
  stop_for_status(r)
  warn_for_status(r)
  #message_for_status(r)
}

