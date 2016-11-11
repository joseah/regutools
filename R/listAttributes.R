listAttributes<-function(mart){
    
  #Validate mart
  if(!all(mart %in% listDatasets())){
    print("Mart is invalid. Here is a list of all available Datasets: ")
    print(listDatasets())
    stop("Please check listDatasets() function.")
  }
  
  #Connection
  db<-dbConnect(SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
  result<-dbListFields(db,mart)
  dbDisconnect(db)
  return(result)
}