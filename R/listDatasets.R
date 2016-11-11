listDatasets<-function(){
  db<-dbConnect(SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
  result<-dbListTables(db)
  dbDisconnect(db)
  return(result)
}