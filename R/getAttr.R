getAttr<-function(attributes=NULL, filters=NULL, values=NULL, mart=NULL, operator='AND'){
  
  #Validate attributes
  if(!prod(attributes %in% listAttributes(mart))){stop("The attributes to be retrieved do not exist. Please check listAttributes() function.")}
  
  #Make query
  if(is.null(filters)){
    query<-paste("SELECT ", paste(attributes, collapse=" , ")," FROM ", mart,sep="")
  } else {
    
    #Validate filters
    if(length(filters)!=length(values)){ stop("Parameters 'filters' and 'values' must be vectors of the same length.")}
    if(!prod(filters %in% listAttributes(mart))){stop("The filters do not exist. Please check listAttributes() function.")}
    
    
    query <- paste(filters[1]," = \"",values[1],"\"", sep="") 
    if(length(filters) > 1){
      query<-c(query, apply(cbind(filters,values),1,function(x){
        result<-paste(" ",operator," ", x[1], " =\"", x[2],"\"",sep="")
        return(result)
      }))
      query<-paste(query,collapse=" ")
    }
    query<-paste("SELECT ", paste(attributes, collapse=" , ")," FROM ", mart, " WHERE ", query, sep="") #Construct query
    
  }
  
  #Connect to database
  library(RSQLite)
  db<-dbConnect(SQLite(), dbname="/Users/emimemime/Desktop/funcionesEmi/regulondb_92_sqlite3.db")
  
  #Retrieve data
  result <-dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  #Check if results are empty 
  if(!nrow(result)){
    print("Your query produced no results. Try changing the filters, values or attributes to be retrieved.")
  }
  return(result)
}