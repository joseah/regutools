#' @title Construct logical condition
#' @description

BuildCondition<-function(filters, dataset, operator){
if(class(filters) == "list"){
  #solo que si no corre
  if(!all(names(filters) %in% regutools::listAttributes(dataset))){
    At_NotEx<-(names(filters) %in% regutools::listAttributes(dataset))
    Names_AtNotEx<- names(filters)[which(!At_NotEx)]
    stop("Provided filter(s) in the list ", Names_AtNotEx , " do not exist. Please check listAttributes() function.", call.= FALSE)
  }
  t<-mapply(paste0, filters, "'", SIMPLIFY = FALSE)
  tm<-mapply(paste, names(t), t, sep=" = '" , SIMPLIFY = FALSE)
  tm3<-lapply(tm, function(x){
        paste0("(", paste(x, collapse = " or "),")")
        })
  query<-paste(unlist(tm3), collapse =paste0(" ",operator, " "))
  return(query)
}
}
