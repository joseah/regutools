#' @title Constructs logical condition to query database
#' @description Given a list of filters, this function builds a logical condition to query database
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @param filters A list with values to restrict the query. The names of all elements in this list correspond to the attributes to bre retrieved.
#' @param dataset A string containing the name of the dataset of interest
#' @param operator A string indicading if all condition -\code{filters}- (AND) or some of them (OR) should be met
#' @return A string with a sql logical condition to query the dataset
#' @examples
#' BuildCondition(filters = list(network_type = "TF-GENE",
#'                          regulator_name = "AraC"),
#'                           dataset = "NETWORK",
#'                           operator = "AND")
#' @export


BuildCondition<-function(filters, dataset, operator){
if(class(filters) == "list"){
  #solo que si no corre
  if(!all(names(filters) %in% ListAttributes(dataset)[["column_name"]])){
    At_NotEx<-(names(filters) %in% ListAttributes(dataset)[["column_name"]])
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
