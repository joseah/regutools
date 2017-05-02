#' @title Constructs a particular logical condition to query database
#' @description Given a list of filters, this function builds a logical condition to query database using intervals
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @param filters A list with values to restrict the query. The names of all elements in this list correspond to the attributes to bre retrieved.
#' @param interv the filters that are going to be in a intervale
#' @param operator A string indicading if all conditions -\code{filters}- (AND) or some of them (OR) should be met
#' @return A string with a sql logical condition to query the dataset
#' @examples
#' BuildCondition(filters = list(network_type = "TF-GENE",
#'                          regulator_name = "AraC"),
#'                           dataset = "NETWORK",
#'                           operator = "AND")
#' @export


ExistingIntervals<-function(filters, interv, operator,partialmatch) {

  existing.interv.index <-  names(filters) %in% interv
  existing.interv <-filters[existing.interv.index]

  #Check that interv filter's value is a pair
  #If it's more than two, then drop the remaining
  existing.interv <- lapply(existing.interv,function(x){
    if(length(x)>2){
      warning("If values of the interval filter are more than 2, only the first two are considered.",call.=FALSE)
      x[1:2]

    } else if (length(x)==1) {
      stop("When filtering by intervals, the values of the interval filter must be exactly 2. ",call.=FALSE)
    } else {
      x
    }
  })
  filters[existing.interv.index]<-existing.interv

  condition.format.interv <- mapply(paste0, filters[existing.interv.index], "", SIMPLIFY = FALSE)
  condition.format.interv<-lapply(condition.format.interv , function(y){
    paste(c(">=","<="),y)
    })
  condition.format.interv <- mapply(paste, names(condition.format.interv), condition.format.interv, sep = " " , SIMPLIFY = FALSE)
  condition.format.interv <- lapply(condition.format.interv, function(x){
    paste0("(", paste(x, collapse = " AND "), ")")
    })
  condition.interv <- paste(unlist(condition.format.interv), collapse = paste0(" ", operator, " "))
  if (!is.null(partialmatch)){
   condition.partialmatch<-ExistingPartialMatch(filters, partialmatch, operator)
   condition.pmandin<-paste(condition.partialmatch, condition.interv, sep = operator, collapse = operator)
   return(condition.pmandin)
  }
  return(condition.interv)
}
