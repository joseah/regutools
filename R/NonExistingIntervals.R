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


NonExistingIntervals<-function(filters, interv, operator, partialmatch){
  if (!(length(partialmatch)==length(filters))){
  non.interv.index<-!((names(filters) %in% interv)|(names(filters) %in% partialmatch))
  non.interv <-filters[non.interv.index]
  condition.format.non.interv <- mapply(paste0, filters[non.interv.index], "'", SIMPLIFY = FALSE)
  condition.format.non.interv <- mapply(paste, names(condition.format.non.interv), condition.format.non.interv, sep = " = '" , SIMPLIFY = FALSE)
  condition.format.non.interv <- lapply(condition.format.non.interv, function(x){
    paste0("(", paste(x, collapse = " OR "), ")") })
  condition.non.interv <- paste(unlist(condition.format.non.interv), collapse = paste0(" ", operator, " "))
  }
  if (!is.null(partialmatch)){
    condition.partialmatch<-ExistingPartialMatch(filters, partialmatch, operator)
    if ((length(partialmatch)==length(filters))){
      return( condition.partialmatch)
    }
    condition.pmandnoin<-paste(condition.partialmatch, condition.non.interv, sep = operator, collapse = operator)
    return(condition.pmandnoin)
  }
  return(condition.non.interv)
  }
