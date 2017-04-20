#' @title Constructs a particular logical condition to query database to add google search
#' @description Given a list of filters, this function builds a logical condition to query database using intervals
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @param filters A list with values to restrict the query. The names of all elements in this list correspond to the attributes to bre retrieved.
#' @param interv the filters that are going to be in a intervale
#' @param operator A string indicading if all conditions -\code{filters}- (AND) or some of them (OR) should be met
#' @return A string with a sql logical condition to query the dataset
#' @examples
#' BuildCondition(filters = list(network_type = "TF-GENE",
#'                          regulator_partialmatch = "AraC"),
#'                           dataset = "NETWORK",
#'                           operator = "AND")
#' @export
ExistingPartialMatch<-function(filters, partialmatch, operator){
existing.partialmatch.index <- names(filters) %in% partialmatch
existing.partialmatch <-filters[existing.partialmatch.index]
condition.format.partialmatch <- mapply(paste0,filters[existing.partialmatch.index], "%'", SIMPLIFY = FALSE)
condition.format.partialmatch <- mapply(paste, names(condition.format.partialmatch), condition.format.partialmatch, sep = " like  '%" , SIMPLIFY = FALSE)
condition.format.partialmatch <- lapply(condition.format.partialmatch, function(x){
  paste0("(", paste(x, collapse = " AND "), ")")
})
condition.partialmatch <- paste(unlist(condition.format.partialmatch), collapse = paste0(" ", operator, " "))
return(condition.partialmatch)
}
