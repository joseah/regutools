#' @title Constructs logical condition to query database
#' @description Given a list of filters, this function builds a logical condition to query database
#' @author
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @param filters A list with values to restrict the query. The names of all elements in this list correspond to the attributes to bre retrieved.
#' @param dataset A string containing the name of the dataset of interest
#' @param operator A string indicading if all conditions -\code{filters}- (AND) or some of them (OR) should be met
#' @return A string with a sql logical condition to query the dataset
#' @examples
#' BuildCondition(filters = list(network_type = "TF-GENE",
#'                          regulator_name = "AraC"),
#'                           dataset = "NETWORK",
#'                           operator = "AND")
#' @export


BuildCondition <- function(filters, dataset, operator){
  if(class(filters) == "list"){
    if(!all(names(filters) %in% ListAttributes(dataset)[["column_name"]])){
      non.existing.attrs.index <- names(filters) %in% ListAttributes(dataset)[["column_name"]]
      non.existing.attrs <- names(filters)[!non.existing.attrs.index]
      stop("Provided filter(s) in the list ", non.existing.attrs ,
           " do not exist. Please check ListAttributes() function.", call.= FALSE)
    }
    condition.format <- mapply(paste0, filters, "'", SIMPLIFY = FALSE)
    condition.format <- mapply(paste, names(condition.format), t, sep = " = '" , SIMPLIFY = FALSE)
    condition.format <- lapply(condition.format, function(x){
      paste0("(", paste(x, collapse = " OR "), ")")
    })
    condition <- paste(unlist(condition.format), collapse = paste0(" ", operator, " "))
    return(condition)
  }
}
