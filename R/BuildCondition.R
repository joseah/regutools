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

#Agregar que solo sea 2, o que ignore lo que le sigue. Se puede ver lengh
#Poner el like por que en operon que tiene varios nombre de genes entre los dos %% va a servir

BuildCondition <- function(filters, dataset, operator, interv, partialmatch){
  if(class(filters) == "list"){
    if(!all(names(filters) %in% ListAttributes(dataset)[["column_name"]])){
      non.existing.attrs.index <- names(filters) %in% ListAttributes(dataset)[["column_name"]]
      non.existing.attrs <- names(filters)[!non.existing.attrs.index]
      stop("Provided filter(s) in the list ", non.existing.attrs ,
           " do not exist. Please check ListAttributes() function.", call.= FALSE)
    }
     if (!is.null(interv)){
      if (!all(interv%in%names(filters))){
        non.existing.interv.index <- !(interv %in% names(filters))
        non.existing.interv <-(interv[non.existing.interv.index])
        stop("Provided filter(s) to use  as intervales", non.existing.interv ,
             " do not exist in the filters you provide", call.= FALSE)
      }
     condition_intervals<-ExistingIntervals(filters, interv, operator, partialmatch)
     if((length(filters)==length(interv))){
         return(condition_intervals)
         }else{
          conditions_nonintervals<-NonExistingIntervals(filters, interv, operator, partialmatch)
          conditionall<-paste(condition_intervals, conditions_nonintervals, sep = " AND ")
          return( conditionall)
         }#Se cierra el que no sean iguales
         }else { #Se cierra que no sean nulos osea que si son
          conditions_nonintervals<-NonExistingIntervals(filters, interv, operator, partialmatch)
          return(conditions_nonintervals)
          }
    }
    stop("Provided filters are no not in a list format", call.= FALSE)
  }

