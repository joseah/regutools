#' @title Call RSAT.
#' @description This function access RSAT via SOAP.
#' @author José Alquicira Hernández
#' @param method Name of the method to be used from RSAT
#' @param parameters List of parameters provided to method
#' @return an R object with results retrieved from RSAT
#' @examples
#'
#' # Get 50 random protein sequences of length 100 in fasta format. Use number 300 as seed.
#'
#' RSAT(method = "random_seq",
#'     parameters = list(sequence_length = 100,
#'                       repetition = 50,
#'                       type = "protein",
#'                       seed = 300,
#'                       format = "fasta"))
#' @export

RSAT <- function(method, parameters = NULL){

  if(!is.null(parameters)){
    request <- BuildXml(method = method,
                      parameters = parameters)
  }else{
    request <- BuildXml(method = method)
  }

  # Communicates to RSAT
  res <- POST("http://embnet.ccg.unam.mx/rsa-tools//web_services/RSATWS.cgi",
       body = request,
       content_type("text/xml; charset=utf-8"))
  stop_for_status(res)
  res <- content(res)

  # Extracts result from XML response
  res.format <- xmlToList(xmlParse(res))
  res.text <- res.format$Body[[1]]$response$client$text

  return(res.text)

}

# RSAT(method = "supported_organisms",
#      parameters = list(return = "ID,name",
#                        source = "NCBI"))
