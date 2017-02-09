#' @title Construct XML schema to commuticate to RSAT.
#' @description This function creates an XML schema to be used by `POST` function.
#' @author José Alquicira Hernández
#' @param method Name of the method to be used from RSAT
#' @param parameters List of parameters provided to method
#' @return an R object with results retrieved from RSAT
#' @examples
#' request <- BuildXml("supported_organisms",
#'                      parameters = list(return = "name,source",
#'                      source = "ensembl"))
#' @export

BuildXml <- function(method, parameters = NULL){

  if(!is.null(parameters)){
    parameters.format <- mapply(function(param, value){
      upper.tag <-  paste0("<", param, ">")
      lower.tag <- paste0('</', param, ">")
      paste0(upper.tag, value, lower.tag)

    }, names(parameters), parameters)

    parameters<- paste("\n",parameters.format, collapse = "\n")
  }


  xml.body <- paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:RSATWS">
                 <soapenv:Header/>
                 <soapenv:Body>
                 <urn:', method,'>
                 <request>',
                 parameters,
                 '\n                 </request>
                 </urn:', method, '>
                 </soapenv:Body>
                 </soapenv:Envelope>')

  return(xml.body)
}
