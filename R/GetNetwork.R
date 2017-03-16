#' GetNetwork
#'
#' Return complete regulatory network.
#' @param type tf-gene, tf-tf, gene-gene
#' @keywords regulation retrieval, tf, networks,
#' @export
#' @author
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' # Retrieve regulation of araC
#'
#' GetGeneRegulation(genes = c("araC"),
#' format = "multirow",
#' output.type = "TF")
GetNetwork<-function(type="TF-GENE"){
  #Get Network
  network<-GetAttr(attributes=c("regulator_name","regulated_name","effect"),
                   filters=list("network_type"=c(type)),
                   dataset="NETWORK")
  colnames(network)<-c("regulator","gene","effect")

  return(network)
}


