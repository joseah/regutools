#' GetNetwork
#'
#' Return complete regulatory network.
#' @param type TF-GENE, TF-TF, GENE-GENE
#' @keywords regulation retrieval, tf, networks,
#' @export
#' @author
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' # Retrieve regulation of araC
#'
#' GetNetwork(type="TF-GENE")
#'
#'
#'

GetNetwork<-function(type="TF-GENE"){
  #Get Network
  network<-GetAttr(attributes=c("regulator_name","regulated_name","effect"),
                   filters=list("network_type"=c(type)),
                   dataset="NETWORK")
  colnames(network)<-c("regulator","gene","effect")

  #Change effect to +, - and +/-
  network$effect<-sub(pattern="activator",replacement="+",x=network$effect)
  network$effect<-sub(pattern="repressor",replacement="-",x=network$effect)
  network$effect<-sub(pattern="dual",replacement="+/-",x=network$effect)

  return(network)
}


