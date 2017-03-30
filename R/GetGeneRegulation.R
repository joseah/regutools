#' Get regulation
#'
#' Given a list of genes (names, bnumbers or GIs), get all transcription factors that regulate them.
#' @param genes List of genes or GIs.
#' @param format   Output format of data: multirow, onerow, table
#' @param output.type Should regulators be represented as TF of GENE?
#' @keywords regulation retrieval, tf, networks,
#' @export
#' @author
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' # Retrieve regulation of araC
#'
#' GetGeneRegulation(genes = c("araC"),
#' format = "multirow",
#' )


GetGeneRegulation<-function(genes,format="multirow",output.type="TF"){
  #Check genes parameter class
  if(! class(genes) %in% c("vector","list","character")){
    stop("Parameter 'genes' must be a character vector or list.",call.=FALSE)
  }
  #Check format parameter
  if(! format %in% c("multirow","onerow", "table")){
    stop("Parameter 'format' must be multirow, onerow, or table.",call.=FALSE)
  }
  #Check output.type
  if(! output.type %in% c("TF","GENE")){
    stop("Parameter 'output.type' must be either 'TF' or 'GENE'",call.=FALSE)
  }

  #Convert GIs to gene names
  equivalence_table<- GetAttr(attributes=c("id","name"),dataset="GENE")
  genes<-lapply(as.list(genes),function(gene){
    if(grepl("ECK12",gene)){
      return(equivalence_table[equivalence_table[,"id"]==gene,"name"])
    } else {
      return(gene)
    }
  })

  if (output.type == "TF"){
    network.type <- "TF-GENE"
  } else if (output.type == "GENE"){
    network.type <- "GENE-GENE"
  }

  #Retrieve data from NETWORK table
  regulation <- GetAttr(attributes=c("regulated_name","regulator_name","effect"),
                        filters=list("regulated_name"=genes, "network_type"=network.type),
                        dataset="NETWORK")
  colnames(regulation)<-c("genes","regulators","effect")

  #Change effect to +, - and +/-
  regulation$effect<-sub(pattern="activator",replacement="+",x=regulation$effect)
  regulation$effect<-sub(pattern="repressor",replacement="-",x=regulation$effect)
  regulation$effect<-sub(pattern="dual",replacement="+/-",x=regulation$effect)

  #Format output
  #Multirow
  if(format=="multirow"){
    return(regulation)

  #Onerow
  } else if (format=="onerow"){
    regulation<-lapply(as.list(genes), function(x){
      genereg<-regulation[regulation[,"genes"]==x,]
      genereg<- paste(paste(genereg$regulators, genereg$effect,sep="(", collapse="), "),")",sep="")
    })
    regulation<-unlist(regulation)
    genes<-unlist(genes)
    regulation<-data.frame(genes,regulation)
    colnames(regulation)<-c("genes","regulators")

    return(regulation)

  #Table
  } else if (format=="table"){
    #Empty dataframe
    rtable<-data.frame(matrix(nrow=length(genes),ncol=length(c(unique(regulation$regulators)))))
    colnames(rtable)<-unique(regulation$regulators)
    rownames(rtable)<-genes

    #Fill dataframe with regulation
    for(i in 1:dim(regulation)[1]){
      rtable[regulation[i,1],regulation[i,2]]<-regulation[i,3]
    }
    regulation<-rtable
    return(regulation)

  }

}

