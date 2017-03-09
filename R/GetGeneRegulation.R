#' Get regulation 
#'
#' Given a list of genes (names, bnumbers or GIs), get all transcription factors that regulate them.
#' @param genes List of genes or GIs. 
#' @param format   Output format of data: 
#' @param output.type Should regulators be represented as TFs of genes?
#' @keywords regulation retrieval, tf, networks,
#' @export
#' @author 
#' Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples 
#' # Retrieve regulation of araC
#' 
#' GetGeneRegulation(genes = c("araC"), 
#' format = "multirow", 
#' output.type = "TF")


GetGeneRegulation<-function(genes,format="multirow",output.type="TF"){
  #Convert GIs to gene names
  equivalence_table<- getAttr(attributes=c("id","name"),dataset="GENE")
  genes<-lapply(as.list(genes),function(gene){
    if(grepl("ECK12",gene)){
      return(equivalence_table[equivalence_table[,"id"]==gene,"name"])
    } else {
      return(gene)
    }
  })
  
  
  #Retrieve data from NETWORK table
  regulation <- getAttr(attributes=c("regulated_name","regulator_name","effect"), 
                        filters=c("regulated_name"), #this solution is temporary, should be replaced with new version of getAttr that handles values as lists
                        values=genes,
                        dataset="NETWORK")
  colnames(regulation)<-c("genes","regulators","effect")
  
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
