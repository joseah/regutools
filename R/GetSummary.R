#' GetSummary
#'
#' Return summary of regulation.
#' @param regulation The input of this function is de output of GetGeneRegulation()
#' @keywords regulation retrieval, summary, networks,
#' @export
#' @author
#' Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca, José Alquicira Hernández
#' @examples
#' # Retrieve summary of  araC
#'
#' araC_regulation<- GetGeneRegulation(genes = c("araC"),
#' format = "multirow",
#' output.type = "TF")
#'
#' GetSummary(regulation=araC_regulation)


GetSummary<-function(regulation){

  # Check if data base exists
  if(system.file("extdata", "regulondb_sqlite3.db",
                 package = "regutools")==""){
    stop("Please download the database using the GetDatabase() function.",call.=FALSE)
  }

  #Regulation is a gene list
  if(class(regulation)%in%c("character","vector")){
    regulation <- GetGeneRegulation(genes=regulation)
  }

  #Checks if user changed attribute format in output data.frame from GetGeneRegulation.
  if( ! attributes(regulation)$format %in% c("multirow","onerow","table")){
    stop("The input data.frame attribute 'format' must be multirow, onerow, or table.",call.=FALSE)
  }

  #If regulation is a data.frame, it must be "multirow" format
  if(attributes(regulation)$format == "onerow"){
    stop("The input data.frame attribute 'format' must be multirow.",call.=FALSE)
  }else if (attributes(regulation)$format == "table"){
    stop("The input data.frame attribute 'format' must be multirow.",call.=FALSE)

  }

  TF_counts<-data.frame(table(regulation$regulators), stringsAsFactors=FALSE)

  summary<- apply(TF_counts, 1, function(x){

    #Get rows for a specific TF
    TF_data <-regulation[regulation[,"regulators"]==x[1],]

    #Count regulated effects
    effect <- table(TF_data$effect)
    #Adds regulation +, - or +/- in case their frequency is 0.
    if(! "+"%in%names(effect)){
      effect <-c(effect, "+"=0)
    }
    if(! "-"%in%names(effect)){
      effect <-c(effect, "-"=0)
    }
    if(! "+/-"%in%names(effect)){
      effect <-c(effect, "+/-"=0)
    }

    #Concatenates row to include the summary information for each TF
    summary_row<-c(TF=x[1], #TF name
                   regulated_number=x[2],#Number of genes regulated by TF
                   regulated_percentage= (as.numeric(x[2])/length(regulation$genes))*100,#Percentage of genes in query regulated by TF
                   activator= effect["+"],#Frequency of activation interactions
                   repressor= effect["-"],#Frequency of repression interactions
                   dual=effect["+/-"] ,#Frequency of dual interactions
                   regulated= paste(TF_data$genes, collapse=", ")#List of genes regulated by TF
    )

    return(summary_row)

  })

  summary <- data.frame(t(summary)) #Format summary as a data.frame
  colnames(summary) <- c("TF", "Regulated_Number", "Regulated_Percentage","+","-","+/-","regulated")

  return(summary)
}



