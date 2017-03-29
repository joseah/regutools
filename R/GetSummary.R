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

  TF_counts<-data.frame(table(regulation$regulators), stringsAsFactors=FALSE)

  summary<- apply(TF_counts, 1, function(x){

    #Get rows for a specific TF
    TF_data <-regulation[regulation[,"regulators"]==x[1],]

    #Count regulated effects
    effect <- table(TF_data$effect)
    if(! "+"%in%names(effect)){
      effect <-c(effect, "+"=0)
    }
    if(! "-"%in%names(effect)){
      effect <-c(effect, "-"=0)
    }
    if(! "+/-"%in%names(effect)){
      effect <-c(effect, "+/-"=0)
    }

    summary_row<-c(TF=x[1],
                   regulated_number=x[2],
                   regulated_percentage= (as.numeric(x[2])/length(regulation$genes))*100,
                   activator= effect["+"],
                   repressor= effect["-"],
                   dual=effect["+/-"] ,
                   regulated= paste(TF_data$genes, collapse=", ")
    )

    return(summary_row)

  })

  summary <- data.frame(t(summary))
  colnames(summary) <- c("TF", "Regulated_Number", "Regulated_Percentage","+","-","+/-","regulated")

  return(summary)
}



