#
#
#

SeuratMarkerGeneToStr <- function(markergenes){
  markergenes <- as.data.frame(markergenes)
  message     <- c()
  for(cluster in unique(markergenes$cluster)){
    genes   <- markergenes[markergenes$cluster == cluster,]$gene
    strcode <- paste(genes, collapse = ", ")
    message <- c(message, strcode)
  }
  return(paste(message, collapse= "\n"))
}