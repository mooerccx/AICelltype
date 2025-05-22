#' Convert the top 10 marker genes obtained from Seurat analysis into a string that meets the input requirements of the GetCellType function.
#' @param  markergenes : Top 10 marker gene list obtained from Seurat analysis.
#' @return The concatenated string.
#' @export
#' @name SeuratMarkerGeneToStr

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