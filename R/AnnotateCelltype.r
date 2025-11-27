#' Cell type annotation
#' @param scRNA      : Seurat object
#' @param model      : The model name you want to use
#' @param tissuename : Cell derived tissue
#' @param api_key    : Key for API interface
#' @param base_url   : API interface address
#' @return           : Annotate the cell types of the Seurat object.
#' @export
#' @name AnnotateCelltype

AnnotateCelltype <- function(scRNA, model="claude-3-5-sonnet-20240620", tissuename="unknow", api_key="", base_url=""){
  library(Seurat)
  if(class(scRNA)[1] != "Seurat"){
    print("The input object is not of type Seurat")
    return(scRNA)
  }
  scRNA.markers <- FindAllMarkers(scRNA, only.pos = TRUE)
  scRNA.markers %>%
    group_by(cluster) %>%
    dplyr::filter(avg_log2FC > 1) %>%
    slice_head(n = 10) %>%
    ungroup() -> top10
  celltypes       <- AICellType::GetCellType(markergenes=SeuratMarkerGeneToStr(top10), model=model, tissuename=tissuename, api_key="", base_url="")
  new.cluster.ids <- sapply(celltypes$content, `[[`, 1)
  basis           <- sapply(celltypes$content, `[[`, 2)
  if(length(levels(top10$cluster)) == length(new.cluster.ids)){
    print(data.frame(
      cluster  = 0:(length(new.cluster.ids)-1),
      celltype = new.cluster.ids,
      basis    = basis
      ))
    names(new.cluster.ids) <- levels(scRNA)
    scRNA <- RenameIdents(scRNA, new.cluster.ids)
  }else{
     celltypes        <- GetCellType(markergenes=SeuratMarkerGeneToStr(top10), model=model, tissuename=tissuename, api_key="", base_url="")
     new.cluster.ids <- sapply(celltypes$content, `[[`, 1)
     basis           <- sapply(celltypes$content, `[[`, 2)
     if(length(levels(top10$cluster)) == length(new.cluster.ids)){
        print(data.frame(
        cluster  = 0:(length(new.cluster.ids)-1),
        celltype = new.cluster.ids,
        basis    = basis
        ))
        names(new.cluster.ids) <- levels(scRNA)
        scRNA <- RenameIdents(scRNA, new.cluster.ids)
     }else{
      cat("The returned data length is abnormal, please try again.\n")
     }
  }
  return(scRNA)
}