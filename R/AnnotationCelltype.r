#
#
#

library(Seurat)

AnnotationCelltype <- function(scRNA, model="claude-3-5-sonnet-20240620", tissuename="unknow", api_key="", base_url=""){
  if(!class(scRNA)[1] != "Seurat"){
    print("The input object is not of type Seurat")
    return(NULL)
  }
  scRNA.markers <- FindAllMarkers(scRNA, only.pos = TRUE)
  scRNA.markers %>%
    group_by(cluster) %>%
    dplyr::filter(avg_log2FC > 1) %>%
    slice_head(n = 10) %>%
    ungroup() -> top10
  celltyps        <- GetCellType(markergenes=SeuratMarkerGeneToStr(top10), model=model, tissuename=tissuename, api_key="", base_url="")
  new.cluster.ids <- unname(unlist(celltype$content))
  if(length(levels(top10$cluster)) == length(new.cluster.ids)){
    names(new.cluster.ids) <- levels(scRNA)
    scRNA <- RenameIdents(scRNA, new.cluster.ids)
  }else{
     celltyps        <- GetCellType(markergenes=SeuratMarkerGeneToStr(top10), model=model, tissuename=tissuename, api_key="", base_url="")
     new.cluster.ids <- unname(unlist(celltype$content))
     if(length(levels(top10$cluster)) == length(new.cluster.ids)){
        names(new.cluster.ids) <- levels(scRNA)
        scRNA <- RenameIdents(scRNA, new.cluster.ids)
     }else{
      cat("The returned data length is abnormal, please try again.\n")
     }
  }
  return(scRNA)
}