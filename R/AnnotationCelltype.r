
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
  new.cluster.ids <- GetCellType(markergenes=SeuratMarkerGeneToStr(top10), model=model, tissuename=tissuename, api_key="", base_url="")
  names(new.cluster.ids) <- levels(scRNA)
  scRNA <- RenameIdents(scRNA, new.cluster.ids)
  return(scRNA)
}