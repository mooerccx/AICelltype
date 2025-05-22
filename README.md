# AICelltype

## **Install**

```R
library(devtools)
devtools::install_github("mooerccx/AICelltype")
```

## **Example**

```shell
wget https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz
tar zvxf pbmc3k_filtered_gene_bc_matrices.tar.gz
```

```R
library(dplyr)
library(Seurat)
library(patchwork)
library(AICelltype)

# Load the PBMC dataset
pbmc.data <- Read10X(data.dir = "/brahms/mollag/practice/filtered_gene_bc_matrices/hg19/")
# Initialize the Seurat object with the raw (non-normalized data).
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc
## An object of class Seurat 
## 13714 features across 2700 samples within 1 assay 
## Active assay: RNA (13714 features, 0 variable features)
##  1 layer present: counts

# The [[ operator can add columns to object metadata. This is a great place to stash QC stats
pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")
pbmc <- subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
pbmc <- NormalizeData(pbmc)
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)
all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))
pbmc <- FindNeighbors(pbmc, dims = 1:10)
pbmc <- FindClusters(pbmc, resolution = 0.5)

#
# Example one directly passes a Seurat object.
# If you have your own API, please fill in the corresponding parameters; leave them blank if not.
pbmc <- AnnotateCelltype(scRNA=pmbc, tissuename="PBMC")


#
# Example two first obtains the top 10 marker genes and then performs annotation operations.
# If you have your own API, please fill in the corresponding parameters; leave them blank if not.
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE)
pbmc.markers %>%
    group_by(cluster) %>%
    dplyr::filter(avg_log2FC > 1) %>%
    slice_head(n = 10) %>%
    ungroup() -> top10
MarkerGenes     <- SeuratMarkerGeneToStr(top10)
celltype        <- GetCellType(markergenes=MarkerGenes, tissuename="PBMC")
new.cluster.ids <- unname(unlist(celltype$content))
names(new.cluster.ids) <- levels(pbmc)
pbmc <- RenameIdents(pbmc, new.cluster.ids)
```



## Notice

Although this function provides a custom model interface, this parameter is only valid when using your own API.

