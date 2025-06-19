# AICelltype: An Open, Efficient, and Customizable Tool for Cell Type Annotation Powered by LLMs


## 📌 Overview

AICelltype is an open, intelligent, and efficient cell type annotation framework powered by Large Language Models (LLMs). With the explosive growth of single-cell RNA sequencing (scRNA-seq) data, accurate and scalable cell type annotation has become a pressing challenge. Existing tools often suffer from limited generalization, heavy reliance on human expertise, high computational costs, and a lack of flexibility across tissues and species.

To address this, we systematically evaluated 79 state-of-the-art LLMs under different conditions (temperature, noise, and prompt formats), and developed an optimized annotation framework that:
<details>
<summary>📊 <b>How it works ? (click to expand)</b> </summary>

<div align="center">
  <img width="800" alt="abs" src="https://github.com/user-attachments/assets/d03bf059-cb1f-437e-8eef-f36b31c5869f" />


</div>

(A) Cell type identification leveraging large language models based on marker gene information.
(B) Evaluation of annotation accuracy and robustness across different language models, temperature settings, and noise conditions; an optimized model was selected using a cell-type matching scoring system.
(C) Integration of AIcelltype with standard Seurat analysis pipelines, enabling users to perform online cell type annotation and visualization through an open platform and OpenRouter interface. The platform supports flexible applications across multiple species and tissue types.
</details>

<details>
  <summary>📊 <b>Benchmark of accuracy for cell type annotation in large language models</b> </summary>

### Benchmark of accuracy for cell type annotation in large language models
![image](https://github.com/user-attachments/assets/0263b835-8f05-43ac-af39-56c967fe3158)

### The relationship between price, speed, and accuracy
![image](https://github.com/user-attachments/assets/7bcb2a6f-a92e-42d6-8d53-0394eee91fb0)
</details>




## 🚀 Key Features

✅ Free online annotation service: No registration or API keys required.

🧠 Leverages both open-source and commercial LLMs, avoiding black-box APIs if needed.

🔁 Supports Seurat-native workflows for easy integration into existing pipelines.

🌍 Enables cross-species and multi-tissue annotation with customizable prompts and scoring logic.

🌐 Web access: Try it now at 👉 https://aicelltype.jinlab.online

💸 Provides a cost-effective and fully open platform through OpenRouter and GitHub distribution.

⚙️ Self-hosting with customizable base URLs: Use your own LLM backend (e.g., local server, proxy API) via the baseurl parameter for full control and data privacy.

Whether you're working with human PBMC, mouse brain, or other complex tissue types, AICelltype offers a robust, extensible solution to empower your single-cell analysis with AI-enhanced annotation.




## **How to Install**

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
library(AICellType)

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
pbmc <- AnnotateCelltype(scRNA=pbmc, tissuename="PBMC")


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
pbmc <- RunUMAP
```

## ⚙️ Advanced Usage
### 🔄 Use Custom LLMs

By default, Claude 3.5 Sonnet (0624) is used for free. To use your own LLM:
```R
pbmc <- AnnotateCelltype(
  scRNA      = pbmc,
  tissuename = "PBMC",
  baseurl    = "https://openrouter.ai/api/v1/chat/completions",
  model      = "openai/gpt-4",
  key        ="your-key"
)'
```
baseurl: Custom API endpoint (e.g. OpenRouter, Ollama, local LLM server)

model: Any supported LLM model name (e.g. meta-llama/llama-3-70b-instruct)

### 🧬 Provide Context in tissuename

You can pass additional biological context to the LLM by customizing the tissuename:
```R
pbmc <- AnnotateCelltype(
  scRNA      = pbmc,
  tissuename = "PBMC，Isolated from dog infected with the virus"
)
```
The more specific the context, the better the model can match relevant cell types.


