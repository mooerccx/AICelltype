### Introduction

This project is designed to reproduce the results presented in the AICelltype paper.
All analytical workflows have been packaged into a single streamlined script: ArticleReproduction.R.

With minimal parameter input, users can execute the entire analysis pipeline in one command—from data preprocessing and clustering to cell type annotation and visualization.

#### Environment Setup and Package Installation

```R
if (!require("devtools")) install.packages("devtools")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if(!require("optparse", quietly=TRUE)) install.packages("optparse")
library(optparse)
if(!require("rols", quietly=TRUE)) BiocManager::install("ropls")
library(rols)
if(!require("ggplot2", quietly=TRUE)) install.packages("ggplot2")
library(ggplot2)
if(!require("tidyverse", quietly=TRUE)) install.packages("tidyverse")
library(tidyverse)
if(!require("viridis", quietly=TRUE)) install.packages("viridis")
library(viridis)
if(!require("ggpubr", quietly=TRUE)) install.packages("ggpubr")
library(ggpubr)
if(!require("cowplot", quietly=TRUE)) install.packages("cowplot")
library(cowplot)
if(!require("colorspace", quietly=TRUE)) install.packages("colorspace")
library(colorspace)
if(!require("AICelltype", quietly=TRUE)) devtools::install_github("mooerccx/AICelltype")
library(AICelltype)
```






#### Example


```shell
Rscript script/ArticleReproduction.R -i data/dataset.csv \
				-o ${PWD}/result/ \
				-m data/broadtype_mapping_results.tsv \
				-u Your_Model_api_baseUrl \
				-k Your_Model_api_key
```



