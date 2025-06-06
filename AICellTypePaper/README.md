### 介绍

该项目旨在复现文章AICelltype中的分析结果，分析代码封装到统一的脚本`ArticleReproduction.R`中，只需要设计极少的参数即可一条命令实现完整的分析。

#### 依赖的包

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



实验数据

- `AICellTypePaper/data/all_data.csv` marker基因列表
- `AICellTypePaper/data/all_gene.csv` 人类基因列表
- `AICellTypePaper/data/broadtype_mapping_results.tsv`  细胞名称映射文件



运行参数

```shell
Rscript script/ArticleReproduction.R -i data/all_data.csv \
				-o ${PWD}/result/ \
				-m data/broadtype_mapping_results.tsv \
				-u Your_Model_api_baseUrl \
				-k Your_Model_api_key
```



