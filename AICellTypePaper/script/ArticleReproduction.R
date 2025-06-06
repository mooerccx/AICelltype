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
# ------------------------------------------------------------------------------------------------------------------------------------------- #
opts <- list(
          make_option(c("-i", "--input")   , type="character", help="Input marker gene file" , default=""),
          make_option(c("-o", "--outdir")  , type="character", help="Output file path"       , default=""),
          make_option(c("-m", "--mapfile") , type="character", help="Cell name mapping file" , default=""),
          make_option(c("-u", "--url")     , type="character", help="Model api baseUrl"      , default=""),
          make_option(c("-k", "--key")     , type="character", help="Model api key"          , default="")
        )
opts_parser <- OptionParser(option_list=opts)
args        <- parse_args(opts_parser)
input       <- args$input
outdir      <- args$outdir
mapfile     <- args$mapfile
app.url     <- args$url
app.key     <- args$key
if(! file.exists(input | input == "")){
  stop("The input file does not exist, please check and try again.")
  
}
if(outdir == ""){
  print("If the output directory is not set, the results will be directly output to the current path.")
  outdir <- getwd()
}
# ------------------------------------------------------------------------------------------------------------------------------------------- #
models = c(
      "claude-3-opus-20240229",
      "claude-3-haiku-20240307",
      "claude-3-5-haiku-20241022",
      "claude-3-5-sonnet-20241022",
      "claude-3-5-sonnet-20240620",
      "gpt-3.5-turbo",
      "gpt-3.5-turbo-0125",
      "gpt-3.5-turbo-1106",
      "gpt-3.5-turbo-0613",
      "o1-mini",
      "o3",
      "o3-mini",
      "o4-mini",
      "gpt-4o",
      "gpt-4o-mini",
      "gpt-4o-mini-search-preview",
      "gpt-4o-2024-11-20",
      "gpt-4",
      "gpt-4-0613",
      "gpt-4-turbo",
      "gpt-4-0125-preview",
      "gpt-4-1106-preview",
      "gpt-4.1",
      "gpt-4.1-mini",
      "gpt-4.1-nano",
      "qwen-max-0125",
      "qwen-qwq-32b",
      "qwen2.5-3b-instruct",
      "qwen2.5-7b-instruct",
      "qwen2.5-14b-instruct",
      "qwen2.5-coder-1.5b-instruct",
      "Qwen/Qwen2.5-72B-Instruct",
      "Qwen/Qwen2.5-32B-Instruct",
      "Qwen/QVQ-72B-Preview",
      "llama-3.2-1b-preview",
      "llama-3.2-3b-preview",
      "llama-3.2-11b-vision-preview",
      "llama-3.2-90b-vision-preview",
      "llama3-8b-8192",
      "llama3-70b-8192",
      "Doubao-1.5-lite-32k",
      "Doubao-1.5-pro-32k",
      "Doubao-1.5-pro-256k",
      "Doubao-pro-4k",
      "Doubao-pro-32k",
      "Doubao-pro-256k",
      "Doubao-lite-4k",
      "Doubao-lite-32k",
      "Doubao-lite-128k",
      "aihubmix-DeepSeek-R1",
      "aihubmix-Mistral-Large-2411",
      "aihubmix-Llama-3-3-70B-Instruct",
      "aihubmix-Llama-3-1-8B-Instruct",
      "aihubmix-Llama-3-1-70B-Instruct",
      "aihubmix-Llama-3-1-405B-Instruct",
      "chutesai/Llama-4-Maverick-17B-128E-Instruct-FP8",
      "chutesai/Llama-4-Scout-17B-16E-Instruct",
      "chutesai/Mistral-Small-3.1-24B-Instruct-2503",
      "DeepSeek-V3",
      "DeepSeek-R1",
      "aihub-Phi-4",
      "kimi-latest",
      "codestral-latest",
      "grok-3-beta",
      "grok-3-mini-beta",
      "grok-3-mini-fast-beta",
      "deepseek-r1-distill-llama-70b",
      "deepseek-ai/DeepSeek-R1-Distill-Qwen-1.5B",
      "deepseek-ai/DeepSeek-R1-Distill-Qwen-7B",
      "deepseek-ai/DeepSeek-R1-Distill-Llama-8B",
      "deepseek-ai/DeepSeek-R1-Distill-Qwen-14B",
      "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B",
      "gemini-2.0-flash-lite-preview-02-05",
      "gemini-2.0-flash-thinking-exp-01-21",
      "gemini-2.5-pro-exp-03-25",
      "gemini-2.5-flash-preview-04-17",
      "gemini-2.5-flash-preview-04-17-nothink",
      "Qwen/Qwen3-235B-A22B",
      "Qwen/Qwen3-30B-A3B",
      "Qwen/Qwen3-32B",
      "Qwen/Qwen3-14B",
      "Qwen/Qwen3-8B"
  )
df               <- read.csv(file=input, header=TRUE)
log_file         <- file("timing_log1.txt", "w")
total_start_time <- Sys.time()
total_datasets   <- length(unique(df$dataset))
current_dataset  <- 0
print("Data reading completed.")
header           <- colnames(df)
for(model in models){
  for(suffix in c("_anno", "_CLname", "_CLID", "_broadtype", '_prompt_tokens', '_completion_tokens', "_time")){
    col.name <- paste0(model, suffix)
    if(!col.name %in% header){
      df[col.name] <- ''
    }
  }
}
for(dataset.name in unique(df$dataset)){
  current_dataset <- current_dataset + 1
  print(paste("Processing the dataset [", current_dataset, "/", total_datasets, "]:", dataset.name))
  dataset_start_time <- Sys.time()
  dataset.source <- subset(df, dataset==dataset.name)
  total_tissues  <- length(unique(dataset.source$tissue))
  current_tissue <- 0
  
  for(tissue.name in unique(dataset.source$tissue)){
    current_tissue <- current_tissue + 1
    print(paste("  Processing the organization [", current_tissue, "/", total_tissues, "]:", tissue.name))
    tissue_start_time <- Sys.time()
    
    dataset.source.tissue <- subset(dataset.source, tissue==tissue.name)
    start  <- 1
    end    <- length(dataset.source.tissue$dataset)
    sep    <- 20
    status <- TRUE
    total_batches <- ceiling(length(dataset.source.tissue$dataset) / sep)
    current_batch <- 0
    
    while(status){
      current_batch <- current_batch + 1
      batch_start_time <- Sys.time()
      ed = start + sep - 1
      if(ed >= end){
        ed     = end
        status = FALSE
      }
      print(paste("    Processing the batch [", current_batch, "/", total_batches, "] from", start, "to", ed))
      markergenes <- paste(dataset.source.tissue[start:ed,]$marker, collapse= "\n")
      # Add progress information in the model loop.
      total_models <- length(models)
      current_model <- 0
      for(model in models){
        current_model <- current_model + 1
        print(paste("      Process the model [", current_model, "/", total_models, "]:", model))
        model_start_time <- Sys.time()
        print(model)
        num = 3
        while(num > 0){
          try({
            celltype <- GetCellType(markergenes=markergenes, model=model, tissuename=tissue.name)
            if((class(celltype) != class('')) & (length(celltype$content) == length(dataset.source.tissue[start:ed,][,paste0(model, "_CLname")]))){
              dataset.source.tissue[start:ed, paste0(model, "_anno")]              <- unname(unlist(celltype$content))
              dataset.source.tissue[start:ed, paste0(model, '_prompt_tokens')]     <- round(celltype$tokens$prompt_tokens / length(celltype$content))
              dataset.source.tissue[start:ed, paste0(model, '_completion_tokens')] <- round(celltype$tokens$completion_tokens / length(celltype$content))
              # Calculate and record the processing time.
              process_time <- as.numeric(difftime(Sys.time(), model_start_time, units="secs"))
              dataset.source.tissue[start:ed, paste0(model, "_time")] <- process_time
              # Update the main data frame.
              idx <- which((df$dataset == dataset.name) & (df$tissue == tissue.name))[start:ed]
              if(length(idx) > 0){
                df[idx, paste0(model, "_anno")             ] <- unname(unlist(celltype$content))
                df[idx, paste0(model, "_prompt_tokens")    ] <- round(celltype$tokens$prompt_tokens / length(celltype$content))
                df[idx, paste0(model, "_completion_tokens")] <- round(celltype$tokens$completion_tokens / length(celltype$content))
                df[idx, paste0(model, "_time")             ] <- process_time
              }
              break
            }
          })
          num = num - 1
        }
        # Add model processing completion information.
        if(exists("process_time")) {
          print(paste("        Model processing completed, time taken :", round(process_time, 2), "seconds"))
        } else {
          print("        Model processing failed.")
        }
      }
      
      # Batch completion information
      batch_time <- difftime(Sys.time(), batch_start_time, units="secs")
      print(paste("    Batch processing completed, time taken:", round(as.numeric(batch_time), 2), "seconds"))
      write.table(df, paste0(outdir, "/AICelltype.anno.tab"), row.names = FALSE, sep="|")
      write.table(df, paste0(outdir, "/AICelltype.anno.tsv"), row.names = FALSE, sep="\t")
      start <- start + sep
    }
    tissue_time <- difftime(Sys.time(), tissue_start_time, units="secs")
    print(paste("  Batch processing completed:", tissue.name, ", time taken:", round(as.numeric(tissue_time), 2), "seconds"))
  }
  dataset_time <- difftime(Sys.time(), dataset_start_time, units="secs")
  print(paste("Complete the dataset processing.:", dataset.name, ", time taken:", round(as.numeric(dataset_time), 2), "seconds"))
}
total_time <- difftime(Sys.time(), total_start_time, units="secs")
print(paste("\nProcessing completed.!"))
print(paste("Total time used:", round(as.numeric(total_time), 2), "seconds"))

write.table(df, paste0(outdir, "/AICelltype.anno.tab"), row.names = FALSE, sep="|")
write.table(df, paste0(outdir, "/AICelltype.anno.tsv"), row.names = FALSE, sep="\t")
print("The result has been saved successfully.")


# ------------------------------------------------------------------------------------------------------------------------------------------- #
# Update the data without comments.
for(model in models){
  dataset <- df[(is.na(df[paste0(model, "_anno")])) | (df[paste0(model, "_anno")]==""),]
  if(nrow(dataset)==0) break;
  for(dataset.name in unique(dataset$dataset)){
    data.set <- subset(dataset, dataset==dataset.name)
    for(tissue.name in unique(data.set$tissue)){
      model_start_time <- Sys.time()
      data.tissue <- subset(data.set, tissue==tissue.name)
      markergenes <- paste(data.tissue$marker, collapse = "\n")
      num = 3
      while(num > 0){
        try({
          celltype <- GetCellType(markergenes=markergenes, model=model, tissuename=tissue.name)
          print(paste0("input----",  nrow(data.tissue)       , "----lines of data"))
          print(paste0("output####",  length(celltype$content), "####lines of data"))
          if((class(celltype) != class('')) & (length(celltype$content) == nrow(data.tissue))){
            # Calculate and record the processing time.
            print("Updating in progress----------------------------------------")
            process_time <- as.numeric(difftime(Sys.time(), model_start_time, units="secs"))
            df[df$id %in% data.tissue$id, paste0(model, "_anno")             ] <- unname(unlist(celltype$content))
            df[df$id %in% data.tissue$id, paste0(model, "_prompt_tokens")    ] <- round(celltype$tokens$prompt_tokens / length(celltype$content))
            df[df$id %in% data.tissue$id, paste0(model, "_completion_tokens")] <- round(celltype$tokens$completion_tokens / length(celltype$content))
            df[df$id %in% data.tissue$id, paste0(model, "_time")             ] <- process_time
            break
          }
        })
        num = num - 1
      }
    }
    
  }
  write.table(df, paste0(outdir, "/AICelltype.anno.update.tab"), row.names = FALSE, sep="|")
  write.table(df, paste0(outdir, "/AICelltype.anno.update.tsv"), row.names = FALSE, sep="\t")
}

# ------------------------------------------------------------------------------------------------------------------------------------------- #
# add CL_ID and CL_name
getOlsinfo <- function(celltype){
  info <- as(olsSearch(OlsSearch(q = celltype, ontology = "cl")), "data.frame")
  info <- info[grep('CL:',info$obo_id),]
  return(c(celltype, info[1,'label'], info[1,'obo_id']))
}


for(model in models){
  print(model)
  header  <- paste0(model, "_anno")
  tryCatch({
    for(id in df$id){
      cell.name <- df[df$id==id, paste0(model, "_anno")]
      CLID      <- df[df$id==id, paste0(model, "_CLID")]
      if((!is.na(CLID)) & (CLID!="") &(CLID!="NA")) next;
      if((!is.na(cell.name)) | (cell.name == "")){
        try(
          {
            cl        <- getOlsinfo(cell.name)  
            df[df$id==id, c(paste0(model, "_CLname"), paste0(model, "_CLID"))] <- cl[2:3]
          }
        )
      }
    }
  },
  error=function(e){
    print(model)
    print(e)
  })
  write.table(df, paste0(outdir, "/AICelltype.anno.update.cl.tab"), row.names = FALSE, sep="|")
  write.table(df, paste0(outdir, "/AICelltype.anno.update.cl.tsv"), row.names = FALSE, sep="\t")
}



# ------------------------------------------------------------------------------------------------------------------------------------------- #
# add broadtype map info
library(parallel)
library(pbapply)
if(mapfile == ""){
  clname_cols      <- grep("_anno$", colnames(df), value = TRUE)
  unique_celltypes <- unique(unlist(df[clname_cols]))
  unique_celltypes <- unique_celltypes[!is.na(unique_celltypes) & unique_celltypes != "" & unique_celltypes != "0"]
  unique_celltypes <- tolower(trimws(unique_celltypes))
  num_cores        <- detectCores() - 1
  cl               <- makeCluster(num_cores)
  batch_size       <- 20
  cell_batches     <- split(unique_celltypes, ceiling(seq_along(unique_celltypes)/batch_size))
  process_batch    <- function(batch) {
                            cells_str <- paste(batch, collapse = "\n")
                            results   <- GenerateComparisonTable(cells_str)
                            return(data.frame(
                              original_name   = batch,
                              broad_type      = results,
                              stringsAsFactors = FALSE
                            ))
                          }
  # Check the necessary variables.
  if (!exists("APP_API_KEY")) {
    stop("Please set the APP_API_KEY variable")
  }
  if (!exists("APP_BASE_URL")) {
    stop("Please set the APP_BASE_URL variable")
  }
  # Export the necessary variables and functions to the cluster.
  clusterExport(cl, c("GenerateComparisonTable", "process_batch", "APP_API_KEY", "APP_BASE_URL"))
  clusterEvalQ(cl, {
    library(httr2)
    library(jsonlite)
    library(dplyr)
    })
  # Use `pblapply` for parallel processing and display a progress bar.
  cat(sprintf("Processing %d cell types, divided into %d batches, using %d cores for parallel processing...\n", 
              length(unique_celltypes), length(cell_batches), num_cores))
  results    <- pblapply(cell_batches, process_batch, cl = cl)
  stopCluster(cl)
  df.map    <- do.call(rbind, results)

  write.table(df.map, paste0(outdir, "broadtype_mapping_results.tab"), sep="|", row.names = FALSE)
  write.table(df.map, paste0(outdir, "broadtype_mapping_results.tsv"), sep="\t", row.names = FALSE)

  cat(sprintf("Processing completed! The results have been saved to broadtype_mapping_results.csv\n"))
  cat(sprintf("Successful matches: %d, Unmatched: %d\n", 
              sum(!is.na(df.map$broad_type)), 
              sum(is.na(df.map$broad_type))))
}else{
  if(file.exists(mapfile)){
    df.map <- read.tab(mapfile, sep="\t", header=T)
  }else{
    stop(paste0("file ", mapfile, " does not exist, please verify and try again!"))
  }
}
print(head(df.map))

# ------------------------------------------------------------------------------------------------------------------------------------------- #

library(stringr)
load_mapping <- function(mapping_path) {
  mapping <- read.csv(mapping_path, stringsAsFactors = FALSE)
  mapping$original_name <- trimws(mapping$original_name)
  mapping$broad_type <- trimws(mapping$broad_type)
  cat("Data file has been loaded, containing", nrow(mapping), "mapping relationships\n")
  return(mapping)
}

load_data <- function(data_path) {
  data <- read.delim(data_path, stringsAsFactors = FALSE, sep="\t", quote="", check.names=FALSE)
  cat("Data file has been loaded, containing", nrow(data), "rows and ", ncol(data), "columns.\n")
  return(data)
}

get_model_prefixes <- function(all_cols, pattern="(_|\\.)[Aa]nno$") {
  anno_cols <- grep(pattern, all_cols, value = TRUE)
  if(length(anno_cols)==0){
    cat("No model column found.\n")
    return(character(0))
  }
  prefixes <- gsub(pattern, "", anno_cols)
  return(prefixes)
}

get_manual_info <- function(all_cols) {
  manual_cols <- grep("manual", all_cols, value = TRUE, ignore.case = TRUE)
  manual_anno <- if(length(manual_cols[grep("[Aa]nno", manual_cols)]) > 0)
  manual_cols[grep("[Aa]nno", manual_cols)][1] else NA
  manual_clid <- if(length(manual_cols[grep("[Cc][Ll][Ii][Dd]", manual_cols)]) > 0)
  manual_cols[grep("[Cc][Ll][Ii][Dd]", manual_cols)][1] else NA
  manual_broadtype <- if(length(manual_cols[grep("broadtype", manual_cols, ignore.case = TRUE)]) > 0)
  manual_cols[grep("broadtype", manual_cols, ignore.case = TRUE)][1] else NA
  if(is.na(manual_broadtype)) {
    manual_broadtype <- "manual_broadtype"
  }
  list(anno = manual_anno, clid = manual_clid, broadtype = manual_broadtype)
}

text_similarity_match <- function(text1, text2) {
              text1      <- tolower(trimws(as.character(text1)))
              text1      <- gsub("\\("        ,  "",   text1)
              text1      <- gsub("\\)"        ,  "",   text1)
              text1      <- gsub("^[ ]{1,10}" ,  "",   text1)
              text2      <- tolower(trimws(as.character(text2)))
              text2      <- gsub("\\("        ,  "",   text2)
              text2      <- gsub("\\)"        ,  "",   text2)
              text2      <- gsub("^[ ]{1,10}" ,  "",   text2)
              max_length <- 200 
              # ---------------------------------------------------------------------------------------------- #
              # Determine whether the string contains spaces. If there are spaces, the string needs to be split and compared first
              if(text1 %in% c("erythroid cell",	"erythrocytes") && text2 %in% c("erythroid cell",	"erythrocytes")) return(TRUE)
              if(text1 %in% c("hepatic stellate cells", "HSC")  && text2 %in% c("hepatic stellate cells", "hsc"))  return(TRUE)
              if(text1 %in% c("neuronal cell", "neural cell")   && text2 %in% c("neuronal cell", "neural cell"))   return(TRUE)
              if(text1 %in% c("erythrocyte", "erythroblasts")   && text2 %in% c("erythrocyte", "erythroblasts"))   return(TRUE)
              if(grepl(" ", text1)){
                tmp <- strsplit(text1, " ")[[1]]
                for(tmp.word in tmp){
                  if(length(text1) > 2 && length(tmp.word) < 2) next;
                  if(tmp.word %in% c("cell", "cells", "Cell", "Cells")) next;
                  if(grepl(tmp.word, text2)){
                    return(TRUE)
                  }
                }
              }else if (grepl(" ", text2)) {
                tmp <- strsplit(text2, " ")[[1]] 
                for(tmp.word in tmp){
                  if(length(text2) > 2 && length(tmp.word) < 2) next;
                  if(tmp.word %in% c("cell", "cells", "Cell", "Cells")) next;
                  if(grepl(tmp.word, text1)){
                    return(TRUE)
                  }
                }  
              }
              # ---------------------------------------------------------------------------------------------- #
              if(nchar(text1) > max_length) text1 <- substr(text1, 1, max_length)
              if(nchar(text2) > max_length) text2 <- substr(text2, 1, max_length)
              if(text1 == text2) return(TRUE)
              singular1  <- gsub("s$", "", text1)
              singular2  <- gsub("s$", "", text2)
              plural1    <- paste0(text1, "s")
              plural2    <- paste0(text2, "s")
              if(text1 == plural2 || plural1 == text2) return(TRUE)
              if(singular1 == singular2 && nchar(singular1) > 3) return(TRUE)
              if((nchar(text1) > 5 && nchar(text1) < 50) &&  (nchar(text2) > 5 && nchar(text2) < 50)) {
                if(grepl(pattern = fixed(text1), x = text2, ignore.case = TRUE) || grepl(pattern = fixed(text2), x = text1, ignore.case = TRUE)) return(TRUE)
              }
              if(grepl("b cell", text1) && grepl("b cell", text2)) return(TRUE)
              if(grepl("t cell", text1) && grepl("t cell", text2)) return(TRUE)
              if(grepl("megakaryocyte", text1) && grepl("megakaryocyte", text2)) return(TRUE)
              
              return(FALSE)
            }
process_model <-  function(data, mapping, prefix, all_cols, manual_info) {
  cat("\nProcessing the model.:", prefix, "\n")
  anno_col_underscore  <- paste0(prefix, "_anno")
  anno_col_dot         <- paste0(prefix, ".anno")
  score_col            <- paste0(prefix, "_score")
  if(paste0(prefix, "_broadtype") %in% all_cols){
    broadtype_col <- paste0(prefix, "_broadtype")
  } else if(paste0(prefix, ".broadtype") %in% all_cols){
    broadtype_col <- paste0(prefix, ".broadtype")
  } else {
    separator <- ifelse(any(grepl(paste0("^", prefix, "_"), all_cols)), "_", ".")
    broadtype_col <- paste0(prefix, separator, "broadtype")
    data[[broadtype_col]] <- NA
  }
  if(anno_col_underscore %in% all_cols){
    anno_col <- anno_col_underscore
  } else if(anno_col_dot %in% all_cols){
    anno_col <- anno_col_dot
  } else {
    return(data)
  }
  data[[score_col]] <- 0
  if(!manual_info$broadtype %in% colnames(data)) {
    data[[manual_info$broadtype]] <- NA
    cat("- Create a reference column:", manual_info$broadtype, "\n")
    if(!is.na(manual_info$anno)) {
      for(i in 1:nrow(data)) {
        cell_type <- trimws(as.character(data[i, manual_info$anno]))
        if(!is.na(cell_type) && cell_type != "") {
          mapping_idx <- which(tolower(mapping$original_name)==tolower(cell_type))
          if(length(mapping_idx)>0) {
            data[i, manual_info$broadtype] <- mapping$broad_type[mapping_idx[1]]
          }
        }
      }
      cat("- It has been obtained from the reference column.", manual_info$anno, "Fill in ", manual_info$broadtype, "\n")
    }
  }
  match_count <- list(anno = 0, clid = 0, broadtype = 0)

  # Loop through each line: Complete the broadtype mapping and score calculation.
  cat("- tart processing line", nrow(data), "of the data...\n")
  for(i in 1:nrow(data)){
    # Show progress.
    if(i %% 1000 == 0) {
      cat("  Start Start processing line ", i, " of the data (", round(i/nrow(data)*100), "%)\r")
      flush.console()
    }
    
    cell_type <- trimws(as.character(data[i, anno_col]))
    if(!is.na(cell_type) && cell_type != ""){
      mapping_idx <- which(tolower(mapping$original_name)==tolower(cell_type))
      if(length(mapping_idx)>0){
        data[i, broadtype_col] <- mapping$broad_type[mapping_idx[1]]
      } else {
        data[i, broadtype_col] <- NA
      }
    }
    score <- 0
    
    if(!is.na(data[i, anno_col]) && !is.na(data[i, manual_info$anno]) && text_similarity_match(data[i, anno_col], data[i, manual_info$anno])){
      score <- 1
      match_count$anno <- match_count$anno + 1
    }
    else if(!is.na(data[i, manual_info$clid]) && !is.na(data[i, paste0(prefix, "_CLID")]) && data[i, paste0(prefix, "_CLID")] == data[i, manual_info$clid]){
      score <- 1
      match_count$clid <- match_count$clid + 1
    }
    else if(!is.na(manual_info$broadtype) && !is.na(data[i, broadtype_col]) && !is.na(data[i, manual_info$broadtype])){
      if(text_similarity_match(data[i, broadtype_col], data[i, manual_info$broadtype] )){
        score <- 0.5
        match_count$broadtype <- match_count$broadtype + 1
      }
    }
    data[i, score_col] <- score
  }
  
  # Summarize matching results.
  total_score <- sum(data[[score_col]], na.rm=TRUE)
  perfect_matches <- sum(data[[score_col]] == 1, na.rm=TRUE)
  partial_matches <- sum(data[[score_col]] == 0.5, na.rm=TRUE)
  
  cat("\n- model", prefix, "Processing complete. Matching statistics:\n")
  cat("  Total score:", total_score, "\n")
  cat("  Perfect match:", perfect_matches, "lines (", round(perfect_matches/nrow(data)*100, 1), "%)\n")
  cat("  Partial match:", partial_matches, "lines (", round(partial_matches/nrow(data)*100, 1), "%)\n")
  cat("  Matching details: anno=", match_count$anno, 
      ", CLid=", match_count$clid, 
      ", broadtype=", match_count$broadtype, "\n")
  
  return(data)
}

# Rearrange the columns of the data after processing by all models.
reorder_columns <- function(data, all_cols, anno_cols) {
  cat("\nRearrange the column order...\n")
  fixed_pattern <- "broadtype|anno|CLid|CLname|score|time"
  base_columns <- setdiff(all_cols, grep(fixed_pattern, all_cols, value = TRUE, ignore.case = TRUE))
  
  result_columns <- base_columns
  model_prefixes <- get_model_prefixes(all_cols)
  
  for(prefix in model_prefixes){
    separator <- ifelse(any(grepl(paste0("^", prefix, "_"), all_cols)), "_", ".")
    model_columns <- grep(paste0("^", prefix, "[_\\.]"), all_cols, value = TRUE)
    
    if(length(model_columns) > 0){
      ordered_cols <- character(0)
      # Replace the original order with a custom order.
      for(col_name in c("anno", "score", "broadtype", "CLname", "CLID", "time")) {
        # Use more precise pattern matching.
        exact_pattern <- paste0("^", prefix, separator, col_name, "$")
        col_found <- grep(exact_pattern, all_cols, value = TRUE, ignore.case = TRUE)
        if(length(col_found) > 0) {
          ordered_cols <- c(ordered_cols, col_found)
          cat(" Add columns:", col_found, "\n")
        }
      }
      # Add any other model-related columns that are not included.
      other_cols <- setdiff(model_columns, ordered_cols)
      if(length(other_cols) > 0) {
        ordered_cols <- c(ordered_cols, other_cols)
        cat("  Add other model-related columns:", paste(other_cols, collapse=", "), "\n")
      }
      result_columns <- c(result_columns, ordered_cols)
    }
  }
  missing_cols <- setdiff(all_cols, result_columns)
  if(length(missing_cols) > 0) {
    result_columns <- c(result_columns, missing_cols)
    cat("  Add the missing columns:", paste(missing_cols, collapse=", "), "\n")
  }
  
  # Rearrange
  cat("Rearrange in the order of 'anno', 'score', 'broadtype', 'CLname', 'CLID', 'time' \n")
  data <- data[, result_columns]
  cat("The rearrangement is completed.\n")
  return(data)
}
cat("====== Single-cell type annotation scoring program ======\n")
all_columns    <- colnames(df)
model_prefixes <- get_model_prefixes(all_columns)
manual_info    <- list(anno = "manual_anno", clid = "old_manual_CLID", broadtype = "manual_broadtype")
for(prefix in model_prefixes){
  df <- process_model(df, df.map, prefix, all_columns, manual_info)
  all_columns <- colnames(df)
}

# Export model scoring summary.
cat("\nSummary of Generative Model Scoring...\n")
model_scores_list <- list()
for(prefix in model_prefixes){
  anno_col  <- paste0(prefix, "_anno")
  score_col <- paste0(prefix, "_score")
  time_col  <- paste0(prefix, "_time")
  anno_data <- if(anno_col %in% all_columns) df[[anno_col]] else rep(NA, nrow(df))
  score_data <- if(score_col %in% all_columns) df[[score_col]] else rep(NA, nrow(df))
  time_data <- if(time_col %in% all_columns) df[[time_col]] else rep(NA, nrow(df))
  
  tmp_df <- data.frame(model = prefix,
                       anno  = anno_data,
                       score = score_data,
                       time  = time_data,
                       stringsAsFactors = FALSE)
  model_scores_list[[prefix]] <- tmp_df
}
if(length(model_scores_list) > 0){
  final_scores <- do.call(rbind, model_scores_list)
  write.table(final_scores, paste0(outdir, "/AICelltype_processed_full_scores.tsv"), sep="\t", quote=FALSE, row.names=FALSE)
  write.table(final_scores, paste0(outdir, "/AICelltype_processed_full_scores.tab"), sep="|", quote=FALSE, row.names=FALSE)
  cat("已生成打分摘要文件: AICelltype_processed_full_scores.tsv\n")
}
df.reorder <- reorder_columns(df, all_columns, grep("(_|\\.)[Aa]nno$", all_columns, value=TRUE))
for(sel in grep("[123]{1}$", colnames(df.reorder), value=T)) df.reorder[sel] <- NULL

for(col in names(df.reorder)) {
  if(is.character(df.reorder[[col]])) {
    df.reorder[[col]] <- gsub("\t", " ", df.reorder[[col]])
    df.reorder[[col]] <- gsub("\n", " ", df.reorder[[col]])
  }
}
write.table(df.reorder, paste0(outdir, "/AICelltype.anno.update.cl.final.tsv"), sep="\t", row.names=FALSE, quote=TRUE, na="NA")
write.table(df.reorder, paste0(outdir, "/AICelltype.anno.update.cl.final.tab"), sep="|", row.names=FALSE, quote=TRUE, na="NA")
cat("The processed data file has been generated: AICelltype_processed_Random_insertion_CL1.tsv\n")
cat("\n====== Processing completed ======\n")


# ------------------------------------------------------------------------------------------------------------------------------------------- #
# 
# 最高允许NA列的占比
cutoff       <- 0.2
data         <- df.reorder
data         <- data %>% mutate_all(~ifelse(. == "NA" , NA, .))
data         <- data[!is.na(data$id),]
data         <- data[!is.na(data$manual_anno),]
data         <- data[!data$manual_anno=="",]
headers      <- grep("_score$", colnames(data), value=TRUE)
df.sel           <- data[headers]
data         <- NULL
headers      <- gsub("[ _]{1}",".",headers)
headers      <- gsub(".score[.1]{0,2}$", "", headers)
colnames(df.sel) <- headers
for(sel in headers[2:length(headers)]){
  if(nrow((df.sel[is.na(data[paste0(sel, "_anno")]) |  (data[paste0(sel, "_anno")] == "") | (data[paste0(sel, "_anno")] == "unknow"),]))/nrow(df.sel)>(1 - cutoff)){
    df.sel[sel] <- NULL
    next
  }
  try({
    df.sel[is.na(df.sel[sel]),sel] = 0.0
  })
  if(nrow(df.sel[df.sel[sel]==0,])/nrow(df.sel) >0.9){
    df.sel[sel] <- NULL
    next
  }
  df.sel[sel]                = as.double(df.sel[,sel])
}
models.name <- c()
score.name  <- c()
score.data  <- c()
models.idx  <- c()
Total       <- c()
idx         <- 1
for(sel in names(df.sel)){
  score.1.0   <- nrow(df.sel[df.sel[sel]==1.0,])
  score.0.5   <- nrow(df.sel[df.sel[sel]==0.5,])
  score.0.0   <- nrow(df.sel[df.sel[sel]==0.0,])
  models.name <- c(models.name, rep(sel, time=3))
  score.name  <- c(score.name , c("Fully match", "Partially match", "Mismatch"))
  score.data  <- c(score.data, c(score.1.0, score.0.5, score.0.0))
  models.idx  <- c(models.idx, rep(idx, time=3))
  Total       <- c(Total     , rep(((score.1.0 + score.0.5) / (score.1.0 + score.0.5 + score.0.0)), time=3))
  idx = idx + 1
}
plot.data           <- c()
plot.data$index     <- 1:length(models.name)
plot.data$Agreement <- factor(score.name, levels = c("Mismatch", "Partially match", "Fully match"))
plot.data$score     <- score.data
plot.data$model     <- models.name
plot.data$total     <- Total
plot.data           <- as.data.frame(plot.data)
plot.data           <- plot.data[order(plot.data$total, plot.data$index, decreasing = TRUE),]
plot.data           <- as.data.frame(plot.data)
plot.data$model     <- factor(plot.data$model, levels = unique(plot.data$model))

p <- ggplot() + 
  geom_bar(data = plot.data,  aes(x=model ,y=score, fill=Agreement), stat="identity", width=0.9, position="fill")+
  theme_classic() + theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1))+
  labs(x = NULL, y = NULL) + 
  scale_y_continuous(breaks=seq(0,1,0.2), labels=c("0%", "20%", "40%", "60%", "80%", "100%"),expand=expansion(mult = c(0, 0)))  +
  scale_fill_manual(values = c("Fully match"="#d9eec5", "Partially match"="#e5c8e2", "Mismatch"="#f4dbc1"))
ggsave(filename=paste0(outdir, "/Model_performance_on_different_datasets_bar.png"), plot =p, width=11, height=6, dpi=300)
ggsave(filename=paste0(outdir, "/Model_performance_on_different_datasets_bar.pdf"), plot =p, width=11, height=6, dpi=300)
write.table(plot.data, paste0(outdir, "/Model_performance_on_different_datasets_bar.tab"), sep="|" , row.names=FALSE)
write.table(plot.data, paste0(outdir, "/Model_performance_on_different_datasets_bar.tsv"), sep="\t", row.names=FALSE)

# ------------------------------------------------------------------------------------------------------------------------------------------- #
library(tidyverse)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(pheatmap)
library(gridExtra)
library(reshape2)
library(stringr)
library(cowplot)
library(scales)
library(patchwork)
library(ggdendro)
library(ggnewscale)

# Define global color variables - maintain consistent visual style
LOW_COLOR     <- "#FDE0DD"
HIGH_COLOR    <- "#9ddaf2"
MISSING_COLOR <- "gray80"

# Set larger font size and high contrast theme (suitable for Nature level charts)
publication_theme <- theme_minimal(base_size = 20) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 22, margin = margin(b = 15)),
    axis.title = element_text(size = 22, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(30, 30, 30, 30)
  )

dpi <- 600
width <- 10
height <- 8

# Modify data validation auxiliary function - increase compatibility with wide tables
validate_data <- function(data_df, message="", required_cols=NULL) {
  # Check if the data box is empty
  if(nrow(data_df) == 0) {
    stop(paste("The data box is empty:", message))
  }
  
  # If no required column is specified, use default value
  if(is.null(required_cols)) {
    if("tissue" %in% colnames(data_df) && "mean_score" %in% colnames(data_df)) {
      # Long format data check
      req_cols <- c("model", "tissue", "mean_score")
    } else {
      # Wide format data only checks the model column
      req_cols <- c("model")
    }
  } else {
    req_cols <- required_cols
  }
  
  # Check if the specified key column exists
  missing_cols <- req_cols[!req_cols %in% colnames(data_df)]
  if(length(missing_cols) > 0) {
    stop(paste("Missing key columns:", paste(missing_cols, collapse=", ")))
  }
  
  # Check NA value
  na_counts <- colSums(is.na(data_df))
  if(any(na_counts > 0)) {
    message(paste("The data contains NA values:", message))
    print(na_counts[na_counts > 0])
  }
  
  return(data_df)
}

# Safe file saving function
safe_ggsave <- function(filename, plot, ...) {
  tryCatch({
    dir_path <- dirname(filename)
    if(!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message(paste("create directory:", dir_path))
    }
    ggsave(filename, plot, ...)
    message(paste("Successfully saved file:", filename))
  }, error = function(e) {
    warning(paste("Failed to save file:", filename, "\nerror:", e$message))
  })
}

# Reading data and data cleaning
data <- df.reorder

# Print data structure information
print("Raw data structure:")
str(data)
print("\nColumn Name:")
print(colnames(data))

# Initial data cleaning
data <- data %>%
  # Convert the 'NA' string to real NA
  mutate_all(~ifelse(. == "NA" | . == "", NA, .)) %>%
  # Processing organization name
  mutate(tissue = ifelse(is.na(tissue) | tissue == "", "Unknown", tissue)) %>%
  # Filter invalid data
  filter(!is.na(manual_anno) & manual_anno != "") %>%
  # Ensure that the 'tissue' column does not contain empty strings
  mutate(tissue = ifelse(tissue == "", "Unknown", tissue))

# Print basic information after data processing
print("Basic information after data cleaning:")
print(paste("Number of rows:", nrow(data)))
print("distribution:")
print(table(data$tissue))
print("manual_anno The number of unique values in a column:")
print(length(unique(data$manual_anno)))

# Extract the model name (columns ending in 'ainno') and remove the. 1 duplicate column
model_cols <- grep("_anno$", colnames(data), value = TRUE)
model_cols <- model_cols[!grepl("\\.1$", model_cols)]
model_names <- gsub("_anno$", "", model_cols)

cat("Total number of records:", nrow(data), "\n")
cat("Total number of models:", length(model_names), "\n")

validate_scores <- function(scores) {
  # Convert the data to characters first, then to numerical values, and suppress 
  # warnings (those that cannot be converted will become NA)
  scores_chr <- as.character(scores)
  valid_scores <- suppressWarnings(as.numeric(scores_chr))
  
  if(any(!is.na(valid_scores) & !(valid_scores %in% c(0, 0.5, 1)))) {
    warning("Discovery of non-standard score values! The value should be 0, 0.5, or 1")
    print(table(valid_scores, useNA = "always"))
  }
  return(valid_scores)
}

# Select effective models (at least 20% of the data is valid)
valid_models <- c()
model_validity_info <- data.frame(model = character(), valid_count = numeric(), 
                                  total = numeric(), percent_valid = numeric())
for(model in model_names) {
  anno_col <- paste0(model, "_anno")
  score_col <- paste0(model, "_score")
  if(anno_col %in% colnames(data) && score_col %in% colnames(data)) {
    non_na_count <- sum(!is.na(data[[anno_col]]))
    valid_percent <- non_na_count / nrow(data)
    model_validity_info <- rbind(model_validity_info, 
                               data.frame(model = model, valid_count = non_na_count, 
                                          total = nrow(data), percent_valid = valid_percent))
    if(valid_percent > 0.2) {
      valid_models <- c(valid_models, model)
    }
  }
}

# Check the organization type to ensure there is no NA
tissues <- unique(data$tissue)
tissues <- tissues[tissues != ""]
cat("Number of organizational types:", length(tissues), "\n")
cat("Organizational type:", paste(tissues, collapse = ", "), "\n")

# Collect rating data from various models and organizations
results <- data.frame()
for(model in valid_models) {
  model_anno_col <- paste0(model, "_anno")
  model_score_col <- paste0(model, "_score")
  if(all(c(model_anno_col, model_score_col) %in% colnames(data))) {
    for(tissue_type in tissues) {
      tissue_data <- data[data$tissue == tissue_type, ]
      tissue_scores <- validate_scores(tissue_data[[model_score_col]])
      valid_count <- sum(!is.na(tissue_scores))
      if(valid_count > 0) {
        mean_score <- mean(tissue_scores, na.rm = TRUE)
        exact_matches <- sum(tissue_scores == 1, na.rm = TRUE)
        exact_match_rate <- ifelse(valid_count > 0, exact_matches / valid_count, 0)
        broad_matches <- sum(tissue_scores == 0.5, na.rm = TRUE)
        broad_match_rate <- ifelse(valid_count > 0, broad_matches / valid_count, 0)
        no_matches <- sum(tissue_scores == 0, na.rm = TRUE)
        no_match_rate <- ifelse(valid_count > 0, no_matches / valid_count, 0)
        total_rate <- exact_match_rate + broad_match_rate + no_match_rate
        if(abs(total_rate - 1) > 0.01) {
          warning(paste("model", model, "tissue", tissue_type, "The total ratio of is not 1:", total_rate))
        }
        results <- rbind(results, data.frame(
          model = model,
          tissue = tissue_type,
          mean_score = mean_score,
          exact_match_rate = exact_match_rate,
          broad_match_rate = broad_match_rate,
          no_match_rate = no_match_rate,
          valid_predictions = valid_count
        ))
      }
    }
  }
}

cat("Total result record:", nrow(results), "\n")
cat("Missing value situation:\n")
print(colSums(is.na(results)))

# Check for duplicate model organization combinations
duplicate_entries <- results %>%
  group_by(model, tissue) %>%
  filter(n() > 1) %>%
  ungroup()

if(nrow(duplicate_entries) > 0) {
  warning("Duplicate model organization combinations discovered, will be merged:")
  print(duplicate_entries)
  
  # Take the average of duplicate entries
  results <- results %>%
    group_by(model, tissue) %>%
    summarize(
      mean_score = mean(mean_score, na.rm = TRUE),
      exact_match_rate = mean(exact_match_rate, na.rm = TRUE),
      broad_match_rate = mean(broad_match_rate, na.rm = TRUE),
      no_match_rate = mean(no_match_rate, na.rm = TRUE),
      valid_predictions = sum(valid_predictions),
      .groups = "drop"
    )
}

# Filter models with at least 10 valid predictions
results_filtered <- results %>% 
  group_by(model) %>% 
  filter(sum(valid_predictions) >= 10) %>% 
  ungroup()
cat("Record after filtering:", nrow(results_filtered), "from", n_distinct(results_filtered$model), "models\n")

# Optimize model renaming function (can automatically handle new models)
rename_model <- function(model_name) {
  patterns <- list(
    "deepseek\\.ai.DeepSeek\\.V3\\.0324"="DeepSeek V3 (0324)",
    "claude\\.3\\.5\\.haiku\\.\\d+" = "Claude 3.5 Haiku",
    "claude\\.3\\.5\\.sonnet\\.20241022" = "Claude 3.5 Sonnet (10/22)",
    "claude\\.3\\.5\\.sonnet\\.20240620" = "Claude 3.5 Sonnet (06/20)",
    "claude\\.3\\.5\\.sonnet\\.\\d+" = "Claude 3.5 Sonnet",
    "claude\\.3\\.haiku\\.\\d+" = "Claude 3 Haiku",
    "claude\\.3\\.opus\\.\\d+" = "Claude 3 Opus",
    "gpt\\.4o\\.2024\\.11\\.20" = "GPT-4o (11/20)",
    "gpt\\.4o(?!\\.mini)" = "GPT-4o",
    "gpt\\.4o\\.mini" = "GPT-4o Mini",
    "gpt\\.4\\.turbo" = "GPT-4 Turbo",
    "gpt\\.4\\.0125\\.preview" = "GPT-4 0125 Preview",
    "gpt\\.4\\.1106\\.preview" = "GPT-4 1106 Preview",
    "gpt\\.3\\.5\\.turbo$" = "GPT-3.5 Turbo",
    "gpt\\.3\\.5\\.turbo\\.0125" = "GPT-3.5 Turbo 0125",
    "gpt\\.3\\.5\\.turbo\\.1106" = "GPT-3.5 Turbo 1106",
    "gpt\\.3\\.5\\.turbo\\.0613" = "GPT-3.5 Turbo 0613",
    "Doubao\\.1\\.5\\.pro\\.256k" = "Doubao 1.5 Pro 256K",
    "Doubao\\.1\\.5\\.pro\\.32k" = "Doubao 1.5 Pro 32K",
    "Doubao\\.1\\.5\\.lite\\.32k" = "Doubao 1.5 Lite 32K",
    "Doubao\\.pro\\.256k" = "Doubao Pro 256K",
    "Doubao\\.pro\\.32k" = "Doubao Pro 32K",
    "Doubao\\.pro\\.4k" = "Doubao Pro 4K",
    "Doubao\\.lite\\.128k" = "Doubao Lite 128K",
    "Doubao\\.lite\\.32k" = "Doubao Lite 32K",
    "Doubao\\.lite\\.4k" = "Doubao Lite 4K",
    "aihubmix\\.Mistral\\.Large\\.2411" = "Mistral Large",
    "aihubmix\\.Llama\\.3\\.3\\.70B\\.Instruct" = "Llama 3.3 70B",
    "aihubmix\\.Llama\\.3\\.1\\.8B\\.Instruct" = "Llama 3.1 8B",
    "aihubmix\\.Llama\\.3\\.1\\.70B\\.Instruct" = "Llama 3.1 70B",
    "aihubmix\\.Llama\\.3\\.1\\.405B\\.Instruct" = "Llama 3.1 405B",
    "llama3\\.8b\\.8192" = "Llama 3 8B",
    "llama3\\.70b\\.8192" = "Llama 3 70B",
    "llama\\.3\\.2\\.1b\\.preview" = "Llama 3.2 1B",
    "llama\\.3\\.2\\.3b\\.preview" = "Llama 3.2 3B",
    "llama\\.3\\.2\\.11b\\.vision\\.preview" = "Llama 3.2 11B Vision",
    "llama\\.3\\.2\\.90b\\.vision\\.preview" = "Llama 3.2 90B Vision",
    "DeepSeek\\.V3" = "DeepSeek V3",
    "DeepSeek\\.R1$" = "DeepSeek R1",
    "aihubmix\\.DeepSeek\\.R1" = "DeepSeek R1 (Hub)",
    "deepseek\\.r1\\.distill\\.llama\\.70b" = "DeepSeek R1 Distill Llama 70B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.1\\.5B" = "DeepSeek R1 Distill Qwen 1.5B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.32B" = "DeepSeek R1 Distill Qwen 32B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.14B" = "DeepSeek R1 Distill Qwen 14B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.7B" = "DeepSeek R1 Distill Qwen 7B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Llama\\.8B" = "DeepSeek R1 Distill Llama 8B",
    "aihub\\.Phi\\.4" = "Phi-4",
    "gemini\\.2\\.0\\.flash\\.thinking\\.exp\\.01\\.21" = "Gemini 2.0 Flash Thinking",
    "gemini\\.2\\.0\\.flash\\.lite\\.preview\\.02\\.05" = "Gemini 2.0 Flash Lite",
    "gemini\\.2\\.0\\.flash\\.search" = "Gemini 2.0 Flash Search",
    "gemini\\.2\\.0\\.flash$" = "Gemini 2.0 Flash",
    "Qwen\\.QVQ\\.72B\\.Preview" = "Qwen QVQ 72B",
    "qwen\\.max\\.0125" = "Qwen Max 0125",
    "Qwen\\.Qwen2\\.5\\.72B\\.Instruct" = "Qwen 2.5 72B",
    "Qwen\\.Qwen2\\.5\\.32B\\.Instruct" = "Qwen 2.5 32B",
    "qwen2\\.5\\.14b\\.instruct" = "Qwen 2.5 14B",
    "qwen2\\.5\\.7b\\.instruct" = "Qwen 2.5 7B",
    "qwen2\\.5\\.3b\\.instruct" = "Qwen 2.5 3B",
    "qwen2\\.5\\.coder\\.1\\.5b\\.instruct" = "Qwen 2.5 Coder 1.5B",
    "o1\\.mini" = "O1 Mini",
    "chutesai\\.Llama\\.4\\.Maverick\\.17B\\.128E\\.Instruct\\.FP8$" = "Llama 4 Maverick 17B 128E FP8",
    "chutesai\\.Llama\\.4\\.Scout\\.17B\\.16E\\.Instruct$" = "Llama 4 Scout 17B 16E"
  )
  for(pattern in names(patterns)) {
    if(grepl(pattern, model_name, perl = TRUE)) {
      return(patterns[[pattern]])
    }
  }
  return(model_name)
}

# Rename Model
unique_models <- unique(results_filtered$model)
for(i in seq_along(unique_models)) {
  old_name <- unique_models[i]
  new_name <- rename_model(old_name)
  results_filtered$model[results_filtered$model == old_name] <- new_name
}

results_filtered$tissue <- str_trim(results_filtered$tissue)

# Calculate the overall score of the model and sort it in descending order of scores (with high scores first)
model_overall_scores <- results_filtered %>% 
  group_by(model) %>% 
  summarize(
    overall_mean_score = weighted.mean(mean_score, valid_predictions, na.rm = TRUE),
    overall_exact_match = weighted.mean(exact_match_rate, valid_predictions, na.rm = TRUE),
    overall_broad_match = weighted.mean(broad_match_rate, valid_predictions, na.rm = TRUE),
    overall_no_match = weighted.mean(no_match_rate, valid_predictions, na.rm = TRUE),
    total_predictions = sum(valid_predictions),
    tissue_count = n_distinct(tissue),
    .groups = "drop"
  ) %>% 
  arrange(desc(overall_mean_score))

# Add model ranking
results_filtered <- results_filtered %>% 
  left_join(model_overall_scores %>% 
              mutate(model_rank = row_number()) %>% 
              select(model, model_rank, overall_mean_score),
            by = "model")

# Organizational clustering analysis
# Check for duplicate model organization combinations
duplicates <- results_filtered %>%
  group_by(model, tissue) %>%
  filter(n() > 1)

if(nrow(duplicates) > 0) {
  warning("Duplicate model organization combinations were found before organizational clustering, and the mean will be taken and merged:")
  print(duplicates)
  
  # Merge duplicate items and take the average score
  results_filtered <- results_filtered %>%
    group_by(model, tissue) %>%
    summarize(
      mean_score = mean(mean_score, na.rm = TRUE),
      valid_predictions = sum(valid_predictions),
      overall_mean_score = first(overall_mean_score),
      model_rank = first(model_rank),
      .groups = "drop"
    )
}

# Use the summarized_duplicates function to handle duplicate values
heatmap_data <- results_filtered %>% 
  select(model, tissue, mean_score) %>% 
  pivot_wider(
    names_from = tissue, 
    values_from = mean_score,
    values_fn = mean  # If there are duplicate values, take the average
  )

# Use the modified validate_data function to check heatmap data
validate_data(heatmap_data, "Organize clustering data", required_cols = c("model"))

# Prepare clustering matrix
hm_matrix <- as.matrix(heatmap_data %>% select(-model))
rownames(hm_matrix) <- heatmap_data$model

# Secure handling of NA values - for clustering purposes only, will not affect the final visualization
hm_matrix_for_clustering <- hm_matrix
hm_matrix_for_clustering[is.na(hm_matrix_for_clustering)] <- -0.1  # Use special values to represent NA

# Cluster the organization
tissue_dist <- dist(t(hm_matrix_for_clustering))
tissue_hc <- hclust(tissue_dist)
tissue_order <- colnames(hm_matrix)[tissue_hc$order]

# Create an aligned clustering tree diagram
ddata <- dendro_data(tissue_hc)

# Create an x-coordinate mapping to ensure alignment with the heatmap
offset <- 2  # Reserve space for Left and Spacer columns
label_positions <- seq_along(tissue_order) + offset
names(label_positions) <- tissue_order

# Calculate the old coordinate range and the new coordinate range
old_range_x <- range(ddata$segments$x, ddata$segments$xend)
new_range_x <- range(label_positions)

# Add a secure coordinate mapping function (to prevent zero division errors)
rescale_coord_x <- function(x) {
  # Check if the range is zero (to prevent zero division errors)
  if(abs(old_range_x[2] - old_range_x[1]) < 1e-10) {
    # Return to the midpoint of the new range
    return(mean(new_range_x))
  } else {
    # Normal mapping
    return(((x - old_range_x[1]) / (old_range_x[2] - old_range_x[1])) * 
           (new_range_x[2] - new_range_x[1]) + new_range_x[1])
  }
}

# Application Mapping - x-Coordinate
ddata$segments$x <- sapply(ddata$segments$x, rescale_coord_x)
ddata$segments$xend <- sapply(ddata$segments$xend, rescale_coord_x)

# Compress tree height - use a more reasonable scaling factor
scale_factor <- 0.15  
ddata$segments$y <- ddata$segments$y * scale_factor
ddata$segments$yend <- ddata$segments$yend * scale_factor

# Create a clustering tree diagram
dendro_plot <- ggplot() +
  geom_segment(data = ddata$segments,
               aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0.5, length(tissue_order) + offset + 0.5),
    expand = c(0, 0)
  )

# Sort the models by overall performance (note: use ascending order in the heatmap, with high scores at the bottom)
# Clearly state that this is intentional and follows the conventions of heat maps
model_order <- model_overall_scores %>% 
  arrange(overall_mean_score) %>%  # Sort in ascending order when displayed, with high scores at the top
  pull(model)

# Prepare heat map data
tissue_long <- results_filtered %>% 
  select(model, tissue, mean_score) %>% 
  mutate(
    model = factor(model, levels = model_order),
    tissue = factor(tissue, levels = tissue_order)
  )

# Validate heatmap data using the modified validation function
validate_data(tissue_long, "Organize heat map data")

# Prepare overall score data
overall_scores <- results_filtered %>% 
  select(model, overall_mean_score) %>% 
  distinct() %>% 
  mutate(
    model = factor(model, levels = model_order),
    mean_score = overall_mean_score
  )

# Verify overall score data
validate_data(overall_scores, "Overall score data", required_cols = c("model", "mean_score"))

# Create label vectors (hide Spacer labels)
all_x_labels <- c("Overall Score", "Spacer", tissue_order)
x_labels <- ifelse(all_x_labels == "Spacer", "", all_x_labels)

# Draw heat map section
heatmap_plot <- ggplot() +
  # Left overall classification
  geom_tile(data = overall_scores,
            aes(x = "Overall Score", y = model, fill = mean_score, width=2)) +
  scale_fill_gradientn(
    colors = colorRampPalette(c(LOW_COLOR, HIGH_COLOR))(100),
    limits = c(min(overall_scores$mean_score, na.rm=TRUE), 
               max(overall_scores$mean_score, na.rm=TRUE)),
    name = "Overall Score"
  ) +
  # Add blank interval column
  geom_tile(aes(x = "Spacer", y = model_order), 
            fill = "white", color = NA) +
  # New filling scale
  ggnewscale::new_scale_fill() +
  # Heat map of each organization on the right
  geom_tile(data = tissue_long,
            aes(x = tissue, y = model, fill = mean_score),
            na.rm = FALSE) +  # Do not ignore NA value
  # Add special tags to NA values
  geom_tile(data = tissue_long %>% filter(is.na(mean_score)),
            aes(x = tissue, y = model),
            fill = MISSING_COLOR, width=1) +
  scale_fill_gradientn(
    colors = colorRampPalette(c(LOW_COLOR, HIGH_COLOR))(100),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0),
    labels = c("0.0", "0.25", "0.5", "0.75", "1.0"),
    name = "Tissue Score",
    na.value = MISSING_COLOR  # NA value uses special colors
  ) +
  # Coordinate axis setting
  scale_x_discrete(
    limits = c("Overall Score", "Spacer", tissue_order),
    labels = x_labels
  ) +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 0, r = 20, b = 20, l = 20)
  )

# Combine tree and heat maps, add global headings
p_combined <- dendro_plot / (heatmap_plot + 
  # Only add text annotations for the overall column on the left side
  geom_text(
    data = overall_scores,
    aes(x = "Overall Score", y = model, label = sprintf("%.2f", mean_score)),
    size = 3.2,
    vjust = 0.5, 
    hjust = 0.5
  )) + 
  plot_layout(heights = c(0.2, 4)) +  # Adjust the scale of the tree diagram and heatmap
  plot_annotation(
    title = "Accuracy of Cell Type Prediction by Large Language Models",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
    )
  )

# display graphics
print(p_combined)

# Safely store high-quality images
safe_ggsave(paste0(outdir, "/model_tissue_heatmap_optimized.pdf") , p_combined, width = 18, height = 20, limitsize = FALSE)
safe_ggsave(paste0(outdir, "/model_tissue_heatmap_optimized.png"), p_combined, width = 16, height = 26, bg = "transparent", limitsize = FALSE)
write.table(overall_scores, paste0(outdir, "/model_tissue_heatmap_optimized.tab"), sep="|" , row.names = FALSE)
write.table(overall_scores, paste0(outdir, "/model_tissue_heatmap_optimized.tsv"), sep="\t", row.names = FALSE)
# Calculate and save the statistical results of model matching rate

# Calculate the matching statistics of each model

# First calculate the weighted average and filter out invalid weights
model_match_stats_weighted <- results_filtered %>%
  group_by(model) %>%
  # Filter out rows with invalid weights (NA or<=0)
  filter(!is.na(valid_predictions) & valid_predictions > 0) %>%
  # If the filtered group is empty, return NA or default value, otherwise calculate weighted average
  summarize(
    complete_match_rate = if(n() > 0) weighted.mean(exact_match_rate, valid_predictions, na.rm = TRUE) else NA_real_,
    partial_match_rate = if(n() > 0) weighted.mean(broad_match_rate, valid_predictions, na.rm = TRUE) else NA_real_,
    error_rate = if(n() > 0) weighted.mean(no_match_rate, valid_predictions, na.rm = TRUE) else NA_real_,
    mean_score = if(n() > 0) weighted.mean(mean_score, valid_predictions, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

# Recalculate the total number of predictions and organizations (based on unfiltered data)
model_totals <- results_filtered %>%
  group_by(model) %>%
  summarize(
    total_predictions = sum(valid_predictions, na.rm = TRUE), # Using na.rm=True
    tissue_count = n_distinct(tissue),
    .groups = "drop"
  )

# Consolidation Result
model_match_stats <- model_totals %>%
  left_join(model_match_stats_weighted, by = "model") %>%
  # Ensure that the sum of the perfect match rate, partial match rate, and error rate is 1 (only for models with valid weights)
  mutate(
    # Normalize only when all three rates are non NA
    normalize = !is.na(complete_match_rate) & !is.na(partial_match_rate) & !is.na(error_rate),
    sum_rates = ifelse(normalize, complete_match_rate + partial_match_rate + error_rate, NA_real_),
    # Normalization processing (to prevent rounding errors), only when sum_rates>0
    complete_match_rate = ifelse(normalize & !is.na(sum_rates) & sum_rates > 0, complete_match_rate / sum_rates, complete_match_rate),
    partial_match_rate = ifelse(normalize & !is.na(sum_rates) & sum_rates > 0, partial_match_rate / sum_rates, partial_match_rate),
    error_rate = ifelse(normalize & !is.na(sum_rates) & sum_rates > 0, error_rate / sum_rates, error_rate)
  ) %>%
  # Sort by descending average score
  arrange(desc(mean_score)) %>%
  # Delete the middle column
  select(-normalize, -sum_rates)

# Round the result to 4 decimal places
model_match_stats <- model_match_stats %>%
  mutate(across(where(is.numeric), ~round(., 4)))

# Save to Results
write.table(model_match_stats, paste0(outdir, "/model_match_stats.tab"), sep = "|" , row.names = FALSE, quote = FALSE)
write.table(model_match_stats, paste0(outdir, "/model_match_stats.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)

# Print the first few lines of results for confirmation
cat(paste0("Generated model matching rate statistics and saved to ", outdir, "/model_match_stats.tsv\n"))
print(head(model_match_stats))


# ------------------------------------------------------------------------------------------------------------------------------------------- #
# Model_Random_insertion_data_experiment
# filepath = paste0(outdir, "/AICelltype_processed_CL.tsv")
df.reorder <- read.table(paste0(outdir, "/Model_Random_insertion_data_experiment.tab"), sep="|", header=TRUE)


genefile = "../data/all_gene.csv"
data         <- df.reorder
data         <- data %>% mutate_all(~ifelse(. == "NA" | . == "0", NA, .))
data         <- subset(data, !(is.na(id)|is.na(marker)))
genes        <- read.csv(genefile, header = FALSE)
genes        <- genes$V1
headers      <- c(
  "id",
  "dataset",
  "tissue",
  "marker",
  "manual_anno",
  "manual_broadtype",
  "old_manual_CLname",
  "old_manual_CLID"
)
df.ins           <- data[headers]
df.ins[is.na(df.ins$tissue), "tissue"] <- "unknow"
set.seed(123)

proportions   <- list(0, 0.2, 0.4, 0.6, 0.8, 1.0)
model         <- "claude-3-5-sonnet-20240620"
ids           <- df.ins$id
for(proportion in proportions){
  if(!paste0("marker_",proportion)%in%colnames(df.ins)){
    df.ins[paste0("marker_"   , proportion)]            <- ""
  }
  if(!paste0("celltype_",proportion,"_anno") %in%colnames(df.ins)){
    df.ins[paste0("celltype_", proportion, "_anno")]    <- ""  
  }
  if(!paste0("celltype_",proportion,"_CLID")%in%colnames(df.ins)){
    df.ins[paste0("celltype_" , proportion, "_CLID")]   <- ""  
  }
  if(!paste0("celltype_" , proportion, "_CLname")%in%colnames(df.ins)){
    df.ins[paste0("celltype_" , proportion, "_CLname")] <- "" 
  }
  for(dataset.name in unique(df.ins$dataset))
  {
    dataset.df <- subset(df.ins, dataset==dataset.name)
    for(tissue.name in unique(dataset.df$tissue))
    {
      tissue.df  <- subset(dataset.df, tissue==tissue.name)
      if(nrow(tissue.df)==0){
        next
      }
      start  <- 1
      end    <- nrow(tissue.df)
      sep    <- 20
      status <- TRUE
      while(status){
        ed = start + sep - 1
        if(ed >= end){
          ed     = end
          status = FALSE
        }
        markergenes <- tissue.df[start:ed,]
        for(id in markergenes$id)
        {
          marker.c                                       <- df.ins[df.ins$id==id, "marker"]
          add.n                                          <- round(length(strsplit(marker.c, ',')[[1]]) * proportion)
          df.ins[df.ins["id"]==id,paste0("marker_", proportion)] <- paste(c(marker.c, sample(genes, add.n)), collapse=",")
        }
        markergene  <- paste(df.ins[markergenes$id, paste0("marker_", proportion)], collapse= "\n")
        print(proportion)
        print(markergene)
        num = 3
        while(num > 0)
        {
          try({
            celltype <- GetCellType(markergenes=markergene, model=model, tissuename=tissue.name)
            if((class(celltype) != class('')) & (length(celltype$content) == length(start:ed))){
              df.ins[markergenes$id,  paste0("celltype_", proportion, "_anno")] <- unname(unlist(celltype$content))
              break
            }
          })
          num = num - 1
        }
        start = start + sep
      }
    }
  }
}
all_columns    <- grep("^celltyp.*anno", colnames(df.ins), value=TRUE)
all_columns    <- gsub("_anno"         , ""              ,all_columns)
for(prefix in all_columns){
  for(id in ids){
    cell.name = df.ins[df.ins$id==id, paste0(prefix, "_anno")]
    CLID      = df.ins[df.ins$id==id, paste0(prefix, "_CLID")]
    if((!is.na(CLID)) & (CLID!="") &(CLID!="0")) next;
    if((!is.na(cell.name)) | (cell.name == "")){
      try(
        {
          cl        <- getOlsinfo(cell.name)  
          df.ins[df.ins$id==id, c(paste0(prefix, "_CLname"), paste0(prefix, "_CLID"))] <- cl[2:3]
        }
      )
    }
  }
}

for(prefix in all_columns){
  df.ins <- process_model(df.ins, df.map, prefix, colnames(df.ins), manual_info)
  all_columns <- colnames(df.ins)
}
write.table(df.ins, paste0(outdir,"/Model_Random_insertion_data_experiment.tab"), sep="|" ,row.names = FALSE)
write.table(df.ins, paste0(outdir,"/Model_Random_insertion_data_experiment.tsv"), sep="\t",row.names = FALSE)

# ----------- draw pictures -----------
# Check data dimensions and structure
print("Dataset dimension:")
print(dim(df.ins))
print("The first few columns of the dataset:")
print(head(df.ins[, c("id", "dataset", "tissue", "marker")]))

# Check the noise level score column in the data
noise_cols <- c("celltype_0_score", "celltype_0.2_score", "celltype_0.4_score", 
                "celltype_0.6_score", "celltype_0.8_score", "celltype_1_score")
print("Does the classification of noise levels exist:")
print(noise_cols %in% names(df.ins))

# ========== Data processing and shaping ==========
# Select the required columns for analysis
df_selected <- df.ins %>%
  select(id, tissue, dataset, celltype_0_score, celltype_0.2_score, 
         celltype_0.4_score, celltype_0.6_score, celltype_0.8_score, celltype_1_score)

# Check the NA value situation
print("Number of NA values in each column:")
print(colSums(is.na(df_selected)))

# Convert data to a long format for easy analysis and visualization
scores_long <- df_selected %>%
  pivot_longer(
    cols = c(celltype_0_score, celltype_0.2_score, celltype_0.4_score, 
             celltype_0.6_score, celltype_0.8_score, celltype_1_score),
    names_to = "noise_level",
    values_to = "score",
    values_drop_na = TRUE
  ) %>%
  mutate(
    noise_level = case_when(
      noise_level == "celltype_0_score" ~ "0% Noise",
      noise_level == "celltype_0.2_score" ~ "20% Noise",
      noise_level == "celltype_0.4_score" ~ "40% Noise",
      noise_level == "celltype_0.6_score" ~ "60% Noise",
      noise_level == "celltype_0.8_score" ~ "80% Noise",
      noise_level == "celltype_1_score" ~ "100% Noise"
    ),
    noise_level = factor(noise_level, levels = c("0% Noise", "20% Noise", "40% Noise", 
                                                 "60% Noise", "80% Noise", "100% Noise"))
  )

# Calculate the average accuracy by grouping by organization and noise level
tissue_scores <- scores_long %>%
  group_by(tissue, noise_level) %>%
  summarise(
    mean_accuracy = mean(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(tissue)) # Filter out rows with missing organizational information

# Check the calculation results
print("Example of Organizational Score:")
print(head(tissue_scores, 12))

# Calculate the overall average accuracy of each noise level
overall_accuracy <- scores_long %>%
  group_by(noise_level) %>%
  summarise(
    mean_accuracy = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

print("Overall average accuracy of various noise levels:")
print(overall_accuracy)

# ========== Visualization Analysis ==========
# 1. Original violin chart - accuracy distribution of each noise level
violin_plot <- ggplot(tissue_scores, aes(x = mean_accuracy, y = as.numeric(noise_level), group = noise_level)) +
  geom_violin(fill = "#1f78b4", alpha = 0.5, color = "grey50") +
  geom_point(color = "#2171b5", size = 3, alpha = 0.85,
             position = position_jitter(width = 0, height = 0.03)) +
  scale_y_reverse(breaks = c(6, 5, 4, 3, 2, 1),
                  labels = c("100% Noise", "80% Noise", "60% Noise", "40% Noise", "20% Noise", "0% Noise"),
                  limits = c(6.5, 0.5)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(
    x = "Prediction accuracy",
    y = NULL,
    title = "Distribution of accuracy across noise levels"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10), face = "bold")
  )

# 2. Box plot - clearer display of accuracy distribution for various noise levels
box_plot <- ggplot(tissue_scores, aes(x = noise_level, y = mean_accuracy, fill = noise_level)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Accuracy distribution by noise level",
    x = "Noise Level",
    y = "Prediction Accuracy"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 3. Line chart - Display the impact trend of noise on accuracy by organization
# Select the top 15 organizations with the largest sample size
top_tissues <- tissue_scores %>%
  group_by(tissue) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  head(15) %>%
  pull(tissue)

line_plot <- tissue_scores %>%
  filter(tissue %in% top_tissues) %>%
  ggplot(aes(x = noise_level, y = mean_accuracy, color = tissue, group = tissue)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Accuracy trends across noise levels by tissue",
    x = "Noise Level",
    y = "Mean Accuracy",
    color = "Tissue"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 4. Heatmap - Accuracy of All Organizations at Different Noise Levels
heatmap_plot <- tissue_scores %>%
  filter(tissue %in% top_tissues) %>%
  ggplot(aes(x = noise_level, y = reorder(tissue, mean_accuracy), fill = mean_accuracy)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Accuracy heatmap by tissue and noise level",
    x = "Noise Level",
    y = "Tissue",
    fill = "Accuracy"
  ) +
  theme_minimal()

# 5. Overall Trend Chart - Display the Relationship between Noise and Average Accuracy
trend_plot <- ggplot(overall_accuracy, aes(x = noise_level, y = mean_accuracy, group = 1)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Overall accuracy trend with increasing noise",
    x = "Noise Level",
    y = "Mean Accuracy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ========== Combining charts and saving ==========
# Combine charts at the top left, top right, and bottom
top_row <- plot_grid(violin_plot, box_plot, ncol = 2, labels = c("A", "B"))
middle_row <- plot_grid(line_plot, trend_plot, ncol = 2, labels = c("C", "D"))
combined_plot <- plot_grid(top_row, middle_row, heatmap_plot, 
                           ncol = 1, 
                           labels = c("", "", "E"), 
                           rel_heights = c(1, 1, 1.2))

# Add overall title
title <- ggdraw() + 
  draw_label(
    "AICelltype Model Robustness with Random Noise Genes",
    fontface = "bold",
    size = 14,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(b = 10, t = 10, l = 0, r = 0)
  )

# Combining titles and charts
final_plot <- plot_grid(
  title, combined_plot,
  ncol = 1,
  rel_heights = c(0.05, 1)
)

# Save Chart
ggsave(paste0(outdir, "/AICelltype_ins_noise_analysis.pdf"), final_plot, width = 12, height = 16, units = "in", dpi = 300)
ggsave(paste0(outdir, "/AICelltype_ins_noise_analysis.png"), final_plot, width = 12, height = 16, units = "in", dpi = 300)


main_violin_plot <- ggplot(tissue_scores %>% filter(noise_level %in% c("0% Noise", "20% Noise", "40% Noise", "60% Noise")), 
                           aes(x = mean_accuracy, y = as.numeric(noise_level), group = noise_level)) +
  geom_violin(fill = "#1f78b4", alpha = 0.5, color = "grey50") +
  geom_point(color = "#2171b5", size = 3, alpha = 0.85,
             position = position_jitter(width = 0, height = 0.03)) +
  scale_y_reverse(breaks = c(4, 3, 2, 1),
                  labels = c("60% Noise", "40% Noise", "20% Noise", "0% Noise"),
                  limits = c(4.5, 0.5)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(
    x = "Prediction accuracy",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10), face = "bold")
  ) + ggtitle("")

# Save the original style chart
ggsave(paste0(outdir, "/AICelltype_ins_robustness_detail_original.pdf"), main_violin_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave(paste0(outdir, "/AICelltype_ins_robustness_detail_original.png"), main_violin_plot, width = 10, height = 6, units = "in", dpi = 300)
# ------------------------------------------------------------------------------------------------------------------------------------------- #
# Model Random deletion
data         <- df.reorder
data         <- data %>% mutate_all(~ifelse(. == "NA" | . == "0", NA, .))
data         <- subset(data, !(is.na(id)|is.na(marker)))
genes        <- read.csv(genefile, header = FALSE)
genes        <- genes$V1
headers      <- c(
  "id",
  "dataset",
  "tissue",
  "marker",
  "manual_anno",
  "manual_broadtype",
  "old_manual_CLname",
  "old_manual_CLID"
)
df.del        <- data[headers]
set.seed(123)
df.del[is.na(df.del$tissue), "tissue"] <- "unknow"
proportions   <- list(0, 0.2, 0.4, 0.6, 0.8)
model         <- "claude-3-5-sonnet-20240620"
for(proportion in proportions){
  if(!paste0("marker_",proportion)%in%colnames(df.del)){
    df.del[paste0("marker_"   , proportion)]            <- ""
  }
  if(!paste0("celltype_",proportion,"_anno") %in%colnames(df.del)){
    df.del[paste0("celltype_", proportion, "_anno")]    <- ""  
  }
  if(!paste0("celltype_",proportion,"_CLID")%in%colnames(df.del)){
    df.del[paste0("celltype_" , proportion, "_CLID")]   <- ""  
  }
  if(!paste0("celltype_" , proportion, "_CLname")%in%colnames(df.del)){
    df.del[paste0("celltype_" , proportion, "_CLname")] <- "" 
  }
  for(dataset.name in unique(df.del$dataset))
  {
    dataset.df <- subset(df.del, dataset==dataset.name)
    for(tissue.name in unique(dataset.df$tissue))
    {
      tissue.df  <- subset(dataset.df, tissue==tissue.name)
      if(nrow(tissue.df)==0){
        next
      }
      start  <- 1
      end    <- nrow(tissue.df)
      sep    <- 20
      status <- TRUE
      while(status){
        ed = start + sep - 1
        if(ed >= end){
          ed     = end
          status = FALSE
        }
        markergenes <- tissue.df[start:ed,]
        for(id in markergenes$id)
        {
          marker.c                                       <- df.del[df.del$id==id,"marker"]
          marker.c                                       <- strsplit(marker.c, ',')[[1]]
          marker.n                                       <- round(length(marker.c) * (1 - proportion))
          df.del[df.del["id"]==id,paste0("marker_", proportion)] <- paste(sample(marker.c, marker.n), collapse=",")
        }
        markergene  <- paste(df.del[markergenes$id, paste0("marker_", proportion)], collapse= "\n")
        print(proportion)
        print(markergene)
        num = 3
        while(num > 0)
        {
          try({
            celltype <- GetCellType(markergenes=markergene, model=model, tissuename=tissue.name)
            if((class(celltype) != class('')) & (length(celltype$content) == length(start:ed))){
              df.del[markergenes$id,  paste0("celltype_", proportion, "_anno")] <- unname(unlist(celltype$content))
              break
            }
          })
          num = num - 1
        }
        start = start + sep
      }
    }
  }
}
all_columns    <- grep("^celltyp.*anno", colnames(df.del), value=TRUE)
all_columns    <- gsub("_anno"         , ""              ,all_columns)
for(prefix in all_columns){
  for(id in df.del$id){
    cell.name = df.del[df.del$id==id, paste0(prefix, "_anno")]
    CLID      = df.del[df.del$id==id, paste0(prefix, "_CLID")]
    if((!is.na(CLID)) & (CLID!="") &(CLID!="0")) next;
    if((!is.na(cell.name)) | (cell.name == "")){
      try(
        {
          cl        <- getOlsinfo(cell.name)  
          df.del[df.del$id==id, c(paste0(prefix, "_CLname"), paste0(prefix, "_CLID"))] <- cl[2:3]
        }
      )
    }
  }
}
for(prefix in all_columns){
  df.del      <- process_model(df.del, df.map, prefix, colnames(df.del), manual_info)
  all_columns <- colnames(df.del)
}
write.table(df.del, paste0(outdir,"/Model_Random_deletion_data_experiment.tab"), sep="|" ,row.names = FALSE)
write.table(df.del, paste0(outdir,"/Model_Random_deletion_data_experiment.tsv"), sep="\t",row.names = FALSE)

# draw pictures
# Check data dimensions and structure
print("Dataset dimension:")
print(dim(df.del))
print("The first few columns of the dataset:")
print(head(df.del[, c("id", "dataset", "tissue", "marker")]))

# Check the noise level score column in the data
noise_cols <- c("celltype_0_score", "celltype_0.2_score", "celltype_0.4_score", 
                "celltype_0.6_score", "celltype_0.8_score")
print("Does the classification of noise levels exist:")
print(noise_cols %in% names(df.del))

# ========== Data processing and shaping ==========
# Select the required columns for analysis
df_selected <- df.del %>%
  select(id, tissue, dataset, celltype_0_score, celltype_0.2_score, 
         celltype_0.4_score, celltype_0.6_score, celltype_0.8_score)

# Check the NA value situation
print("Number of NA values in each column:")
print(colSums(is.na(df_selected)))

# Convert data to a long format for easy analysis and visualization
scores_long <- df_selected %>%
  pivot_longer(
    cols = c(celltype_0_score, celltype_0.2_score, celltype_0.4_score, 
             celltype_0.6_score, celltype_0.8_score),
    names_to = "noise_level",
    values_to = "score",
    values_drop_na = TRUE
  ) %>%
  mutate(
    noise_level = case_when(
      noise_level == "celltype_0_score" ~ "0% Noise",
      noise_level == "celltype_0.2_score" ~ "20% Noise",
      noise_level == "celltype_0.4_score" ~ "40% Noise",
      noise_level == "celltype_0.6_score" ~ "60% Noise",
      noise_level == "celltype_0.8_score" ~ "80% Noise"
    ),
    noise_level = factor(noise_level, levels = c("0% Noise", "20% Noise", "40% Noise", 
                                                 "60% Noise", "80% Noise"))
  )

# Calculate the average accuracy by grouping by organization and noise level
tissue_scores <- scores_long %>%
  group_by(tissue, noise_level) %>%
  summarise(
    mean_accuracy = mean(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(tissue)) # Filter out rows with missing organizational information

# Check the calculation results
print("Example of Organizational Score:")
print(head(tissue_scores, 12))

# Calculate the overall average accuracy of each noise level
overall_accuracy <- scores_long %>%
  group_by(noise_level) %>%
  summarise(
    mean_accuracy = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

print("Overall average accuracy of various noise levels:")
print(overall_accuracy)

# ========== Visualization Analysis ==========
# 1. Original violin chart - accuracy distribution of each noise level
violin_plot <- ggplot(tissue_scores, aes(x = mean_accuracy, y = as.numeric(noise_level), group = noise_level)) +
  geom_violin(fill = "#1f78b4", alpha = 0.5, color = "grey50") +
  geom_point(color = "#2171b5", size = 3, alpha = 0.85,
             position = position_jitter(width = 0, height = 0.03)) +
  scale_y_reverse(breaks = c(6, 5, 4, 3, 2, 1),
                  labels = c("100% Noise", "80% Noise", "60% Noise", "40% Noise", "20% Noise", "0% Noise"),
                  limits = c(6.5, 0.5)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(
    x = "Prediction accuracy",
    y = NULL,
    title = "Distribution of accuracy across noise levels"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10), face = "bold")
  )

# 2. Box plot - clearer display of accuracy distribution for various noise levels
box_plot <- ggplot(tissue_scores, aes(x = noise_level, y = mean_accuracy, fill = noise_level)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Accuracy distribution by noise level",
    x = "Noise Level",
    y = "Prediction Accuracy"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 3. Line chart - Display the impact trend of noise on accuracy by organization
# Select the top 15 organizations with the largest sample size
top_tissues <- tissue_scores %>%
  group_by(tissue) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  head(15) %>%
  pull(tissue)

line_plot <- tissue_scores %>%
  filter(tissue %in% top_tissues) %>%
  ggplot(aes(x = noise_level, y = mean_accuracy, color = tissue, group = tissue)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Accuracy trends across noise levels by tissue",
    x = "Noise Level",
    y = "Mean Accuracy",
    color = "Tissue"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 4. Heatmap - Accuracy of All Organizations at Different Noise Levels
heatmap_plot <- tissue_scores %>%
  filter(tissue %in% top_tissues) %>%
  ggplot(aes(x = noise_level, y = reorder(tissue, mean_accuracy), fill = mean_accuracy)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Accuracy heatmap by tissue and noise level",
    x = "Noise Level",
    y = "Tissue",
    fill = "Accuracy"
  ) +
  theme_minimal()

# 5. Overall Trend Chart - Display the Relationship between Noise and Average Accuracy
trend_plot <- ggplot(overall_accuracy, aes(x = noise_level, y = mean_accuracy, group = 1)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Overall accuracy trend with increasing noise",
    x = "Noise Level",
    y = "Mean Accuracy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ========== Combining charts and saving ==========
# Combine charts at the top left, top right, and bottom
top_row <- plot_grid(violin_plot, box_plot, ncol = 2, labels = c("A", "B"))
middle_row <- plot_grid(line_plot, trend_plot, ncol = 2, labels = c("C", "D"))
combined_plot <- plot_grid(top_row, middle_row, heatmap_plot, 
                           ncol = 1, 
                           labels = c("", "", "E"), 
                           rel_heights = c(1, 1, 1.2))

# Add overall title
title <- ggdraw() + 
  draw_label(
    "AICelltype Model Robustness with Random Noise Genes",
    fontface = "bold",
    size = 14,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(b = 10, t = 10, l = 0, r = 0)
  )

# Combining titles and charts
final_plot <- plot_grid(
  title, combined_plot,
  ncol = 1,
  rel_heights = c(0.05, 1)
)

# Save Chart
ggsave(paste0(outdir, "/AICelltype_del_noise_analysis.pdf"), final_plot, width = 12, height = 16, units = "in", dpi = 300)
ggsave(paste0(outdir, "/AICelltype_del_noise_analysis.png"), final_plot, width = 12, height = 16, units = "in", dpi = 300)


main_violin_plot <- ggplot(tissue_scores %>% filter(noise_level %in% c("0% Noise", "20% Noise", "40% Noise", "60% Noise")), 
                           aes(x = mean_accuracy, y = as.numeric(noise_level), group = noise_level)) +
  geom_violin(fill = "#1f78b4", alpha = 0.5, color = "grey50") +
  geom_point(color = "#2171b5", size = 3, alpha = 0.85,
             position = position_jitter(width = 0, height = 0.03)) +
  scale_y_reverse(breaks = c(4, 3, 2, 1),
                  labels = c("60% Noise", "40% Noise", "20% Noise", "0% Noise"),
                  limits = c(4.5, 0.5)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(
    x = "Prediction accuracy",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10), face = "bold")
  ) + ggtitle("")

# Save the original style chart
ggsave(paste0(outdir, "/AICelltype_del_robustness_detail_original.pdf"), main_violin_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave(paste0(outdir, "/AICelltype_del_robustness_detail_original.png"), main_violin_plot, width = 10, height = 6, units = "in", dpi = 300)


# ------------------------------------------------------------------------------------------------------------------------------------------- #
#temperature










# ------------------------------------------------------------------------------------------------------------------------------------------- #
# Scoring of each organization in the target model
model      <- "claude.3.5.sonnet.20240620_score"
df         <- df.reorder
df         <- df[c("dataset", "tissue", model)]
df         <- df[!is.na(df$tissue),]
dataset.df <- c()
tissue.df  <- c()
source.df  <- c()
num.df     <- c()
index.df   <- c()
Agreement  <- c()
Total      <- c()
local.X    <- c()
idx        <- 1
# Step by step to select subsets and construct a dataset
for(dataset.name in unique(df$dataset))
{
  df.dataset <- subset(df, dataset == dataset.name)
  for(tissue.name in unique(df.dataset$tissue))
  {
    tissue.dataset <- subset(df.dataset, tissue == tissue.name)
    tissue.dataset <- tissue.dataset[!is.na(tissue.dataset[model]),]
    source.1       <- nrow(subset(tissue.dataset, claude.3.5.sonnet.20240620_score == 1))
    source.0.5     <- nrow(subset(tissue.dataset, claude.3.5.sonnet.20240620_score == 0.5))
    source.0       <- nrow(subset(tissue.dataset, claude.3.5.sonnet.20240620_score == 0))
    dataset.df     <- c(dataset.df, c(dataset.name, dataset.name, dataset.name))
    tissue.df      <- c(tissue.df , c(tissue.name , NA          , NA          ))
    source.df      <- c(source.df , c("source.1"  , "source.0.5", "source.0"  ))
    Agreement      <- c(Agreement , c("Fully match", "Partially match", "Mismatch"))
    num.df         <- c(num.df    , c(source.1    , source.0.5  , source.0    ))
    local.X        <- c(local.X   , rep(((source.1 + source.0.5/2)/(source.1 + source.0.5 + source.0))    , time=3))
    Total          <- c(Total     , rep(((source.1 + source.0.5 *.5) / (source.1 + source.0.5 + source.0)), time=3))
    index.df       <- c(index.df  , c(idx         , idx         ,  idx        ))
    idx = idx + 1
  }
}
df.sorce           <- c()
df.sorce$index     <- 1:length(dataset.df)
df.sorce$idx       <- index.df
df.sorce$dataset   <- dataset.df
df.sorce$tissue    <- tissue.df
df.sorce$source    <- source.df
df.sorce$Agreement <- factor(Agreement, levels=c("Mismatch", "Partially match", "Fully match"))
df.sorce$local.X   <- local.X
df.sorce$Total     <- Total
df.sorce$num       <- num.df
df.sorce           <- as.data.frame(df.sorce)
df.sorced          <- df.sorce[order(df.sorce$Total, df.sorce$idx),]
df.sorced          <- as.data.frame(df.sorced)
index              = 1
point.X            <- c()
point.Y            <- c()
for(i in seq(from = 1, to = nrow(df.sorced), by = 3))
{
  df.sorced[i:(i+2),]$idx <- index
  point.X                 <- c(point.X, df.sorced[i,]$local.X)
  point.Y                 <- c(point.Y, index)
  index                   =  index + 1
}

head(df.sorce)

colors <- c("Azimuth"        = "#f4dbc1", 
            "coloncancer"    = "#f7c4c1", 
            "tabulasapiens " = "#e5c8e2", 
            "HCA"            = "#cecce3", 
            "lungcancer"     = "#c4e2ec", 
            "literature"     = "#bbd1bc", 
            "BCL"            = "#d9eec5",
            "HCL"            = "#cf8995",
            "MCA"            = "#dabe88")

p <- ggplot() + 
  geom_bar(data = df.sorced,  aes(x=idx ,y=num, fill=dataset, alpha=Agreement), stat="identity", width=0.9, position="fill")+
  theme_classic() + coord_flip() +
  labs(x = NULL, y = NULL) + 
  scale_y_continuous(breaks=NULL, labels=NULL,expand=expansion(mult = c(0, 0)))+
  scale_x_continuous(breaks=1:length(df.sorce[!is.na(df.sorce$tissue),"tissue"]), labels=df.sorce[!is.na(df.sorce$tissue),"tissue"],expand=expansion(mult = c(0, 0))) +
  geom_point(data = data.frame(X=point.Y, Y=point.X), aes(x=X, y=Y), color="#595959")+
  scale_fill_manual(values = colors)
ggsave(filename=paste0(outdir, "Model_performance_on_different_datasets.png"), plot =p, width=6, height=8, dpi=300)
ggsave(filename=paste0(outdir, "Model_performance_on_different_datasets.pdf"), plot =p, width=6, height=8, dpi=300)
write.table(df.sorced, paste0(outdir,"/Model_performance_on_different_datasets.tab"), sep="|" ,row.names = FALSE)
write.table(df.sorced, paste0(outdir,"/Model_performance_on_different_datasets.tsv"), sep="\t",row.names = FALSE)
# ------------------------------------------------------------------------------------------------------------------------------------------- #






