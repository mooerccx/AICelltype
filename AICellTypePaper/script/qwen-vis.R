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

dark_palette <- scale_fill_viridis(option = "plasma", direction = -1)

dpi <- 600
width <- 10
height <- 8

# Read data and data cleaning
data <- read.delim("Y:/ccx/AICelltype/20250528/AICelltype.anno.update.cl.final.tsv", sep = "\t")
# data <- data %>% mutate_all(~ifelse(. == "NA" | . == "0", NA, .))

# Extract model names (columns ending with _anno), exclude .1 duplicate columns
model_cols <- grep("_anno$", colnames(data), value = TRUE)
model_cols <- model_cols[!grepl("\\.1$", model_cols)]
model_names <- gsub("_anno$", "", model_cols)

cat("Total records:", nrow(data), "\n")
cat("Total models:", length(model_names), "\n")

validate_scores <- function(scores) {
  # First convert data to character, then to numeric, and suppress warnings (those that cannot be converted will become NA)
  scores_chr <- as.character(scores)
  valid_scores <- suppressWarnings(as.numeric(scores_chr))
  
  if(any(!is.na(valid_scores) & !(valid_scores %in% c(0, 0.5, 1)))) {
    warning("Non-standard score values found! Values should be 0, 0.5 or 1")
    print(table(valid_scores, useNA = "always"))
  }
  return(valid_scores)
}

# Filter valid models (at least 20% valid data)
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
    if(valid_percent > 0.2 || model == "qwen2.5.coder.1.5b.instruct") {
      valid_models <- c(valid_models, model)
    }
  }
}

tissues <- unique(data$tissue)
tissues <- tissues[!is.na(tissues) & tissues != ""]
cat("Number of tissue types:", length(tissues), "\n")
cat("Tissue types:", paste(tissues, collapse = ", "), "\n")

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
          warning(paste("Model", model, "tissue", tissue_type, "rate sum is not 1:", total_rate))
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

cat("Total result records:", nrow(results), "\n")
cat("Missing value summary:\n")
print(colSums(is.na(results)))

results_filtered <- results %>% 
  group_by(model) %>% 
  filter(sum(valid_predictions) >= 10 | model == "qwen2.5.coder.1.5b.instruct") %>% 
  ungroup()
cat("Filtered records:", nrow(results_filtered), "from", n_distinct(results_filtered$model), "models\n")

# Optimized model renaming function (can automatically handle new models)
rename_model <- function(model_name) {
  patterns <- list(
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
    "o1\\.mini" = "O1 Mini"
  )
  for(pattern in names(patterns)) {
    if(grepl(pattern, model_name, perl = TRUE)) {
      return(patterns[[pattern]])
    }
  }
  return(model_name)
}
unique_models <- unique(results_filtered$model)
for(i in seq_along(unique_models)) {
  old_name <- unique_models[i]
  new_name <- rename_model(old_name)
  results_filtered$model[results_filtered$model == old_name] <- new_name
}

results_filtered$tissue <- str_trim(results_filtered$tissue)

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

results_filtered <- results_filtered %>% 
  left_join(model_overall_scores %>% 
              mutate(model_rank = row_number()) %>% 
              select(model, model_rank, overall_mean_score),
            by = "model")

heatmap_data <- results_filtered %>% 
  select(model, tissue, mean_score, overall_mean_score, model_rank) %>% 
  pivot_wider(names_from = tissue, values_from = mean_score) %>% 
  arrange(model_rank)
heatmap_matrix <- as.matrix(heatmap_data %>% select(-model, -model_rank, -overall_mean_score))
rownames(heatmap_matrix) <- heatmap_data$model

annotation_row <- data.frame(
  OverallScore = heatmap_data$overall_mean_score
)
rownames(annotation_row) <- heatmap_data$model
ann_colors = list(
  OverallScore = colorRampPalette(c("#FDE0DD", "#9ddaf2"))(100)  # Deep contrast color scheme
)
heatmap_matrix[is.na(heatmap_matrix)] <- 0

# Reconstruct heatmap code - first organize clustering, then model ranking
library(tidyverse)
qwen_tissue_scores <- results_filtered %>% 
  filter(model %in% c("Qwen Max 0125", "Qwen 2.5 72B", "Qwen 2.5 32B", 
                      "Qwen 2.5 14B", "Qwen 2.5 7B", "Qwen 2.5 3B", 
                      "Qwen 2.5 Coder 1.5B")) %>%
  mutate(
    # Extract parameters like "1.5B" or "72B" (supporting decimals)
    param_size = str_extract(model, "[0-9]+\\.?[0-9]*B"),
    param_numeric = as.numeric(str_remove(param_size, "B"))
  ) %>% 
  # For cases when parameter cannot be extracted, assign manually
  mutate(
    param_numeric = if_else(is.na(param_numeric) & str_detect(model, fixed("Qwen 2.5 Coder 1.5B")), 1.5, param_numeric),
    param_numeric = if_else(is.na(param_numeric) & str_detect(model, fixed("Qwen Max 0125")), 110, param_numeric)
  ) %>% 
  # Convert the numeric parameter value to a factor for grouping
  mutate(param_factor = factor(param_numeric))

# Plot the violin chart with gray border and overlay jitter points
p_qwen_tissue_violin <- ggplot(qwen_tissue_scores, aes(x = param_factor, y = mean_score, fill = param_factor)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "gray", adjust = 1.5, n = 2000) +
  geom_jitter(aes(color = tissue), width = 0.15, size = 3, alpha = 0.9) +
  labs(x = "Parameter Size (Billion)",
       y = "Tissue Accuracy",
       title = "Distribution of Tissue Accuracy for Different Qwen Models") +
  theme_minimal(base_size = 20) +  # Increase base font size
  theme(
    axis.title = element_text(size = 22,),  # Axis title font size
    axis.text = element_text(size = 12),    # Axis tick label font size
    axis.text.x = element_text(size = 22),  # X-axis tick labels
    axis.text.y = element_text(size = 22),  # Y-axis tick labels
    plot.title = element_text(size = 18, ), # Title font size
    legend.title = element_text(size = 12, ), # Legend title
    legend.text = element_text(size = 12),  # Legend text
    legend.position = "right"
  )

print(p_qwen_tissue_violin)

# Save the plot as high-dpi PDF to avoid rasterization artifacts
ggsave(
  filename = "y:/ccx/AICelltype/Script/qwen_tissue_violin1.pdf",
  plot     = p_qwen_tissue_violin,
  width    = 12,
  height   = 7,
  dpi      = 300,
  limitsize= FALSE
)

# Directly check filtered results
cat("Check filtering results:\n")
qwen_models_check <- results_filtered %>% 
  filter(model %in% c("Qwen Max 0125", "Qwen 2.5 72B", "Qwen 2.5 32B", 
                      "Qwen 2.5 14B", "Qwen 2.5 7B", "Qwen 2.5 3B", 
                      "Qwen 2.5 Coder 1.5B"))

cat("Found models:\n")
print(unique(qwen_models_check$model))
cat("Record count for each model:\n")
print(table(qwen_models_check$model))

# If Qwen 2.5 Coder 1.5B exists, check parameter extraction
if("Qwen 2.5 Coder 1.5B" %in% qwen_models_check$model) {
  cat("Qwen 2.5 Coder 1.5B exists, checking parameter extraction:\n")
  coder_data <- qwen_models_check %>% filter(model == "Qwen 2.5 Coder 1.5B") %>% slice(1)
  print(coder_data[c("model", "param_size", "param_numeric", "param_factor")])
}
