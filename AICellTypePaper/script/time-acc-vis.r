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
library(ggrepel)

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

data <- read.delim("Y:/ccx/AICelltype/20250528/AICelltype.anno.update.cl.final.tsv", sep = "\t")

model_cols <- grep("_anno$", colnames(data), value = TRUE)
model_cols <- model_cols[!grepl("\\.1$", model_cols)]
model_names <- gsub("_anno$", "", model_cols)

cat("Total records:", nrow(data), "\n")
cat("Total models:", length(model_names), "\n")

validate_scores <- function(scores) {
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
    "deepseek\\.ai.DeepSeek\\.V3\\.0324$"                         = "DeepSeek V3 (0324)",
    "claude\\.3\\.5\\.haiku\\.\\d+"                               = "Claude 3.5 Haiku",
    "claude\\.3\\.5\\.sonnet\\.20241022$"                         = "Claude 3.5 Sonnet (10/22)",
    "claude\\.3\\.5\\.sonnet\\.20240620$"                         = "Claude 3.5 Sonnet (06/20)",
    "claude\\.3\\.5\\.sonnet\\.\\d+"                              = "Claude 3.5 Sonnet",
    "claude\\.3\\.haiku\\.\\d+"                                   = "Claude 3 Haiku",
    "claude\\.3\\.opus\\.\\d+"                                    = "Claude 3 Opus",
    "gpt\\.4o\\.2024\\.11\\.20$"                                  = "GPT-4o (11/20)",
    "gpt\\.4o(?!\\.mini)$"                                        = "GPT-4o",
    "gpt\\.4o\\.mini$"                                            = "GPT-4o Mini",
    "gpt\\.4\\.turbo$"                                            = "GPT-4 Turbo",
    "gpt\\.4\\.0125\\.preview$"                                   = "GPT-4 0125 Preview",
    "gpt\\.4\\.1106\\.preview$"                                   = "GPT-4 1106 Preview",
    "gpt\\.4\\.1\\.mini$"                                         = "GPT-4.1 Mini",
    "gpt\\.4\\.1\\.nano$"                                         = "GPT-4.1 Nano",
    "gpt\\.4\\.1$"                                                = "GPT-4.1",
    "gpt\\.4\\.0613$"                                             = "GPT-4 0613",
    "gpt\\.4$"                                                    = "GPT-4",
    "gemini\\.2\\.5\\.flash\\.preview\\.04\\.17\\.nothink$"       = "Gemini-2.5-flash-p 04.17 nothink",
    "gemini\\.2\\.5\\.flash\\.preview\\.04\\.17$"                 = "Gemini-2.5-flash-p 04.17",
    "gpt\\.3\\.5\\.turbo\\.0125$"                                 = "GPT-3.5 Turbo 0125",
    "gpt\\.3\\.5\\.turbo\\.1106$"                                 = "GPT-3.5 Turbo 1106",
    "gpt\\.3\\.5\\.turbo\\.0613$"                                 = "GPT-3.5 Turbo 0613",
    "gpt\\.3\\.5\\.turbo$"                                        = "GPT-3.5 Turbo",
    "Doubao\\.1\\.5\\.pro\\.256k$"                                = "Doubao 1.5 Pro 256K",
    "Doubao\\.1\\.5\\.pro\\.32k$"                                 = "Doubao 1.5 Pro 32K",
    "Doubao\\.1\\.5\\.lite\\.32k$"                                = "Doubao 1.5 Lite 32K",
    "Doubao\\.pro\\.256k$"                                        = "Doubao Pro 256K",
    "Doubao\\.pro\\.32k$"                                         = "Doubao Pro 32K",
    "Doubao\\.pro\\.4k$"                                          = "Doubao Pro 4K",
    "Doubao\\.lite\\.128k$"                                       = "Doubao Lite 128K",
    "Doubao\\.lite\\.32k$"                                        = "Doubao Lite 32K",
    "Doubao\\.lite\\.4k$"                                         = "Doubao Lite 4K",
    "aihubmix\\.Mistral\\.Large\\.2411$"                          = "Mistral Large",
    "aihubmix\\.Llama\\.3\\.3\\.70B\\.Instruct$"                  = "Llama 3.3 70B",
    "aihubmix\\.Llama\\.3\\.1\\.8B\\.Instruct$"                   = "Llama 3.1 8B",
    "aihubmix\\.Llama\\.3\\.1\\.70B\\.Instruct$"                  = "Llama 3.1 70B",
    "aihubmix\\.Llama\\.3\\.1\\.405B\\.Instruct$"                 = "Llama 3.1 405B",
    "llama3\\.8b\\.8192$"                                         = "Llama 3 8B",
    "llama3\\.70b\\.8192$"                                        = "Llama 3 70B",
    "llama\\.3\\.2\\.1b\\.preview$"                               = "Llama 3.2 1B",
    "llama\\.3\\.2\\.3b\\.preview$"                               = "Llama 3.2 3B",
    "llama\\.3\\.2\\.11b\\.vision\\.preview$"                     = "Llama 3.2 11B Vision",
    "llama\\.3\\.2\\.90b\\.vision\\.preview$"                     = "Llama 3.2 90B Vision",
    "DeepSeek\\.V3$"                                              = "DeepSeek V3",
    "DeepSeek\\.R1$"                                              = "DeepSeek R1",
    "aihubmix\\.DeepSeek\\.R1$"                                   = "DeepSeek R1 (Hub)",
    "deepseek\\.r1\\.distill\\.llama\\.70b$"                      = "DeepSeek R1 Distill Llama 70B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.1\\.5B$"    = "DeepSeek R1 Distill Qwen 1.5B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.32B$"       = "DeepSeek R1 Distill Qwen 32B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.14B$"       = "DeepSeek R1 Distill Qwen 14B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Qwen\\.7B$"        = "DeepSeek R1 Distill Qwen 7B",
    "deepseek\\.ai\\.DeepSeek\\.R1\\.Distill\\.Llama\\.8B$"       = "DeepSeek R1 Distill Llama 8B",
    "chutesai\\.Mistral\\.Small\\.3\\.1\\.24B\\.Instruct\\.2503$" = "Mistral Small-3.1 24B Ins 2503",
    "aihub\\.Phi\\.4$"                                            = "Phi-4",
    "gemini\\.2\\.0\\.flash\\.thinking\\.exp\\.01\\.21$"          = "Gemini 2.0 Flash Thinking",
    "gemini\\.2\\.0\\.flash\\.lite\\.preview\\.02\\.05$"          = "Gemini 2.0 Flash Lite",
    "gemini\\.2\\.0\\.flash\\.search$"                            = "Gemini 2.0 Flash Search",
    "gemini\\.2\\.0\\.flash$"                                     = "Gemini 2.0 Flash",
    "Qwen\\.QVQ\\.72B\\.Preview$"                                 = "Qwen QVQ 72B",
    "qwen\\.max\\.0125$"                                          = "Qwen Max 0125",
    "Qwen\\.Qwen2\\.5\\.72B\\.Instruct$"                          = "Qwen 2.5 72B",
    "Qwen\\.Qwen2\\.5\\.32B\\.Instruct$"                          = "Qwen 2.5 32B",
    "qwen2\\.5\\.14b\\.instruct$"                                 = "Qwen 2.5 14B",
    "qwen2\\.5\\.7b\\.instruct$"                                  = "Qwen 2.5 7B",
    "qwen2\\.5\\.3b\\.instruct$"                                  = "Qwen 2.5 3B",
    "qwen2\\.5\\.coder\\.1\\.5b\\.instruct$"                      = "Qwen 2.5 Coder 1.5B",
    "o1\\.mini$"                                                  = "O1 Mini",
    "chutesai\\.Llama\\.4\\.Maverick\\.17B\\.128E\\.Instruct\\.FP8$" = "Llama 4 Maverick 17B 128E FP8",
    "chutesai\\.Llama\\.4\\.Scout\\.17B\\.16E\\.Instruct$"           = "Llama 4 Scout 17B 16E"
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

# Add generation speed analysis code - using the style of the reference code
library(grid)  # Add dependency for grid, used for unit settings

# Extract time columns
time_cols <- grep("_time$", colnames(data), value = TRUE)
time_cols <- time_cols[!grepl("\\.1$", time_cols)]
time_model_names <- gsub("_time$", "", time_cols)

cat("Number of time columns found:", length(time_cols), "\n")

# Filter valid models with time data
valid_time_models <- c()
for(model in time_model_names) {
  time_col <- paste0(model, "_time")
  if(time_col %in% colnames(data)) {
    # Check validity of time data
    time_data <- data[[time_col]]
    time_data <- suppressWarnings(as.numeric(as.character(time_data)))
    valid_time_count <- sum(!is.na(time_data) & time_data > 0)
    if(valid_time_count > 5) {  # Lower threshold to at least 5 valid time records
      valid_time_models <- c(valid_time_models, model)
    }
  }
}

cat("Number of models with valid time data:", length(valid_time_models), "\n")

# Collect time data and calculate median time for each model
time_summary_data <- data.frame()
for(model in valid_time_models) {
  time_col <- paste0(model, "_time")
  if(time_col %in% colnames(data)) {
    time_values <- suppressWarnings(as.numeric(as.character(data[[time_col]])))
    valid_times <- time_values[!is.na(time_values) & time_values > 0]
    
    if(length(valid_times) > 0) {
      time_summary_data <- rbind(time_summary_data, data.frame(
        model = model,
        median_time = median(valid_times),
        mean_time = mean(valid_times),
        min_time = min(valid_times),
        max_time = max(valid_times),
        count = length(valid_times),
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Apply model renaming
if(nrow(time_summary_data) > 0) {
  time_summary_data$model_display <- sapply(time_summary_data$model, rename_model)
  
  # Merge with accuracy data
  speed_accuracy_data <- merge(
    time_summary_data,
    model_overall_scores,
    by.x = "model_display",
    by.y = "model",
    all.x = TRUE
  )
  
  # Filter out models without accuracy data, keep only top 20 models
  speed_accuracy_data <- speed_accuracy_data[!is.na(speed_accuracy_data$overall_mean_score), ]
  speed_accuracy_data <- speed_accuracy_data[order(-speed_accuracy_data$overall_mean_score), ]
  
  # Keep only top 20 models
  top_n <- min(20, nrow(speed_accuracy_data))
  speed_accuracy_data <- speed_accuracy_data[1:top_n, ]
  
  cat("Number of models used for visualization:", nrow(speed_accuracy_data), "\n")
  
  # Define company/tech group mapping function
  map_model_to_company <- function(model_name) {
    if(grepl("GPT|gpt|OpenAI|o1|o3|o4", model_name, ignore.case = TRUE)) {
      return("OpenAI")
    } else if(grepl("Claude|claude|Anthropic", model_name, ignore.case = TRUE)) {
      return("Claude")
    } else if(grepl("Gemini|gemini|Google", model_name, ignore.case = TRUE)) {
      return("Google")
    } else if(grepl("Llama|llama|Meta", model_name, ignore.case = TRUE)) {
      return("Llama")
    } else if(grepl("DeepSeek|deepseek", model_name, ignore.case = TRUE)) {
      return("Deepseek")
    } else {
      return("Other")
    }
  }
  
  # Add company group to data
  speed_accuracy_data$company_group <- sapply(speed_accuracy_data$model_display, map_model_to_company)
  
  # Mark important models
  speed_accuracy_data$is_important <- ifelse(
    grepl("Claude 3.5 Sonnet|GPT-4o|DeepSeek V3|Qwen 2.5 72B", speed_accuracy_data$model_display, ignore.case = TRUE),
    TRUE, FALSE
  )
  
  # Define company/tech group palette
  company_group_palette <- c(
    "OpenAI" = "#69b3cb",      # Deep blue
    "Google" = "#e8a76f",      # Deep orange
    "Claude" = "#e47470",      # Deep red
    "Llama"  = "#8bc560",      # Deep green
    "Deepseek" = "#8986b4",    # Deep purple
    "Other"  = "#c38bbd"       # Deep pink
  )
  
  # Create accuracy vs speed scatter plot
  p_accuracy_vs_speed <- ggplot(speed_accuracy_data, aes(x = median_time, y = overall_mean_score)) +
    geom_point(aes(color = company_group), size = 6, alpha = 0.9)
  
  # Add labels for non-important models
  non_important_models <- subset(speed_accuracy_data, !is_important)
  if(nrow(non_important_models) > 0) {
    for(i in 1:nrow(non_important_models)) {
      p_accuracy_vs_speed <- p_accuracy_vs_speed + geom_text_repel(
        data = non_important_models[i,],
        aes(label = model_display),
        color = "gray20",
        size = 4,
        box.padding = 0.5,
        point.padding = 0.3,
        min.segment.length = 0,
        segment.size = 0.2,
        segment.color = "gray80",
        force = 2,
        force_pull = 0,
        max.overlaps = 15,
        direction = "both"
      )
    }
  }
  
  # Add labels for important models
  important_models <- subset(speed_accuracy_data, is_important)
  if(nrow(important_models) > 0) {
    for(i in 1:nrow(important_models)) {
      p_accuracy_vs_speed <- p_accuracy_vs_speed + geom_text_repel(
        data = important_models[i,],
        aes(label = model_display),
        color = "black",
        size = 4,
        box.padding = 0.5,
        point.padding = 0.3,
        min.segment.length = 0,
        segment.size = 0.2,
        segment.color = "gray80",
        force = 2,
        direction = "both"
      )
    }
  }
  
  p_accuracy_vs_speed <- p_accuracy_vs_speed + 
    scale_color_manual(values = company_group_palette, name = "Provider") +
    scale_x_log10(
      breaks = c(0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000),
      labels = c("0.1s", "0.3s", "1s", "3s", "10s", "30s", "100s", "300s", "1000s"),
      minor_breaks = c(0.2, 0.5, 2, 5, 20, 50, 200, 500)
    ) +
    scale_y_continuous(
      limits = c(0.65, 0.76),
      breaks = seq(0.65, 0.75, by = 0.02),
      minor_breaks = seq(0.65, 0.76, by = 0.01)
    ) +
    labs(
      x = "Generation Time (log scale)",
      y = "Weighted Accuracy",
      title = "Model Accuracy vs. Generation Speed"
    ) +
    theme_minimal(base_family = "Helvetica", base_size = 12) +
    theme(
      panel.grid.minor = element_line(color = "gray97", size = 0.3),
      panel.grid.major = element_line(color = "gray90", size = 0.5),
      axis.line = element_line(color = "gray80", size = 0.6),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 14, color = "gray30"),
      axis.title = element_text(size = 16, color = "gray20"),
      plot.title = element_text(size = 14, hjust = 0, face = "plain", margin = margin(b = 15)),
      legend.position = "bottom",
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  print(p_accuracy_vs_speed)
  
  # Save plot method 1: Use Cairo PDF
  if(requireNamespace("Cairo", quietly = TRUE)) {
    Cairo::CairoPDF("y:/ccx/AICelltype/Script/accuracy_vs_speed_tradeoff.pdf", 
                  width = 8, height = 6, pointsize = 12)
    print(p_accuracy_vs_speed)
    dev.off()
  } else {
    # If Cairo is not available, use standard PDF device with higher DPI
    ggsave("y:/ccx/AICelltype/Script/accuracy_vs_speed_tradeoff.pdf", 
           plot = p_accuracy_vs_speed, width = 8, height = 6, dpi = 600)
  }
  
  # Save plot method 2: Save as SVG
  ggsave("y:/ccx/AICelltype/Script/accuracy_vs_speed_tradeoff.svg", 
         plot = p_accuracy_vs_speed, width = 8, height = 6)
  
  # Calculate statistical summary
  cat("\nGeneration speed statistics summary (sorted by median):\n")
  speed_summary_final <- speed_accuracy_data[order(speed_accuracy_data$median_time), 
                                           c("model_display", "median_time", "mean_time", "overall_mean_score", "count")]
  print(speed_summary_final)
  
} else {
  cat("Warning: No valid time data found\n")
}
write_tsv(speed_accuracy_data, "Y:/ccx/AICelltype/Script/speed_accuracy_data.tsv")
