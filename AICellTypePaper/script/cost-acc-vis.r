# Load required packages
library(ggplot2)
library(readr)
library(ggrepel)
library(rPref)
library(grid) # For unit settings

# Read data, handle #N/A as NA
data <- read_tsv("Y:/ccx/AICelltype/20250528/model_match_stats.tsv", 
                 na = c("", "NA", "#N/A"))

# Ensure 'spend' column is numeric and remove rows with NA
data <- data[!is.na(data$spend), ]
data$spend <- as.numeric(data$spend)

# Remove manual model
data <- data[data$model != "manual", ]

# Select top 20 models by weighted_accuracy
top_n <- 20
data <- data[order(-data$weighted_accuracy), ][1:top_n, ]

# Calculate cost per cell
data$spend_per_cell <- data$spend / 1130

# Get Pareto front (using cost per cell)
pareto_front <- psel(data, high(weighted_accuracy) * low(spend_per_cell), top = nrow(data))

# Model renaming function for display
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
    "gemini\\.2\\.5\\.flash\\.preview\\.04\\.17\\.nothink$"       = "gemini-2.5-flash-p 04.17 nothink",
    "gemini\\.2\\.5\\.flash\\.preview\\.04\\.17$"                 = "gemini-2.5-flash-p 04.17",
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

# 为 pareto_front 数据添加重命名的模型名称
pareto_front$model_display <- sapply(pareto_front$model, rename_model)

# 为 pareto_front 数据添加 company_group 列
pareto_front$company_group <- sapply(pareto_front$model, map_model_to_company)

# 标记重要模型 (使用原始模型名称进行匹配)
pareto_front$is_important <- ifelse(
  grepl("claude\\.3\\.5\\.sonnet\\.20240620", pareto_front$model, ignore.case = TRUE) | 
  grepl("^gpt\\.4$", pareto_front$model, ignore.case = TRUE),
  TRUE, FALSE
)

# 定义公司/技术组调色板
company_group_palette <- c(
  "OpenAI" = "#69b3cb",  # 更深的蓝色
  "Google" = "#e8a76f",  # 更深的橙色
  "Claude" = "#e47470",  # 更深的红色
  "Llama"  = "#8bc560",  # 更深的绿色
  "Deepseek"   = "#8986b4",  # 更深的紫色
  "Other"  = "#c38bbd"   # 更深的粉色
)

# 作图 - 使用单细胞费用
p <- ggplot(pareto_front, aes(x = spend_per_cell, y = weighted_accuracy)) +
  geom_point(aes(color = company_group), size = 6, alpha = 0.9)

# 为每个非重要模型单独添加标签
non_important_models <- subset(pareto_front, !is_important)
for(i in 1:nrow(non_important_models)) {
  p <- p + geom_text_repel(
    data = non_important_models[i,],
    aes(label = model_display),  # 使用重命名后的标签
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

# 为每个重要模型单独添加标签
important_models <- subset(pareto_front, is_important)
for(i in 1:nrow(important_models)) {
  p <- p + geom_text_repel(
    data = important_models[i,],
    aes(label = model_display),  # 使用重命名后的标签
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

p <- p + scale_color_manual(values = company_group_palette, name = "Provider") +
  labs(
    # title = "Model Accuracy vs. Cost",
    x = "Cost per Cell (USD)",
    y = "Weighted Accuracy"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    axis.line = element_line(color = "gray80", size = 0.6),  # 轴线更粗且更淡
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

print(p)

# 保存图表方法1: 使用Cairo PDF
if(requireNamespace("Cairo", quietly = TRUE)) {
  Cairo::CairoPDF("Y:/ccx/AICelltype/20250428/pareto_cost_accuracy_per_cell.pdf", 
                width = 8, height = 6, pointsize = 12)
  print(p)
  dev.off()
} else {
  # 如果没有Cairo包，使用标准PDF设备但增加DPI
  ggsave("Y:/ccx/AICelltype/20250428/pareto_cost_accuracy_per_cell.pdf", 
         plot = p, width = 8, height = 6, dpi = 600)
}

# 保存图表方法2: 保存为SVG (通常保持更好的文本独立性)
ggsave("Y:/ccx/AICelltype/20250428/pareto_cost_accuracy_per_cell.svg", 
       plot = p, width = 8, height = 6)

# 保存PNG格式(可选)
# ggsave("Y:/ccx/AICelltype/20250428/pareto_cost_accuracy_per_cell.png", 
#        plot = p, width = 8, height = 6, dpi = 300)