# ==============================================================================
# 01_data_preprocessing.R
# 数据预处理脚本 - 合并患病率和发病率数据的清洗与标准化
# ==============================================================================
#
# Description:
#   清洗和标准化PsA流行病学研究的原始数据
#   - 统一国家名称
#   - 创建样本量分类变量
#   - 处理缺失值
#
# Input:
#   - data/input-prevalence.csv  : 患病率原始数据
#   - data/input-incidence.csv : 发病率原始数据
#   - data/studies.csv          : 研究信息数据
#
# Output:
#   - output/All_PsA-pre-input.Rdata   : 清洗后的患病率数据
#   - output/All_PsA_Inc-input.Rdata   : 清洗后的发病率数据
#   - output/spdf_fortified.Rdata      : 地图数据
#
# Required R packages:
#   - dplyr
#   - rgdal
#   - broom
#   - maptools
#
# ==============================================================================

# 加载所需的R包
library(dplyr)
library(rgdal)
library(broom)
library(maptools)

# 设置工作目录为脚本所在目录
setwd(dirname(parent.frame(2)$ofile))

# 定义数据路径
data_dir <- "data"
output_dir <- "output"

# ==============================================================================
# Part 1: 患病率数据预处理
# ==============================================================================

cat("=== Processing Prevalence Data ===\n")

# 读取患病率数据
prevalence_data <- read.csv(file.path(data_dir, "input-prevalence.csv"),
                            header = TRUE, stringsAsFactors = FALSE)

# 查看数据结构
str(prevalence_data)
head(prevalence_data)

# 统一列名（处理空格问题）
colnames(prevalence_data) <- gsub(" ", ".", colnames(prevalence_data))
colnames(prevalence_data) <- gsub("\\(", "", colnames(prevalence_data))
colnames(prevalence_data) <- gsub("\\)", "", colnames(prevalence_data))

# 查看处理后的列名
colnames(prevalence_data)

# 确保数值列为数值类型
prevalence_data$population <- as.numeric(prevalence_data$population)
prevalence_data$total.PsA <- as.numeric(prevalence_data$total.PsA)

# 创建样本量分类变量
prevalence_data$sample_size <- ifelse(prevalence_data$population > 1000000,
                                       ">100W", "<=100W")
table(prevalence_data$sample_size)

# 统一国家名称（与地图数据匹配）
prevalence_data$country <- recode(prevalence_data$country,
                                   "USA" = "United States",
                                   "China-Beijing" = "China",
                                   "China-Taiwan" = "China",
                                   "Czech Repunlic" = "Czech Republic",
                                   "Canada-Ontario" = "Canada",
                                   "UK" = "United Kingdom")

# 查看各变量的分布
table(prevalence_data$country)
table(prevalence_data$Region)
table(prevalence_data$Super.region)
table(prevalence_data$study_period)
table(prevalence_data$Definition)
table(prevalence_data$age_group)

# 保存预处理后的患病率数据
prevalence_data_use <- prevalence_data
save(prevalence_data_use, file = file.path(output_dir, "All_PsA-pre-input.Rdata"))
cat("Prevalence data saved to output/All_PsA-pre-input.Rdata\n")

# ==============================================================================
# Part 2: 发病率数据预处理
# ==============================================================================

cat("\n=== Processing Incidence Data ===\n")

# 读取发病率数据
incidence_data <- read.csv(file.path(data_dir, "input-incidence.csv"),
                           header = TRUE, stringsAsFactors = FALSE)

# 查看数据结构
str(incidence_data)
head(incidence_data)

# 统一列名
colnames(incidence_data) <- gsub(" ", ".", colnames(incidence_data))
colnames(incidence_data) <- gsub("\\(", "", colnames(incidence_data))
colnames(incidence_data) <- gsub("\\)", "", colnames(incidence_data))

# 查看处理后的列名
colnames(incidence_data)

# 确保数值列为数值类型
incidence_data$person_years <- as.numeric(incidence_data$person_years)
incidence_data$Incident.PSA <- as.numeric(incidence_data$Incident.PSA)

# 创建样本量分类变量
incidence_data$sample_size <- ifelse(incidence_data$person_years > 1000000,
                                      ">100W", "<=100W")
table(incidence_data$sample_size)

# 统一国家名称
incidence_data$country <- recode(incidence_data$country,
                                 "USA" = "United States",
                                 "China-Beijing" = "China",
                                 "China-Taiwan" = "China",
                                 "Czech Repunlic" = "Czech Republic",
                                 "Canada-Ontario" = "Canada",
                                 "UK" = "United Kingdom",
                                 "USA(Olmsted County,MN)" = "United States")

# 查看各变量的分布
table(incidence_data$country)
table(incidence_data$Region)
table(incidence_data$Super.region)
table(incidence_data$study_period)
table(incidence_data$Definition)
table(incidence_data$age_group)

# 保存预处理后的发病率数据
incidence_data_use <- incidence_data
save(incidence_data_use, file = file.path(output_dir, "All_PsA_Inc-input.Rdata"))
cat("Incidence data saved to output/All_PsA_Inc-input.Rdata\n")

# ==============================================================================
# Part 3: 地图数据预处理
# ==============================================================================

cat("\n=== Processing Map Data ===\n")

# 读取地图形状文件
my_spdf <- readOGR(
  dsn = file.path(data_dir, "world_shape_file"),
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)

# 允许地图数据处理
gpclibPermit()

# 将形状数据转换为ggplot2需要的格式
spdf_fortified <- tidy(my_spdf, region = "NAME")

# 保存地图数据
save(spdf_fortified, file = file.path(output_dir, "spdf_fortified.Rdata"))
cat("Map data saved to output/spdf_fortified.Rdata\n")

# 获取所有国家名称列表
country_list <- names(table(spdf_fortified$id))
save(country_list, file = file.path(output_dir, "all_country.Rdata"))
cat("Country list saved to output/all_country.Rdata\n")

# ==============================================================================
# Part 4: 研究信息数据预处理
# ==============================================================================

cat("\n=== Processing Studies Data ===\n")

# 读取研究信息
studies_data <- read.csv(file.path(data_dir, "studies.csv"),
                         header = TRUE, stringsAsFactors = FALSE)

# 统一国家名称
studies_data$country <- recode(studies_data$country,
                               "USA" = "United States",
                               "China-Beijing" = "China",
                               "China-Taiwan" = "China",
                               "Czech Repunlic" = "Czech Republic",
                               "Canada-Ontario" = "Canada",
                               "UK" = "United Kingdom")

# 统计每个国家的研究数量
study_counts <- table(studies_data$country)
study_df <- data.frame(country = names(study_counts),
                       number = as.integer(study_counts))

# 添加台湾（如果需要单独显示）
if ("China" %in% study_df$country) {
  taiwan_row <- study_df[study_df$country == "China", ]
  taiwan_row$country <- "Taiwan"
  study_df <- rbind(study_df, taiwan_row)
}

# 保存研究计数数据
save(study_df, file = file.path(output_dir, "study_counts.Rdata"))
cat("Study counts saved to output/study_counts.Rdata\n")

cat("\n=== Data Preprocessing Complete ===\n")
cat("Files generated:\n")
cat("  - output/All_PsA-pre-input.Rdata\n")
cat("  - output/All_PsA_Inc-input.Rdata\n")
cat("  - output/spdf_fortified.Rdata\n")
cat("  - output/all_country.Rdata\n")
cat("  - output/study_counts.Rdata\n")