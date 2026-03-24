# ==============================================================================
# 04_prevalence_prediction.R
# 患病率预测脚本 - 生成全球、区域和国家级别预测
# ==============================================================================
#
# Description:
#   使用拟合的贝叶斯模型进行患病率预测
#   生成三个层级的预测：
#   1. 全球级别 (Global)
#   2. 超区域级别 (Super-region)
#   3. 区域级别 (Region)
#   4. 国家级别 (Country)
#
# Input:
#   - output/prev_model-ALL.Rdata     : 拟合的患病率模型
#   - output/All_PsA-pre-input.Rdata  : 原始数据（用于提取国家列表）
#   - output/all_country.Rdata        : 国家列表
#
# Output:
#   - output/Predict/Global_estimate-all.csv       : 全球预测结果
#   - output/Predict/Super_estimate-all.csv       : 超区域预测结果
#   - output/Predict/Region_estimate-all.csv       : 区域预测结果
#   - output/Predict/Country_estimate-pre.csv      : 国家预测结果
#
# Required R packages:
#   - dplyr
#   - brms
#   - bayesplot
#
# ==============================================================================

# 加载所需的R包
library(dplyr)
library(brms)
library(bayesplot)

# 设置随机种子以确保可重复性
set.seed(123)

# 设置工作目录
setwd(dirname(parent.frame(2)$ofile))

# 定义路径
output_dir <- "output"

# 创建预测输出目录
predict_dir <- file.path(output_dir, "Predict")
if (!dir.exists(predict_dir)) {
  dir.create(predict_dir)
}

# ==============================================================================
# 加载模型和数据
# ==============================================================================

cat("=== Loading Prevalence Model ===\n")

# 加载模型
load(file.path(output_dir, "prev_model-ALL.Rdata"))

# 加载原始数据以获取国家列表
load(file.path(output_dir, "All_PsA-pre-input.Rdata"))

# 加载国家列表
load(file.path(output_dir, "all_country.Rdata"))

# 查看模型公式
cat("\nModel formula:\n")
print(prev_model$formula)

# ==============================================================================
# Part 1: 全球级别预测
# ==============================================================================

cat("\n=== Generating Global Predictions ===\n")

# 创建预测数据框 - 全球不同年龄组
new_data_global <- data.frame(
  country = rep("Global", 3),
  Region = rep("Global", 3),
  Super.region = rep("Global", 3),
  age_group = c("ALL", "Adult", "Child"),
  sample_size = rep(">100W", 3),
  study_period = rep("ALL", 3),
  Definition = rep("CASPAR", 3),
  population = rep(1000000, 3)
)

# 进行预测
global_pred <- predict(prev_model, newdata = new_data_global, allow_new_levels = TRUE)
global_est <- posterior_interval(global_pred, prob = 0.95)

# 计算患病率（每万人）
new_data_global$ave <- global_pred[, 1] / 10000
new_data_global$lower <- global_est[, 1] / 10000
new_data_global$upper <- global_est[, 2] / 10000

# 保存全球预测
write.csv(new_data_global,
          file = file.path(predict_dir, "Global_estimate-all.csv"),
          row.names = FALSE)
cat("Global predictions saved\n")

# ==============================================================================
# Part 2: 超区域级别预测
# ==============================================================================

cat("\n=== Generating Super-region Predictions ===\n")

# 定义所有超区域
super_regions <- c(
  "Central Europe, eastern Europe, and central Asia",
  "High income",
  "Latin America and Caribbean",
  "North Africa and Middle East",
  "South Asia",
  "South East Asia, east Asia, and Oceania",
  "Sub-Saharan Africa"
)

new_data_super <- data.frame(
  country = rep("Global", length(super_regions)),
  Region = rep("Global", length(super_regions)),
  Super.region = super_regions,
  age_group = rep("ALL", length(super_regions)),
  sample_size = rep(">100W", length(super_regions)),
  study_period = rep("ALL", length(super_regions)),
  Definition = rep("CASPAR", length(super_regions)),
  population = rep(1000000, length(super_regions))
)

# 进行预测
super_pred <- predict(prev_model, newdata = new_data_super, allow_new_levels = TRUE)
super_est <- posterior_interval(super_pred, prob = 0.95)

new_data_super$ave <- super_pred[, 1] / 10000
new_data_super$lower <- super_est[, 1] / 10000
new_data_super$upper <- super_est[, 2] / 10000

# 保存超区域预测
write.csv(new_data_super,
          file = file.path(predict_dir, "Super_estimate-all.csv"),
          row.names = FALSE)
cat("Super-region predictions saved\n")

# ==============================================================================
# Part 3: 区域级别预测
# ==============================================================================

cat("\n=== Generating Region Predictions ===\n")

# 定义所有区域及其对应的超区域
regions_data <- data.frame(
  Region = c("Asia, central", "Europe, central", "Europe, eastern",
              "Asia Pacific, high income", "Australasia", "Europe, western",
              "Latin America, southern", "North America, high income",
              "Caribbean", "Latin America, Andean", "Latin America, central",
              "Latin America, tropical", "North Africa and the Middle East",
              "Asia, south", "Asia, east", "Asia, South East", "Oceania",
              "Sub-Saharan Africa, central", "Sub-Saharan Africa, eastern",
              "Sub-Saharan Africa, southern", "Sub-Saharan Africa, western"),
  Super.region = c(
    rep("Central Europe, eastern Europe, and central Asia", 3),
    rep("High income", 5),
    rep("Latin America and Caribbean", 4),
    rep("North Africa and Middle East", 1),
    rep("South Asia", 1),
    rep("South East Asia, east Asia, and Oceania", 3),
    rep("Sub-Saharan Africa", 4)
  )
)

new_data_region <- data.frame(
  country = rep("Global", nrow(regions_data)),
  Region = regions_data$Region,
  Super.region = regions_data$Super.region,
  age_group = rep("ALL", nrow(regions_data)),
  sample_size = rep(">100W", nrow(regions_data)),
  study_period = rep("ALL", nrow(regions_data)),
  Definition = rep("CASPAR", nrow(regions_data)),
  population = rep(1000000, nrow(regions_data))
)

# 进行预测
region_pred <- predict(prev_model, newdata = new_data_region, allow_new_levels = TRUE)
region_est <- posterior_interval(region_pred, prob = 0.95)

new_data_region$ave <- region_pred[, 1] / 10000
new_data_region$lower <- region_est[, 1] / 10000
new_data_region$upper <- region_est[, 2] / 10000

# 保存区域预测
write.csv(new_data_region,
          file = file.path(predict_dir, "Region_estimate-all.csv"),
          row.names = FALSE)
cat("Region predictions saved\n")

# ==============================================================================
# Part 4: 国家级别预测
# ==============================================================================

cat("\n=== Generating Country Predictions ===\n")

# 获取数据中的唯一国家列表
unique_countries <- unique(prevalence_data_use$country)
unique_countries <- unique_countries[!is.na(unique_countries)]

# 创建议预测数据框
new_data_country <- data.frame(
  country = unique_countries,
  Region = NA,
  Super.region = NA,
  age_group = rep("ALL", length(unique_countries)),
  sample_size = rep(">100W", length(unique_countries)),
  study_period = rep("ALL", length(unique_countries)),
  Definition = rep("CASPAR", length(unique_countries)),
  population = rep(1000000, length(unique_countries))
)

# 从原始数据中提取Region和Super.region信息
country_info <- prevalence_data_use[, c("country", "Region", "Super.region")]
country_info <- country_info[!duplicated(country_info$country), ]

# 合并信息
new_data_country <- merge(new_data_country, country_info, by = "country", all.x = TRUE)

# 进行预测
country_pred <- predict(prev_model, newdata = new_data_country, allow_new_levels = TRUE)
country_est <- posterior_interval(country_pred, prob = 0.95)

new_data_country$ave <- country_pred[, 1] / 10000
new_data_country$lower <- country_est[, 1] / 10000
new_data_country$upper <- country_est[, 2] / 10000

# 按患病率排序
new_data_country <- arrange(new_data_country, desc(ave))

# 保存国家预测
write.csv(new_data_country,
          file = file.path(predict_dir, "Country_estimate-pre.csv"),
          row.names = FALSE)
cat("Country predictions saved\n")

# ==============================================================================
# Part 5: 后验预测检查
# ==============================================================================

cat("\n=== Posterior Predictive Check ===\n")

# 使用后验预测方法
pred_result <- posterior_predict(prev_model, newdata = new_data_country, allow_new_levels = TRUE)
mean_pred <- colMeans(pred_result)

# 计算患病率
new_data_country$ave_pp <- mean_pred / new_data_country$population * 100

# 保存后验预测结果
write.csv(new_data_country,
          file = file.path(predict_dir, "Country_posterior_predict.csv"),
          row.names = FALSE)

cat("\n=== Prevalence Prediction Complete ===\n")
cat("Output files:\n")
cat("  - Predict/Global_estimate-all.csv\n")
cat("  - Predict/Super_estimate-all.csv\n")
cat("  - Predict/Region_estimate-all.csv\n")
cat("  - Predict/Country_estimate-pre.csv\n")