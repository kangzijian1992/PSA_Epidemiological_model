# ==============================================================================
# 06_prevalence_visualization.R
# 患病率可视化脚本 - 生成地图和森林图
# ==============================================================================
#
# Description:
#   生成患病率的可视化图表：
#   1. 世界地图（各国患病率分布）
#   2. 国家级别森林图
#   3. 区域级别森林图
#   4. 研究分布地图
#
# Input:
#   - output/prev_model-ALL.Rdata       : 患病率模型
#   - output/spdf_fortified.Rdata       : 地图数据
#   - output/all_country.Rdata          : 国家列表
#   - output/Predict/Country_estimate-pre.csv : 国家预测结果
#   - output/study_counts.Rdata         : 研究计数
#
# Output:
#   - output/figures/Prevalence-country.png/pdf
#   - output/figures/Prevalence-country-forest.png/pdf
#   - output/figures/Region-forest.png/pdf
#   - output/figures/study_distribution.png/pdf
#
# Required R packages:
#   - dplyr
#   - ggplot2
#   - rgdal
#   - broom
#   - maptools
#
# ==============================================================================

# 加载所需的R包
library(dplyr)
library(ggplot2)
library(rgdal)
library(broom)
library(maptools)

# 设置随机种子以确保可重复性
set.seed(123)

# 设置工作目录
setwd(dirname(parent.frame(2)$ofile))

# 定义路径
output_dir <- "output"
figures_dir <- file.path(output_dir, "figures")

# 创建图形输出目录
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
}

# ==============================================================================
# 加载数据
# ==============================================================================

cat("=== Loading Data for Visualization ===\n")

# 加载地图数据
load(file.path(output_dir, "spdf_fortified.Rdata"))
load(file.path(output_dir, "all_country.Rdata"))

# 加载国家预测结果
country_est <- read.csv(file.path(output_dir, "Predict", "Country_estimate-pre.csv"))

# 加载研究计数
load(file.path(output_dir, "study_counts.Rdata"))

cat("Data loaded successfully\n")

# ==============================================================================
# Part 1: 世界地图 - 患病率分布
# ==============================================================================

cat("\n=== Generating Prevalence World Map ===\n")

# 准备地图数据
# 添加台湾到预测数据
taiwan_row <- country_est[country_est$country == "China", ]
taiwan_row$country <- "Taiwan"
country_est <- rbind(country_est, taiwan_row)

# 确保国家名称匹配
country_est$country <- recode(country_est$country,
                               "Cote d'Ivoire" = "Cote d'Ivoire")

# 合并地图和预测数据
spdf_merged <- spdf_fortified %>%
  left_join(country_est, by = c("id" = "country"))

# 转换为数值型
spdf_merged$ave <- as.numeric(spdf_merged$ave)

# 绘制世界地图
gg_map <- ggplot() +
  geom_polygon(data = spdf_merged,
               aes(fill = ave, x = long, y = lat, group = group),
               size = 0.2, color = "black") +
  theme_void() +
  scale_fill_gradientn(colours = c("#EBF4F9", "#9fcbe2", "#70ADDA", "#4594CF",
                                    "#3183be", "#245F8C"),
                       name = "Prevalence (%)",
                       na.value = "white") +
  theme(panel.background = element_rect(fill = "white"))

# 保存地图
ggsave(gg_map,
       filename = file.path(figures_dir, "Prevalence-country.png"),
       width = 15, height = 8, dpi = 400)
ggsave(gg_map,
       filename = file.path(figures_dir, "Prevalence-country.pdf"),
       width = 15, height = 8)
cat("Prevalence map saved\n")

# ==============================================================================
# Part 2: 国家级别森林图
# ==============================================================================

cat("\n=== Generating Country Forest Plot ===\n")

# 准备森林图数据
country_forest <- country_est[!is.na(country_est$ave), ]
country_forest <- arrange(country_forest, desc(ave))
country_forest$country <- factor(country_forest$country,
                                 levels = rev(country_forest$country))

# 绘制森林图
gg_forest_country <- ggplot(country_forest, aes(x = ave, y = country)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1,
                  color = alpha("#3183B3", 0.5)) +
  geom_point(size = 2, color = "#3183B3") +
  geom_vline(xintercept = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
             linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 16, colour = "black")) +
  labs(x = "Prevalence (%)", y = NULL) +
  coord_cartesian(xlim = c(0, 0.7)) +
  ggtitle("Country")

# 保存森林图
ggsave(gg_forest_country,
       filename = file.path(figures_dir, "Prevalence-country-forest.png"),
       width = 6, height = 8, dpi = 400)
ggsave(gg_forest_country,
       filename = file.path(figures_dir, "Prevalence-country-forest.pdf"),
       width = 6, height = 8)
cat("Country forest plot saved\n")

# 保存森林图数据
write.csv(country_forest,
          file = file.path(output_dir, "Country_forest_data.csv"),
          row.names = FALSE)

# ==============================================================================
# Part 3: 区域级别森林图
# ==============================================================================

cat("\n=== Generating Region Forest Plot ===\n")

# 加载区域预测结果
region_est <- read.csv(file.path(output_dir, "Predict", "Region_estimate-all.csv"))

# 准备森林图数据
region_forest <- region_est[!is.na(region_est$ave), ]
region_forest <- arrange(region_forest, desc(ave))
region_forest$Region <- factor(region_forest$Region,
                                levels = rev(region_forest$Region))

# 绘制森林图
gg_forest_region <- ggplot(region_forest, aes(x = ave, y = Region)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1,
                 color = alpha("#3183B3", 0.5)) +
  geom_point(size = 2, color = "#3183B3") +
  geom_vline(xintercept = c(0.1, 0.2, 0.3, 0.4), linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Prevalence (%)", y = NULL) +
  coord_cartesian(xlim = c(0, 0.35)) +
  ggtitle("Region")

# 保存森林图
ggsave(gg_forest_region,
       filename = file.path(figures_dir, "Region-forest.png"),
       width = 5.5, height = 6, dpi = 400)
ggsave(gg_forest_region,
       filename = file.path(figures_dir, "Region-forest.pdf"),
       width = 5.5, height = 6)
cat("Region forest plot saved\n")

# 保存区域森林图数据
write.csv(region_forest,
          file = file.path(output_dir, "Region_forest_data.csv"),
          row.names = FALSE)

# ==============================================================================
# Part 4: 研究分布地图
# ==============================================================================

cat("\n=== Generating Study Distribution Map ===\n")

# 准备研究分布地图数据
spdf_study <- spdf_fortified %>%
  left_join(study_df, by = c("id" = "country"))

spdf_study$number <- as.numeric(spdf_study$number)
spdf_study$number[is.na(spdf_study$number)] <- 0

# 绘制研究分布地图
gg_study <- ggplot() +
  geom_polygon(data = spdf_study,
               aes(fill = number, x = long, y = lat, group = group),
               size = 0.2, color = "black") +
  theme_void() +
  scale_fill_gradientn(colours = c("white", "#70ADDA", "#4594CF", "#3183be"),
                       name = "No. of Studies",
                       na.value = "white") +
  theme(panel.background = element_rect(fill = "white"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

# 保存研究分布地图
ggsave(gg_study,
       filename = file.path(figures_dir, "study_distribution.png"),
       width = 15, height = 8, dpi = 400)
ggsave(gg_study,
       filename = file.path(figures_dir, "study_distribution.pdf"),
       width = 15, height = 8)
cat("Study distribution map saved\n")

cat("\n=== Prevalence Visualization Complete ===\n")
cat("Output files:\n")
cat("  - figures/Prevalence-country.png/pdf\n")
cat("  - figures/Prevalence-country-forest.png/pdf\n")
cat("  - figures/Region-forest.png/pdf\n")
cat("  - figures/study_distribution.png/pdf\n")