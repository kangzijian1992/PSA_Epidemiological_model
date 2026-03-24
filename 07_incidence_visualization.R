# ==============================================================================
# 07_incidence_visualization.R
# 发病率可视化脚本 - 生成地图和森林图
# ==============================================================================
#
# Description:
#   生成发病率的可视化图表：
#   1. 世界地图（各国发病率分布）
#   2. 国家级别森林图
#   3. 区域级别森林图
#
# Input:
#   - output/Inc_model-ALL.Rdata           : 发病率模型
#   - output/spdf_fortified.Rdata          : 地图数据
#   - output/all_country.Rdata             : 国家列表
#   - output/Predict/Country_estimate-Inc.csv : 国家预测结果
#
# Output:
#   - output/figures/Incidence-country.png/pdf
#   - output/figures/Incidence-country-forest.png/pdf
#   - output/figures/Incidence-Region-forest.png/pdf
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
country_est <- read.csv(file.path(output_dir, "Predict", "Country_estimate-Inc.csv"))

cat("Data loaded successfully\n")

# ==============================================================================
# Part 1: 世界地图 - 发病率分布
# ==============================================================================

cat("\n=== Generating Incidence World Map ===\n")

# 准备地图数据
# 添加台湾到预测数据
taiwan_row <- country_est[country_est$Country == "China", ]
taiwan_row$Country <- "Taiwan"
country_est <- rbind(country_est, taiwan_row)

# 重命名列以匹配地图数据
names(country_est)[names(country_est) == "Country"] <- "country"

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
  scale_fill_gradient(low = "#D7E8F5", high = "#3183B3",
                     name = "Incident PsA\nper 100,000\nperson-years",
                     na.value = "white") +
  theme(panel.background = element_rect(fill = "white"))

# 保存地图
ggsave(gg_map,
       filename = file.path(figures_dir, "Incidence-country.png"),
       width = 15, height = 8, dpi = 400)
ggsave(gg_map,
       filename = file.path(figures_dir, "Incidence-country.pdf"),
       width = 15, height = 8)
cat("Incidence map saved\n")

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
  geom_vline(xintercept = c(10, 20, 30, 40), linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 16, colour = "black")) +
  labs(x = "Incident PsA per 100,000 person-years", y = NULL) +
  coord_cartesian(xlim = c(0, 35)) +
  ggtitle("Country")

# 保存森林图
ggsave(gg_forest_country,
       filename = file.path(figures_dir, "Incidence-country-forest.png"),
       width = 5, height = 6, dpi = 400)
ggsave(gg_forest_country,
       filename = file.path(figures_dir, "Incidence-country-forest.pdf"),
       width = 5, height = 6)
cat("Country forest plot saved\n")

# 保存森林图数据
write.csv(country_forest,
          file = file.path(output_dir, "Country_Inc_forest_data.csv"),
          row.names = FALSE)

# ==============================================================================
# Part 3: 区域级别森林图
# ==============================================================================

cat("\n=== Generating Region Forest Plot ===\n")

# 加载区域预测结果
region_est <- read.csv(file.path(output_dir, "Predict", "Region_Inc_estimate-all.csv"))

# 准备森林图数据
region_forest <- region_est[!is.na(region_est$ave), ]
region_forest <- arrange(region_forest, desc(ave))
region_forest$region <- factor(region_forest$region,
                                levels = rev(region_forest$region))

# 绘制森林图
gg_forest_region <- ggplot(region_forest, aes(x = ave, y = region)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1,
                 color = alpha("#3183B3", 0.5)) +
  geom_point(size = 2, color = "#3183B3") +
  geom_vline(xintercept = c(10, 20, 30), linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Incident PsA per 100,000 person-years", y = NULL) +
  coord_cartesian(xlim = c(0, 25)) +
  ggtitle("Region")

# 保存森林图
ggsave(gg_forest_region,
       filename = file.path(figures_dir, "Incidence-Region-forest.png"),
       width = 5.5, height = 6, dpi = 400)
ggsave(gg_forest_region,
       filename = file.path(figures_dir, "Incidence-Region-forest.pdf"),
       width = 5.5, height = 6)
cat("Region forest plot saved\n")

# 保存区域森林图数据
write.csv(region_forest,
          file = file.path(output_dir, "Region_Inc_forest_data.csv"),
          row.names = FALSE)

cat("\n=== Incidence Visualization Complete ===\n")
cat("Output files:\n")
cat("  - figures/Incidence-country.png/pdf\n")
cat("  - figures/Incidence-country-forest.png/pdf\n")
cat("  - figures/Incidence-Region-forest.png/pdf\n")