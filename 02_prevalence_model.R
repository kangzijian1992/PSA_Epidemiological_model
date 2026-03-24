# ==============================================================================
# 02_prevalence_model.R
# 患病率贝叶斯模型构建脚本
# ==============================================================================
#
# Description:
#   使用贝叶斯分层模型估计PsA患病率
#   模型公式: prevalence ~ 1 + (1 | Super.region/Region/country) + (1 | study_period)
#           + (1 | sample_size) + (1 | age_group) + (1 | Definition)
#
# Input:
#   - output/All_PsA-pre-input.Rdata : 预处理后的患病率数据
#
# Output:
#   - output/prev_model-ALL.Rdata : 拟合的贝叶斯模型
#
# Required R packages:
#   - dplyr
#   - brms
#   - bayesplot
#
# Model Settings:
#   - Family: binomial (二项分布)
#   - Chains: 4
#   - Iterations: 4000
#   - Thin: 10
#   - Prior: student_t(3, 0, 2.5) for intercept
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
data_dir <- "data"
output_dir <- "output"

# ==============================================================================
# 加载预处理数据
# ==============================================================================

cat("=== Loading Preprocessed Prevalence Data ===\n")

# 加载预处理数据（如果不存在则运行数据预处理脚本）
if (!file.exists(file.path(output_dir, "All_PsA-pre-input.Rdata"))) {
  source(file.path("scripts", "01_data_preprocessing.R"))
}

load(file.path(output_dir, "All_PsA-pre-input.Rdata"))

# 使用清洗后的数据
datause <- prevalence_data_use

# 查看数据基本信息
cat("\nData dimensions:", dim(datause), "\n")
cat("\nColumn names:\n")
print(colnames(datause))
cat("\nData head:\n")
head(datause)

# 查看各分类变量的分布
cat("\n=== Variable Distributions ===\n")
cat("Countries:", length(unique(datause$country)), "\n")
cat("Regions:", length(unique(datause$Region)), "\n")
cat("Super-regions:", length(unique(datause$Super.region)), "\n")

# ==============================================================================
# 构建贝叶斯分层模型
# ==============================================================================

cat("\n=== Building Bayesian Hierarchical Model ===\n")

# 模型公式说明:
# - 固定效应: 截距
# - 随机效应:
#   - Super.region/Region/country: 分层随机效应（国家嵌套在区域中）
#   - study_period: 研究时间周期
#   - sample_size: 样本量分类
#   - age_group: 年龄组
#   - Definition: 诊断标准

# 构建贝叶斯模型
# 使用二项分布 family = binomial
# trials(population) 表示总人口数作为试验次数

prev_model <- brm(
  formula = total.PsA | trials(population) ~
    1 +
    (1 | Super.region / Region / country) +
    (1 | study_period) +
    (1 | sample_size) +
    (1 | age_group) +
    (1 | Definition),

  data = datause,
  family = binomial,

  # MCMC设置
  thin = 10,          # 每10次迭代保留1个样本
  chains = 4,         # 4条马尔可夫链
  iter = 4000,        # 每次链迭代4000次
  cores = 4,          # 使用4个CPU核心

  # 采样控制参数
  control = list(
    adapt_delta = 0.995,    # 增加adapt_delta以减少发散转换
    max_treedepth = 20      # 增加树深度限制
  ),

  # 先验设置
  prior = c(
    prior(student_t(3, 0, 2.5), class = "Intercept")
  ),

  # 输出设置
  verbose = TRUE
)

# ==============================================================================
# 模型摘要和保存
# ==============================================================================

cat("\n=== Model Summary ===\n")
print(summary(prev_model))

# 保存模型
save(prev_model, file = file.path(output_dir, "prev_model-ALL.Rdata"))
cat("\nModel saved to output/prev_model-ALL.Rdata\n")

# ==============================================================================
# 模型收敛诊断（基础）
# ==============================================================================

cat("\n=== Convergence Diagnostics ===\n")

# 检查R-hat值（应 < 1.01）
cat("\nR-hat values:\n")
print(head(rhat(prev_model)))

# 检查有效样本量
cat("\nEffective sample sizes:\n")
print(head(neff_ratio(prev_model)))

cat("\n=== Prevalence Model Complete ===\n")