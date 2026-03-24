# ==============================================================================
# 08_model_diagnostics.R
# 模型诊断脚本 - 贝叶斯模型收敛性和拟合优度评估
# ==============================================================================
#
# Description:
#   对贝叶斯模型进行全面的收敛性诊断和拟合优度评估
#   这些是审稿人最关注的内容：
#   1. R-hat 值检查
#   2. 有效样本量 (ESS)
#   3. 发散转换检查
#   4. 后验预测检查 (pp_check)
#   5. LOO-CV 交叉验证
#   6. 轨迹图 (trace plots)
#
# Input:
#   - output/prev_model-ALL.Rdata : 患病率模型
#   - output/Inc_model-ALL.Rdata  : 发病率模型
#
# Output:
#   - output/diagnostics/prevalence_convergence.png
#   - output/diagnostics/incidence_convergence.png
#   - output/diagnostics/loo_comparison.png
#   - output/diagnostics/pp_check_prevalence.png
#   - output/diagnostics/pp_check_incidence.png
#   - output/diagnostics/model_diagnostics_summary.txt
#
# Required R packages:
#   - brms
#   - bayesplot
#   - loo
#
# ==============================================================================

# 加载所需的R包
library(brms)
library(bayesplot)
library(loo)

# 设置随机种子以确保可重复性
set.seed(123)

# 设置工作目录
setwd(dirname(parent.frame(2)$ofile))

# 定义路径
output_dir <- "output"
diagnostics_dir <- file.path(output_dir, "diagnostics")

# 创建诊断输出目录
if (!dir.exists(diagnostics_dir)) {
  dir.create(diagnostics_dir)
}

# ==============================================================================
# 函数定义
# ==============================================================================

run_diagnostics <- function(model, model_name, output_dir) {

  cat(paste("\n==========", model_name, "Model Diagnostics", "==========\n"))

  # ==============================================================================
  # 1. 模型摘要
  # ==============================================================================

  cat("\n--- Model Summary ---\n")
  model_summary <- summary(model)
  print(model_summary)

  # ==============================================================================
  # 2. R-hat 值检查
  # ==============================================================================

  cat("\n--- R-hat Diagnostics ---\n")
  rhat_vals <- rhat(model)
  cat("All R-hat values < 1.01:", all(rhat_vals < 1.01), "\n")
  cat("Max R-hat:", max(rhat_vals), "\n")
  cat("Mean R-hat:", mean(rhat_vals), "\n")

  # 绘制R-hat图
  gg_rhat <- mcmc_rhat(rhat_vals)
  ggsave(gg_rhat,
         filename = file.path(output_dir, paste0("rhat_", tolower(model_name), ".png")),
         width = 10, height = 6)

  # ==============================================================================
  # 3. 有效样本量 (ESS) 检查
  # ==============================================================================

  cat("\n--- Effective Sample Size Diagnostics ---\n")
  neff <- neff_ratio(model)
  cat("All Bulk_ESS > 400:", all(neff > 400), "\n")
  cat("Min Bulk_ESS:", min(neff), "\n")
  cat("Mean Bulk_ESS:", mean(neff), "\n")

  # 绘制ESS图
  gg_neff <- mcmc_neff(neff)
  ggsave(gg_neff,
         filename = file.path(output_dir, paste0("neff_", tolower(model_name), ".png")),
         width = 10, height = 6)

  # ==============================================================================
  # 4. 发散转换检查
  # ==============================================================================

  cat("\n--- Divergent Transitions Diagnostics ---\n")
  nuts_params <- nuts_params(model)
  divergent <- nuts_params %>% filter(Parameter == "divergent__")
  n_divergent <- sum(divergent$Value)
  cat("Number of divergent transitions:", n_divergent, "\n")
  cat("Total iterations:", nrow(nuts_params) / length(unique(nuts_params$Chain)), "\n")

  # ==============================================================================
  # 5. 后验预测检查 (pp_check) - 审稿人重点关注
  # ==============================================================================

  cat("\n--- Posterior Predictive Check (pp_check) ---\n")

  # 5.1 密度叠加图
  gg_pp1 <- pp_check(model, type = "dens_overlay", ndraws = 100)
  ggsave(gg_pp1,
         filename = file.path(output_dir, paste0("pp_check_density_", tolower(model_name), ".png")),
         width = 8, height = 6)

  # 5.2 均值统计量检查
  gg_pp2 <- pp_check(model, type = "stat", stat = "mean")
  ggsave(gg_pp2,
         filename = file.path(output_dir, paste0("pp_check_mean_", tolower(model_name), ".png")),
         width = 8, height = 6)

  # 5.3 标准差统计量检查
  gg_pp3 <- pp_check(model, type = "stat", stat = "sd")
  ggsave(gg_pp3,
         filename = file.path(output_dir, paste0("pp_check_sd_", tolower(model_name), ".png")),
         width = 8, height = 6)

  # 5.4 分位数检查
  gg_pp4 <- pp_check(model, type = "stat_2d", stat = c("mean", "sd"))
  ggsave(gg_pp4,
         filename = file.path(output_dir, paste0("pp_check_stat2d_", tolower(model_name), ".png")),
         width = 8, height = 6)

  # ==============================================================================
  # 6. LOO-CV 交叉验证 - 审稿人重点关注
  # ==============================================================================

  cat("\n--- Leave-One-Out Cross-Validation (LOO-CV) ---\n")

  # 计算LOO
  loo_result <- loo(model)
  print(loo_result)

  # 保存LOO结果
  png(file.path(output_dir, paste0("loo_pareto_k_", tolower(model_name), ".png")))
  plot(loo_result)
  dev.off()

  # LOO对比（如果有多个模型可以对比）
  cat("LOO IC (lower is better):", loo_result$estimates[3, 1], "\n")
  cat("LOO IC SE:", loo_result$estimates[3, 2], "\n")

  # ==============================================================================
  # 7. 轨迹图 (Trace Plots)
  # ==============================================================================

  cat("\n--- Trace Plots ---\n")

  # 7.1 截距轨迹图
  gg_trace1 <- mcmc_trace(model, pars = "b_Intercept")
  ggsave(gg_trace1,
         filename = file.path(output_dir, paste0("trace_intercept_", tolower(model_name), ".png")),
         width = 10, height = 4)

  # 7.2 随机效应SD轨迹图
  gg_trace2 <- mcmc_trace(model, regex_pars = "^sd_")
  ggsave(gg_trace2,
         filename = file.path(output_dir, paste0("trace_random_sds_", tolower(model_name), ".png")),
         width = 10, height = 8)

  # ==============================================================================
  # 8. 生成诊断摘要报告
  # ==============================================================================

  diagnostics_summary <- paste0(
    "========== ", model_name, " Model Diagnostics Summary ==========\n\n",
    "1. Convergence Diagnostics:\n",
    "   - All R-hat < 1.01: ", all(rhat_vals < 1.01), "\n",
    "   - Max R-hat: ", round(max(rhat_vals), 4), "\n",
    "   - All Bulk_ESS > 400: ", all(neff > 400), "\n",
    "   - Min Bulk_ESS: ", round(min(neff), 0), "\n",
    "   - Divergent transitions: ", n_divergent, "\n\n",

    "2. Posterior Predictive Check:\n",
    "   - See pp_check_*.png files\n\n",

    "3. LOO-CV:\n",
    "   - LOO IC: ", round(loo_result$estimates[3, 1], 2), "\n",
    "   - LOO IC SE: ", round(loo_result$estimates[3, 2], 2), "\n\n",

    "Model formula:\n",
    as.character(model$formula), "\n"
  )

  return(diagnostics_summary)
}

# ==============================================================================
# 运行诊断
# ==============================================================================

cat("=== Starting Model Diagnostics ===\n")

# 检查模型文件是否存在
prev_model_exists <- file.exists(file.path(output_dir, "prev_model-ALL.Rdata"))
inc_model_exists <- file.exists(file.path(output_dir, "Inc_model-ALL.Rdata"))

if (!prev_model_exists) {
  cat("Warning: Prevalence model not found. Please run 02_prevalence_model.R first.\n")
}

if (!inc_model_exists) {
  cat("Warning: Incidence model not found. Please run 03_incidence_model.R first.\n")
}

# 诊断患病率模型
all_summaries <- character(0)

if (prev_model_exists) {
  cat("\n>>> Diagnosing Prevalence Model <<<\n")
  load(file.path(output_dir, "prev_model-ALL.Rdata"))
  prev_summary <- run_diagnostics(prev_model, "Prevalence", diagnostics_dir)
  all_summaries <- paste(all_summaries, prev_summary)
}

# 诊断发病率模型
if (inc_model_exists) {
  cat("\n>>> Diagnosing Incidence Model <<<\n")
  load(file.path(output_dir, "Inc_model-ALL.Rdata"))
  inc_summary <- run_diagnostics(Inc_model, "Incidence", diagnostics_dir)
  all_summaries <- paste(all_summaries, inc_summary)
}

# 保存诊断摘要到文件
writeLines(all_summaries,
           file.path(output_dir, "diagnostics_summary.txt"))

cat("\n=== Model Diagnostics Complete ===\n")
cat("Output files:\n")
cat("  - diagnostics/rhat_*.png\n")
cat("  - diagnostics/neff_*.png\n")
cat("  - diagnostics/pp_check_*.png\n")
cat("  - diagnostics/loo_pareto_k_*.png\n")
cat("  - diagnostics/trace_*.png\n")
cat("  - diagnostics_summary.txt\n")