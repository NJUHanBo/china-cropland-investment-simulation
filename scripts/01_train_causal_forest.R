#!/usr/bin/env Rscript
# =============================================================================
# 01 — 训练因果森林（4政策维度 × 3结果变量 = 12模型）
# =============================================================================
#
# 输入：data/basic_data.csv, models/stage3_factor_weights_detail.csv
# 输出：models/causal_forest_results/ 下的 τ̂矩阵、ATE汇总、变量重要性、模型RDS
#
# =============================================================================

library(grf)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")
source("lib/causal_forest_functions.R")

# ---- 1. 加载数据 & 构建因子 ------------------------------------------------

cat("=== 01: 因果森林训练 ===\n\n")

basic_data <- read.csv("data/basic_data.csv")
cat(sprintf("data: %d counties, %d columns\n", nrow(basic_data), ncol(basic_data)))

factors <- calculate_ml_guided_factors(basic_data, verbose = TRUE)
cat(sprintf("factors: %d rows, %d dimensions\n\n", nrow(factors), ncol(factors)))

outcomes <- basic_data[, c("CPI", "CPS", "CPA")]

# 检查NA
na_factors  <- colSums(is.na(factors))
na_outcomes <- colSums(is.na(outcomes))
if (any(na_factors > 0))  cat("WARNING: NA in factors: ",  paste(names(na_factors[na_factors > 0]), collapse = ", "), "\n")
if (any(na_outcomes > 0)) cat("WARNING: NA in outcomes: ", paste(names(na_outcomes[na_outcomes > 0]), collapse = ", "), "\n")

# ---- 2. 训练12个因果森林 ---------------------------------------------------

cat("training 12 causal forest models ...\n\n")
start_time <- Sys.time()

cf_results <- run_all_causal_forests(
  factors  = factors,
  outcomes = outcomes,
  num.trees       = 4000,
  min.node.size   = 15,
  sample.fraction = 0.35,
  honesty         = TRUE,
  seed            = 2024
)

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\ntraining done in %.1f min\n\n", as.numeric(elapsed)))

# ---- 3. 保存结果 -----------------------------------------------------------

out_dir <- "models/causal_forest_results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# τ̂ 矩阵（1560 × 12）
tau_df <- data.frame(
  Code = basic_data$Code,
  cf_results$tau_matrix
)
write.csv(tau_df, file.path(out_dir, "cf_tau_matrix.csv"), row.names = FALSE)
cat("saved: cf_tau_matrix.csv\n")

# τ̂ 标准误矩阵
tau_se_df <- data.frame(
  Code = basic_data$Code,
  cf_results$tau_se_matrix
)
write.csv(tau_se_df, file.path(out_dir, "cf_tau_se_matrix.csv"), row.names = FALSE)
cat("saved: cf_tau_se_matrix.csv\n")

# ATE 汇总
write.csv(cf_results$ate_summary, file.path(out_dir, "cf_ate_summary.csv"), row.names = FALSE)
cat("saved: cf_ate_summary.csv\n")

# 变量重要性
write.csv(cf_results$vi_summary, file.path(out_dir, "cf_variable_importance.csv"), row.names = FALSE)
cat("saved: cf_variable_importance.csv\n")

# 模型对象
saveRDS(cf_results$models, file.path(out_dir, "cf_models.RDS"))
cat("saved: cf_models.RDS\n")

# BN边修正建议
edge_lists <- generate_bn_edge_lists(cf_results$ate_summary)
saveRDS(edge_lists, file.path(out_dir, "cf_bn_edge_lists.RDS"))
cat("saved: cf_bn_edge_lists.RDS\n")

# ---- 4. 打印汇总 -----------------------------------------------------------

cat("\n=== ATE Summary ===\n")
print(cf_results$ate_summary[, c("Model_Label", "ATE", "ATE_SE", "Significant")])

n_sig <- sum(cf_results$ate_summary$Significant)
cat(sprintf("\n%d / %d treatment-outcome pairs are significant\n", n_sig, nrow(cf_results$ate_summary)))

cat(sprintf("\nwhitelist edges (confirmed by CF): %d\n", nrow(edge_lists$whitelist)))
cat(sprintf("blacklist edges (removed by CF):   %d\n", nrow(edge_lists$blacklist)))

cat("\n=== 01 complete ===\n")
