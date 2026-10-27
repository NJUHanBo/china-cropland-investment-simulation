#!/usr/bin/env Rscript
# =============================================================================
# 00 — 管线测试脚本（运行前必跑）
# =============================================================================
#
# 三层测试：
#   Layer 1: 依赖检查 + 数据加载
#   Layer 2: 核心方法论验证（解析预测 vs cpdist 一致性）
#   Layer 3: 小规模端到端测试（50个县 × 4种情景）
#
# =============================================================================

cat("====================================================\n")
cat("  Pipeline Test Script\n")
cat("====================================================\n\n")

passed <- 0L
failed <- 0L

check <- function(desc, expr) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    cat(sprintf("  FAIL: %s\n        %s\n", desc, result$message))
    failed <<- failed + 1L
    return(FALSE)
  }
  if (isTRUE(result)) {
    cat(sprintf("  PASS: %s\n", desc))
    passed <<- passed + 1L
    return(TRUE)
  }
  cat(sprintf("  FAIL: %s (returned FALSE)\n", desc))
  failed <<- failed + 1L
  FALSE
}

# =============================================================================
# Layer 1: 依赖 & 数据
# =============================================================================

cat("--- Layer 1: Dependencies & Data ---\n\n")

check("package grf", requireNamespace("grf", quietly = TRUE))
check("package bnlearn", requireNamespace("bnlearn", quietly = TRUE))
check("package cluster", requireNamespace("cluster", quietly = TRUE))
check("package dplyr", requireNamespace("dplyr", quietly = TRUE))
check("package tidyr", requireNamespace("tidyr", quietly = TRUE))
check("package ggplot2", requireNamespace("ggplot2", quietly = TRUE))

check("data/basic_data.csv exists", file.exists("data/basic_data.csv"))
check("data/CPI_20year_mean_by_county.csv exists", file.exists("data/CPI_20year_mean_by_county.csv"))
check("models/stage3_factor_weights_detail.csv exists", file.exists("models/stage3_factor_weights_detail.csv"))
check("models/enhanced_network_results/detailed_edge_statistics.csv exists",
      file.exists("models/enhanced_network_results/detailed_edge_statistics.csv"))

cat("\n")

# 加载数据
basic_data <- read.csv("data/basic_data.csv")
check("basic_data has 1560 rows", nrow(basic_data) == 1560)
check("basic_data has CPI/CPS/CPA columns", all(c("CPI", "CPS", "CPA") %in% names(basic_data)))

# 加载函数库
source("lib/ml_guided_factor_calculation.R")
source("lib/causal_forest_functions.R")
source("lib/cluster_bn_functions.R")

check("ml_guided_factor_calculation loaded", exists("calculate_ml_guided_factors"))
check("causal_forest_functions loaded", exists("train_single_causal_forest"))
check("cluster_bn_functions loaded", exists("predict_bn_node_analytical"))

# 因子计算
factors <- calculate_ml_guided_factors(basic_data, verbose = FALSE)
check("factors has 6 dimensions", ncol(factors) == 6)
check("factors has correct column names",
      all(c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度") %in% names(factors)))
check("factors has no NA", !any(is.na(factors)))

cat("\n")

# =============================================================================
# Layer 2: 核心方法论验证
# =============================================================================

cat("--- Layer 2: Methodology Validation ---\n\n")

library(grf)
library(bnlearn)

# ---- 2a: causal_forest 基本运行 ----

cat("  testing causal_forest with continuous W ...\n")
set.seed(2024)
n_test <- 200
idx_test <- sample(nrow(basic_data), n_test)

W_test <- factors$保护政策[idx_test]
Y_test <- basic_data$CPI[idx_test]
X_test <- factors[idx_test, c("城市化", "气候变化", "土地利用", "利用强度", "产权制度")]

cf_test <- tryCatch({
  causal_forest(
    X = as.matrix(X_test), Y = Y_test, W = W_test,
    num.trees = 500, min.node.size = 15, honesty = TRUE, seed = 2024
  )
}, error = function(e) e)

check("causal_forest runs without error", !inherits(cf_test, "error"))

if (!inherits(cf_test, "error")) {
  tau_test <- predict(cf_test, estimate.variance = TRUE)
  check("causal_forest returns predictions", length(tau_test$predictions) == n_test)
  check("causal_forest returns variance", length(tau_test$variance.estimates) == n_test)

  ate_test <- average_treatment_effect(cf_test)
  check("ATE has estimate and std.err", all(c("estimate", "std.err") %in% names(ate_test)))
  cat(sprintf("    ATE = %.4f (SE = %.4f)\n", ate_test["estimate"], ate_test["std.err"]))
}

# ---- 2b: 高斯BN解析预测 vs cpdist 一致性 ----

cat("\n  testing analytical BN prediction vs cpdist ...\n")

# 构建一个小的测试BN
network_data_test <- data.frame(
  城市化   = factors$城市化,
  气候变化 = factors$气候变化,
  保护政策 = factors$保护政策,
  土地利用 = factors$土地利用,
  利用强度 = factors$利用强度,
  产权制度 = factors$产权制度,
  CPI = basic_data$CPI,
  CPS = basic_data$CPS,
  CPA = basic_data$CPA
)
network_data_test[] <- lapply(network_data_test, as.numeric)

# 用简单的DAG
test_dag <- empty.graph(names(network_data_test))
test_edges <- data.frame(
  from = c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度",
           "城市化", "土地利用", "CPS"),
  to   = c("CPI", "CPI", "CPS", "CPS", "CPA", "CPA",
           "CPS", "产权制度", "CPA"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(test_edges))) {
  tryCatch({
    temp <- set.arc(test_dag, test_edges$from[i], test_edges$to[i])
    if (acyclic(temp)) test_dag <- temp
  }, error = function(e) NULL)
}

test_bn <- bn.fit(test_dag, network_data_test, method = "mle-g")

# 选10个县做对比
test_idx <- 1:10
evidence_df <- network_data_test[test_idx, c("城市化", "气候变化", "保护政策",
                                              "土地利用", "利用强度", "产权制度")]

# 解析预测
analytical_pred <- predict_bn_targets_analytical(test_bn, c("CPI", "CPS", "CPA"), evidence_df)

# cpdist 预测（逐样本）
cpdist_CPI <- numeric(nrow(evidence_df))
cpdist_CPS <- numeric(nrow(evidence_df))
cpdist_CPA <- numeric(nrow(evidence_df))

for (i in seq_len(nrow(evidence_df))) {
  ev <- as.list(evidence_df[i, ])
  set.seed(42)
  dist_CPI <- cpdist(test_bn, nodes = "CPI", evidence = ev, method = "lw", n = 10000)
  dist_CPS <- cpdist(test_bn, nodes = "CPS", evidence = ev, method = "lw", n = 10000)
  dist_CPA <- cpdist(test_bn, nodes = "CPA", evidence = ev, method = "lw", n = 10000)
  cpdist_CPI[i] <- mean(dist_CPI$CPI, na.rm = TRUE)
  cpdist_CPS[i] <- mean(dist_CPS$CPS, na.rm = TRUE)
  cpdist_CPA[i] <- mean(dist_CPA$CPA, na.rm = TRUE)
}

# 对比
diff_CPI <- abs(analytical_pred$CPI - cpdist_CPI)
diff_CPS <- abs(analytical_pred$CPS - cpdist_CPS)
diff_CPA <- abs(analytical_pred$CPA - cpdist_CPA)

max_diff <- max(c(diff_CPI, diff_CPS, diff_CPA))
mean_diff <- mean(c(diff_CPI, diff_CPS, diff_CPA))

cat(sprintf("    max |analytical - cpdist|  = %.6f\n", max_diff))
cat(sprintf("    mean |analytical - cpdist| = %.6f\n", mean_diff))

# 高斯BN的解析解应该与大样本cpdist几乎完全一致（差异来自蒙特卡洛噪声）
check("analytical vs cpdist max diff < 0.05", max_diff < 0.05)
check("analytical vs cpdist mean diff < 0.01", mean_diff < 0.01)

# 相关性应该非常高
cor_CPI <- cor(analytical_pred$CPI, cpdist_CPI)
cor_CPS <- cor(analytical_pred$CPS, cpdist_CPS)
cor_CPA <- cor(analytical_pred$CPA, cpdist_CPA)

cat(sprintf("    correlation CPI: %.6f, CPS: %.6f, CPA: %.6f\n", cor_CPI, cor_CPS, cor_CPA))
check("analytical vs cpdist correlation > 0.999", min(cor_CPI, cor_CPS, cor_CPA) > 0.999)

cat("\n")

# ---- 2c: predict_by_cluster 分群路由 ----

cat("  testing predict_by_cluster routing ...\n")

# 模拟2个群组
mock_clusters <- ifelse(seq_len(nrow(network_data_test)) <= 780, 1L, 2L)
mock_bns <- list(test_bn, test_bn)  # 用相同的BN，只验证路由逻辑

pred_cluster <- predict_by_cluster(mock_bns, mock_clusters, c("CPI", "CPS", "CPA"),
                                    network_data_test[, c("城市化", "气候变化", "保护政策",
                                                           "土地利用", "利用强度", "产权制度")])

check("predict_by_cluster returns correct rows", nrow(pred_cluster) == nrow(network_data_test))
check("predict_by_cluster has no NA in CPI", !any(is.na(pred_cluster$CPI)))
check("predict_by_cluster has CPI_sd column", "CPI_sd" %in% names(pred_cluster))

cat("\n")

# =============================================================================
# Layer 3: 小规模端到端测试
# =============================================================================

cat("--- Layer 3: Mini End-to-End Test ---\n\n")

# 用50个县、2个外部环境、2个政策、2个空间分配 = 8种组合
n_mini <- 50
set.seed(2024)
mini_idx <- sample(nrow(basic_data), n_mini)
mini_data <- basic_data[mini_idx, ]
mini_factors <- factors[mini_idx, ]
mini_outcomes <- mini_data[, c("CPI", "CPS", "CPA")]

# ---- 3a: 因果森林 mini ----

cat("  3a: training 2 mini causal forests ...\n")

# 只训练2个模型（保护政策->CPI, 土地利用->CPS）作为验证
cf_mini_1 <- train_single_causal_forest(
  Y = mini_outcomes$CPI,
  W = mini_factors$保护政策,
  X = mini_factors[, c("城市化", "气候变化", "土地利用", "利用强度", "产权制度")],
  num.trees = 200, seed = 2024
)

cf_mini_2 <- train_single_causal_forest(
  Y = mini_outcomes$CPS,
  W = mini_factors$土地利用,
  X = mini_factors[, c("城市化", "气候变化", "保护政策", "利用强度", "产权制度")],
  num.trees = 200, seed = 2024
)

check("mini CF 1 has tau_hat of correct length", length(cf_mini_1$tau_hat) == n_mini)
check("mini CF 2 has ATE", is.numeric(cf_mini_2$ate) && length(cf_mini_2$ate) == 1)

# ---- 3b: 聚类 mini ----

cat("\n  3b: clustering mini tau matrix ...\n")

mini_tau <- cbind(cf_mini_1$tau_hat, cf_mini_2$tau_hat)
colnames(mini_tau) <- c("保护_CPI", "土地_CPS")

mini_cluster <- cluster_counties_by_tau(mini_tau, k_range = 2:3, seed = 2024)

check("clustering returns assignments", length(mini_cluster$assignments) == n_mini)
check("all counties assigned", !any(is.na(mini_cluster$assignments)))

# ---- 3c: 分群BN mini ----

cat("\n  3c: fitting cluster BNs ...\n")

mini_network_data <- data.frame(
  城市化 = mini_factors$城市化,
  气候变化 = mini_factors$气候变化,
  保护政策 = mini_factors$保护政策,
  土地利用 = mini_factors$土地利用,
  利用强度 = mini_factors$利用强度,
  产权制度 = mini_factors$产权制度,
  CPI = mini_data$CPI,
  CPS = mini_data$CPS,
  CPA = mini_data$CPA
)
mini_network_data[] <- lapply(mini_network_data, as.numeric)

mini_cluster_bns <- fit_cluster_bns(test_dag, mini_network_data, mini_cluster$assignments)

check("got K BN models", length(mini_cluster_bns) == mini_cluster$optimal_k)

# ---- 3d: 情景预测 mini ----

cat("\n  3d: scenario prediction ...\n")

# 基线预测
baseline_pred <- predict_by_cluster(
  mini_cluster_bns, mini_cluster$assignments, c("CPI", "CPS", "CPA"),
  mini_network_data[, c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度")]
)

# 施加简单干预：保护政策因子 × 1.5
intervention_data <- mini_network_data
intervention_data$保护政策 <- intervention_data$保护政策 * 1.5

intervention_pred <- predict_by_cluster(
  mini_cluster_bns, mini_cluster$assignments, c("CPI", "CPS", "CPA"),
  intervention_data[, c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度")]
)

effects <- intervention_pred[, c("CPI", "CPS", "CPA")] - baseline_pred[, c("CPI", "CPS", "CPA")]

check("effects computed without NA", !any(is.na(effects)))
check("effects are not all zero", any(effects != 0))
check("effects have reasonable magnitude (< 1)",
      all(abs(effects) < 1, na.rm = TRUE))

cat(sprintf("    mean effects: CPI=%.4f, CPS=%.4f, CPA=%.4f\n",
            mean(effects$CPI), mean(effects$CPS), mean(effects$CPA)))

# ---- 3e: 权衡分类 mini ----

cat("\n  3e: tradeoff classification ...\n")

n_pos <- (effects$CPI > 0) + (effects$CPS > 0) + (effects$CPA > 0)
tradeoff_class <- ifelse(n_pos == 3, "full_synergy",
                  ifelse(n_pos == 2, "partial_synergy",
                  ifelse(n_pos == 1, "partial_tradeoff", "full_negative")))

check("tradeoff classification works", length(tradeoff_class) == n_mini)
cat(sprintf("    distribution: %s\n",
            paste(names(table(tradeoff_class)), table(tradeoff_class), sep = "=", collapse = ", ")))

# =============================================================================
# 结果汇总
# =============================================================================

cat("\n====================================================\n")
cat(sprintf("  RESULTS: %d PASSED, %d FAILED\n", passed, failed))
cat("====================================================\n")

if (failed > 0) {
  cat("\n  WARNING: Some tests failed. Review before running full pipeline.\n")
} else {
  cat("\n  All tests passed. Safe to run full pipeline:\n")
  cat("    Rscript scripts/01_train_causal_forest.R\n")
  cat("    Rscript scripts/02_cluster_counties.R\n")
  cat("    Rscript scripts/03_build_cluster_bn.R\n")
  cat("    Rscript scripts/04_scenario_simulation.R\n")
  cat("    Rscript scripts/05_tradeoff_analysis.R\n")
  cat("    Rscript scripts/06_national_analysis.R\n")
  cat("    Rscript scripts/07_visualization.R\n")
}
