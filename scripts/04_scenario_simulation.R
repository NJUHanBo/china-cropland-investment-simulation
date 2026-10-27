#!/usr/bin/env Rscript
# =============================================================================
# 04 — 情景模拟（分群BN解析预测，个体层面）
# =============================================================================
#
# 输入：models/corrected_bn/  (DAG + K个BN)
#       models/cluster_results/cluster_assignments.csv
#       data/basic_data.csv, data/CPI_20year_mean_by_county.csv
# 输出：output/csv/ 下的完整情景预测结果
#
# 关键优化：高斯BN闭式解替代cpdist蒙特卡洛，向量化矩阵运算替代逐县循环
#
# =============================================================================

library(bnlearn)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")

# =============================================================================
# 情景配置（与旧管线保持一致）
# =============================================================================

external_configs <- list(
  E1 = c(0.85, 0.90),
  E2 = c(1.00, 1.00),
  E3 = c(1.30, 1.10)
)

policy_configs <- list(
  Baseline = c(0, 0, 0, 0),
  I01 = c(3, 3, 3, 3),
  I02 = c(9, 1, 1, 1),
  I03 = c(1, 9, 1, 1),
  I04 = c(1, 1, 9, 1),
  I05 = c(1, 1, 1, 9),
  I06 = c(5, 5, 1, 1),
  I07 = c(5, 1, 5, 1),
  I08 = c(5, 1, 1, 5),
  I09 = c(1, 5, 5, 1),
  I10 = c(1, 5, 1, 5),
  I11 = c(1, 1, 5, 5),
  I12 = c(4, 4, 4, 0),
  I13 = c(4, 4, 0, 4),
  I14 = c(4, 0, 4, 4),
  I15 = c(0, 4, 4, 4)
)

allocation_strategies <- c(
  "统一投资",
  "CPDS雪中送炭", "CPDS锦上添花",
  "CPI雪中送炭", "CPI锦上添花",
  "保护锦_土地锦_利用锦_产权锦",
  "保护锦_土地锦_利用锦_产权雪",
  "保护锦_土地锦_利用雪_产权锦",
  "保护锦_土地锦_利用雪_产权雪",
  "保护锦_土地雪_利用锦_产权锦",
  "保护锦_土地雪_利用锦_产权雪",
  "保护锦_土地雪_利用雪_产权锦",
  "保护锦_土地雪_利用雪_产权雪",
  "保护雪_土地锦_利用锦_产权锦",
  "保护雪_土地锦_利用锦_产权雪",
  "保护雪_土地锦_利用雪_产权锦",
  "保护雪_土地锦_利用雪_产权雪",
  "保护雪_土地雪_利用锦_产权锦",
  "保护雪_土地雪_利用锦_产权雪",
  "保护雪_土地雪_利用雪_产权锦",
  "保护雪_土地雪_利用雪_产权雪"
)

# =============================================================================
# 加载数据和模型
# =============================================================================

cat("=== 04: scenario simulation (cluster-BN analytical prediction) ===\n\n")

basic_data    <- read.csv("data/basic_data.csv")
cpi_mean_data <- read.csv("data/CPI_20year_mean_by_county.csv")
combined_data <- merge(basic_data, cpi_mean_data[, c("Code", "CPI_20year_mean")],
                       by = "Code", all.x = TRUE)
assignments   <- read.csv("models/cluster_results/cluster_assignments.csv")
dag           <- readRDS("models/corrected_bn/corrected_dag.RDS")

cluster_ids <- assignments$Cluster[match(combined_data$Code, assignments$Code)]
stopifnot("cluster alignment failed" = !any(is.na(cluster_ids)))
K <- max(cluster_ids)

cluster_bns <- lapply(seq_len(K), function(k) {
  readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k))
})

cat(sprintf("data: %d counties, %d clusters, %d BN models\n", nrow(combined_data), K, K))

# 变量组定义
urban_vars   <- grep("URB", names(combined_data), value = TRUE)
climate_vars <- grep("Clim", names(combined_data), value = TRUE)
conserv_vars <- grep("Conserv\\.", names(combined_data), value = TRUE)
land_vars    <- grep("LAN", names(combined_data), value = TRUE)
using_vars   <- grep("Using", names(combined_data), value = TRUE)
prop_vars    <- grep("Prop", names(combined_data), value = TRUE)

cpds_values <- combined_data$CPDS
cpi_values  <- combined_data$CPI_20year_mean

var_groups <- list(conserv_vars, land_vars, using_vars, prop_vars)
dimension_names <- c("保护", "土地", "利用", "产权")

n_counties <- nrow(combined_data)
target_nodes <- c("CPI", "CPS", "CPA")

total_combos <- length(external_configs) * length(policy_configs) * length(allocation_strategies)
cat(sprintf("scenarios: %d external x %d policy x %d allocation = %d combos\n",
            length(external_configs), length(policy_configs),
            length(allocation_strategies), total_combos))

# =============================================================================
# 辅助函数：向量化空间分配
# =============================================================================

# 计算某维度的空间分配乘数（向量化，返回 n_counties 长度的向量）
compute_spatial_mults <- function(allocation_strategy, dim_idx, base_mult,
                                  policy_data, external_data, var_group) {
  if (base_mult <= 1) return(rep(1.0, n_counties))

  policy_part <- base_mult - 1.0

  is_old_strategy <- allocation_strategy %in% c("CPDS雪中送炭", "CPDS锦上添花",
                                                  "CPI雪中送炭", "CPI锦上添花")
  # 确定分配标准
  if (is_old_strategy) {
    criterion <- if (grepl("CPDS", allocation_strategy)) cpds_values else cpi_values
  } else {
    criterion <- rowMeans(policy_data[, var_group, drop = FALSE], na.rm = TRUE)
  }

  # 归一化到 [0,1]
  crit_range <- range(criterion, na.rm = TRUE)
  criterion_norm <- (criterion - crit_range[1]) / max(crit_range[2] - crit_range[1], 1e-10)
  criterion_norm[!is.finite(criterion_norm)] <- 0

  # 判断锦/雪
  if (is_old_strategy) {
    is_snow <- grepl("雪", allocation_strategy)
  } else {
    pattern <- paste0(dimension_names[dim_idx], "([锦雪])")
    match_res <- regmatches(allocation_strategy, regexec(pattern, allocation_strategy))
    is_snow <- length(match_res[[1]]) > 1 && match_res[[1]][2] == "雪"
  }

  # 向量化计算理想乘数
  ideal_policy_mult <- if (is_snow) {
    policy_part * (2 - 2 * criterion_norm)
  } else {
    policy_part * (2 * criterion_norm)
  }

  raw_mults <- 1.0 + ideal_policy_mult

  # 预算校准
  base_vals <- rowMeans(external_data[, var_group, drop = FALSE], na.rm = TRUE)
  target_total <- sum(base_vals * base_mult, na.rm = TRUE)
  raw_total    <- sum(base_vals * raw_mults, na.rm = TRUE)

  constraint <- if (raw_total > 0 && raw_total != target_total) target_total / raw_total else 1.0
  raw_mults * constraint
}

# =============================================================================
# 主模拟循环
# =============================================================================

cat("\nstarting simulation ...\n")
start_time <- Sys.time()

all_results <- vector("list", total_combos)
combo_idx <- 0L

# VECTORIZATION CHECK: 外层三重循环遍历不同情景组合，每个组合的数据修改不同，无法向量化
# 但每个组合内部的所有计算都是向量化的（矩阵运算、全县批量预测）

for (e_name in names(external_configs)) {
  ext_cfg <- external_configs[[e_name]]
  cat(sprintf("\n--- External: %s ---\n", e_name))

  # 向量化施加外部环境
  external_data <- combined_data
  external_data[, urban_vars]   <- external_data[, urban_vars]   * ext_cfg[1]
  external_data[, climate_vars] <- external_data[, climate_vars] * ext_cfg[2]

  # 基线因子（该外部环境下，无政策干预）
  baseline_factors <- calculate_ml_guided_factors(external_data, verbose = FALSE)

  # 基线预测（一次性，所有县）
  baseline_pred <- predict_by_cluster(cluster_bns, cluster_ids, target_nodes, baseline_factors)

  for (p_name in names(policy_configs)) {
    pol_cfg <- policy_configs[[p_name]]
    base_mults <- 1 + pol_cfg / 12

    for (alloc in allocation_strategies) {
      combo_idx <- combo_idx + 1L

      if (combo_idx %% 100 == 0 || combo_idx == 1) {
        cat(sprintf("  combo %d/%d: %s / %s / %s\n", combo_idx, total_combos, e_name, p_name, alloc))
      }

      # 向量化施加政策
      policy_data <- external_data
      for (d in 1:4) {
        if (base_mults[d] > 1) {
          policy_data[, var_groups[[d]]] <- external_data[, var_groups[[d]]] * base_mults[d]
        }
      }

      # 向量化施加空间分配
      final_data <- policy_data
      if (alloc != "统一投资") {
        for (d in 1:4) {
          if (base_mults[d] > 1) {
            spatial_mults <- compute_spatial_mults(alloc, d, base_mults[d],
                                                   policy_data, external_data, var_groups[[d]])
            # sweep: 每行乘以各自的 spatial_mults 值
            final_data[, var_groups[[d]]] <- sweep(
              as.matrix(external_data[, var_groups[[d]]]), 1, spatial_mults, `*`
            )
          }
        }
      }

      # 计算干预因子
      intervention_factors <- calculate_ml_guided_factors(final_data, verbose = FALSE)

      # 分群BN预测（向量化矩阵运算）
      intervention_pred <- predict_by_cluster(cluster_bns, cluster_ids, target_nodes, intervention_factors)

      # 向量化计算效应
      # 政策边际效应（线性BN下不随外部环境变化，但仍记录）
      policy_effect_CPI <- intervention_pred$CPI - baseline_pred$CPI
      policy_effect_CPS <- intervention_pred$CPS - baseline_pred$CPS
      policy_effect_CPA <- intervention_pred$CPA - baseline_pred$CPA

      # 总变化 = 干预预测 - 原始观测值（同时包含外部环境和政策效应）
      total_change_CPI <- intervention_pred$CPI - combined_data$CPI
      total_change_CPS <- intervention_pred$CPS - combined_data$CPS
      total_change_CPA <- intervention_pred$CPA - combined_data$CPA

      all_results[[combo_idx]] <- data.frame(
        External_Config     = e_name,
        Policy_Config       = p_name,
        Allocation_Strategy = alloc,
        County_Code         = combined_data$Code,
        Cluster_ID          = cluster_ids,

        Observed_CPI = combined_data$CPI,
        Observed_CPS = combined_data$CPS,
        Observed_CPA = combined_data$CPA,

        BN_CPI_Baseline   = baseline_pred$CPI,
        BN_CPS_Baseline   = baseline_pred$CPS,
        BN_CPA_Baseline   = baseline_pred$CPA,

        BN_CPI_Prediction = intervention_pred$CPI,
        BN_CPS_Prediction = intervention_pred$CPS,
        BN_CPA_Prediction = intervention_pred$CPA,

        BN_CPI_Policy_Effect = policy_effect_CPI,
        BN_CPS_Policy_Effect = policy_effect_CPS,
        BN_CPA_Policy_Effect = policy_effect_CPA,

        BN_CPI_Total_Change = total_change_CPI,
        BN_CPS_Total_Change = total_change_CPS,
        BN_CPA_Total_Change = total_change_CPA,

        BN_CPI_SD         = intervention_pred$CPI_sd,
        BN_CPS_SD         = intervention_pred$CPS_sd,
        BN_CPA_SD         = intervention_pred$CPA_sd,

        stringsAsFactors = FALSE
      )
    }
  }
}

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nsimulation done in %.1f min (%d combos)\n", as.numeric(elapsed), combo_idx))

# =============================================================================
# 合并 & 保存
# =============================================================================

cat("\nbinding results ...\n")
results_df <- do.call(rbind, all_results)
cat(sprintf("result: %d rows x %d cols\n", nrow(results_df), ncol(results_df)))

out_file_csv <- "output/csv/scenario_results_cluster_bn.csv"
out_file_rds <- "output/csv/scenario_results_cluster_bn.RDS"

write.csv(results_df, out_file_csv, row.names = FALSE)
cat(sprintf("saved: %s\n", out_file_csv))

saveRDS(results_df, out_file_rds)
cat(sprintf("saved: %s\n", out_file_rds))

# 简要统计
cat("\n=== Policy Marginal Effects (by external config) ===\n")
policy_summary <- results_df %>%
  group_by(.data$External_Config) %>%
  summarise(
    Mean_CPI_PolicyEff = mean(.data$BN_CPI_Policy_Effect, na.rm = TRUE),
    Mean_CPS_PolicyEff = mean(.data$BN_CPS_Policy_Effect, na.rm = TRUE),
    Mean_CPA_PolicyEff = mean(.data$BN_CPA_Policy_Effect, na.rm = TRUE),
    .groups = "drop"
  )
print(policy_summary)

cat("\n=== Total Change from Observed (by external config) ===\n")
total_summary <- results_df %>%
  group_by(.data$External_Config) %>%
  summarise(
    Mean_CPI_Total = mean(.data$BN_CPI_Total_Change, na.rm = TRUE),
    Mean_CPS_Total = mean(.data$BN_CPS_Total_Change, na.rm = TRUE),
    Mean_CPA_Total = mean(.data$BN_CPA_Total_Change, na.rm = TRUE),
    .groups = "drop"
  )
print(total_summary)

cat("\n=== 04 complete ===\n")
