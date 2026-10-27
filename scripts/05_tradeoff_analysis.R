#!/usr/bin/env Rscript
# =============================================================================
# 05 — CPI/CPS/CPA 权衡与协同分析
# =============================================================================
#
# 输入：output/csv/scenario_results_cluster_bn.csv
# 输出：output/csv/ 下的权衡/协同分类结果
#
# 分类用 Total_Change = 干预预测 - 模型现状基线（E2、无干预），
# 含外部环境效应与政策效应，不含拟合残差（见 lib/change_reference.R）。
# 线性BN中政策效应不随外部环境变化，随环境变化的只有外部项。
#
# =============================================================================

library(dplyr)
library(tidyr)

source("lib/change_reference.R")

cat("=== 05: trade-off / synergy analysis ===\n\n")

# ---- 1. 加载数据 ------------------------------------------------------------

results_df <- readRDS("output/csv/scenario_results_cluster_bn.RDS")
results_df <- add_model_baseline_change(results_df)
cat(sprintf("loaded: %d rows\n", nrow(results_df)))

# ---- 2. 向量化分类（基于 Total_Change） ------------------------------------

pos_CPI <- results_df$BN_CPI_Total_Change > 0
pos_CPS <- results_df$BN_CPS_Total_Change > 0
pos_CPA <- results_df$BN_CPA_Total_Change > 0

n_positive <- as.integer(pos_CPI) + as.integer(pos_CPS) + as.integer(pos_CPA)

results_df$Tradeoff_Class <- ifelse(
  n_positive == 3, "full_synergy",
  ifelse(n_positive == 2, "partial_synergy",
         ifelse(n_positive == 1, "partial_tradeoff", "full_negative"))
)

results_df$Improving <- ifelse(
  n_positive == 3, "CPI+CPS+CPA",
  ifelse(n_positive == 0, "none",
         paste0(
           ifelse(pos_CPI, "CPI", ""),
           ifelse(pos_CPS, "+CPS", ""),
           ifelse(pos_CPA, "+CPA", "")
         )
  )
)
results_df$Improving <- gsub("^\\+", "", results_df$Improving)

cat("classification done (based on Total_Change = prediction - model status-quo baseline)\n")
cat(sprintf("  full_synergy:     %d (%.1f%%)\n",
            sum(n_positive == 3), 100 * mean(n_positive == 3)))
cat(sprintf("  partial_synergy:  %d (%.1f%%)\n",
            sum(n_positive == 2), 100 * mean(n_positive == 2)))
cat(sprintf("  partial_tradeoff: %d (%.1f%%)\n",
            sum(n_positive == 1), 100 * mean(n_positive == 1)))
cat(sprintf("  full_negative:    %d (%.1f%%)\n",
            sum(n_positive == 0), 100 * mean(n_positive == 0)))

# ---- 3. 按情景组合聚合 ------------------------------------------------------

by_scenario <- results_df %>%
  group_by(.data$External_Config, .data$Policy_Config, .data$Allocation_Strategy) %>%
  summarise(
    N = n(),
    Pct_Full_Synergy     = mean(.data$Tradeoff_Class == "full_synergy") * 100,
    Pct_Partial_Synergy  = mean(.data$Tradeoff_Class == "partial_synergy") * 100,
    Pct_Partial_Tradeoff = mean(.data$Tradeoff_Class == "partial_tradeoff") * 100,
    Pct_Full_Negative    = mean(.data$Tradeoff_Class == "full_negative") * 100,
    Mean_CPI_Total = mean(.data$BN_CPI_Total_Change, na.rm = TRUE),
    Mean_CPS_Total = mean(.data$BN_CPS_Total_Change, na.rm = TRUE),
    Mean_CPA_Total = mean(.data$BN_CPA_Total_Change, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 4. 按政策聚合 ----------------------------------------------------------

by_policy <- results_df %>%
  group_by(.data$Policy_Config) %>%
  summarise(
    Pct_Full_Synergy     = mean(.data$Tradeoff_Class == "full_synergy") * 100,
    Pct_Partial_Synergy  = mean(.data$Tradeoff_Class == "partial_synergy") * 100,
    Pct_Partial_Tradeoff = mean(.data$Tradeoff_Class == "partial_tradeoff") * 100,
    Pct_Full_Negative    = mean(.data$Tradeoff_Class == "full_negative") * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(.data$Pct_Full_Synergy))

# ---- 5. 按空间分配策略聚合 --------------------------------------------------

by_allocation <- results_df %>%
  group_by(.data$Allocation_Strategy) %>%
  summarise(
    Pct_Full_Synergy     = mean(.data$Tradeoff_Class == "full_synergy") * 100,
    Pct_Partial_Synergy  = mean(.data$Tradeoff_Class == "partial_synergy") * 100,
    Pct_Partial_Tradeoff = mean(.data$Tradeoff_Class == "partial_tradeoff") * 100,
    Pct_Full_Negative    = mean(.data$Tradeoff_Class == "full_negative") * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(.data$Pct_Full_Synergy))

# ---- 6. 按群组聚合 ----------------------------------------------------------

by_cluster <- results_df %>%
  group_by(.data$Cluster_ID) %>%
  summarise(
    N_Counties = n_distinct(.data$County_Code),
    Pct_Full_Synergy     = mean(.data$Tradeoff_Class == "full_synergy") * 100,
    Pct_Partial_Synergy  = mean(.data$Tradeoff_Class == "partial_synergy") * 100,
    Pct_Partial_Tradeoff = mean(.data$Tradeoff_Class == "partial_tradeoff") * 100,
    Pct_Full_Negative    = mean(.data$Tradeoff_Class == "full_negative") * 100,
    Mean_CPI_Total = mean(.data$BN_CPI_Total_Change, na.rm = TRUE),
    Mean_CPS_Total = mean(.data$BN_CPS_Total_Change, na.rm = TRUE),
    Mean_CPA_Total = mean(.data$BN_CPA_Total_Change, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 7. 外部环境效应对比（关键新增） ----------------------------------------

by_external <- results_df %>%
  group_by(.data$External_Config) %>%
  summarise(
    Pct_Full_Synergy     = mean(.data$Tradeoff_Class == "full_synergy") * 100,
    Pct_Full_Negative    = mean(.data$Tradeoff_Class == "full_negative") * 100,
    Mean_CPI_Total = mean(.data$BN_CPI_Total_Change, na.rm = TRUE),
    Mean_CPS_Total = mean(.data$BN_CPS_Total_Change, na.rm = TRUE),
    Mean_CPA_Total = mean(.data$BN_CPA_Total_Change, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 8. 权衡情景中的效应量级分析 --------------------------------------------

tradeoff_subset <- results_df %>%
  filter(.data$Tradeoff_Class %in% c("partial_synergy", "partial_tradeoff"))

if (nrow(tradeoff_subset) > 0) {
  tradeoff_magnitude <- tradeoff_subset %>%
    group_by(.data$Policy_Config, .data$Allocation_Strategy) %>%
    summarise(
      Mean_CPI = mean(.data$BN_CPI_Total_Change, na.rm = TRUE),
      Mean_CPS = mean(.data$BN_CPS_Total_Change, na.rm = TRUE),
      Mean_CPA = mean(.data$BN_CPA_Total_Change, na.rm = TRUE),
      Max_Gain = pmax(.data$Mean_CPI, .data$Mean_CPS, .data$Mean_CPA),
      Max_Loss = pmin(.data$Mean_CPI, .data$Mean_CPS, .data$Mean_CPA),
      Gain_Loss_Ratio = abs(.data$Max_Gain / pmin(.data$Max_Loss, -1e-10)),
      .groups = "drop"
    ) %>%
    arrange(desc(.data$Gain_Loss_Ratio))
} else {
  tradeoff_magnitude <- data.frame()
}

# ---- 9. 保存 ----------------------------------------------------------------

write.csv(results_df[, c("External_Config", "Policy_Config", "Allocation_Strategy",
                          "County_Code", "Cluster_ID", "Tradeoff_Class", "Improving",
                          "BN_CPI_Total_Change", "BN_CPS_Total_Change", "BN_CPA_Total_Change")],
          "output/csv/tradeoff_county_level.csv", row.names = FALSE)
cat("saved: tradeoff_county_level.csv\n")

write.csv(by_scenario, "output/csv/tradeoff_by_scenario.csv", row.names = FALSE)
cat("saved: tradeoff_by_scenario.csv\n")

write.csv(by_policy, "output/csv/tradeoff_by_policy.csv", row.names = FALSE)
cat("saved: tradeoff_by_policy.csv\n")

write.csv(by_allocation, "output/csv/tradeoff_by_allocation.csv", row.names = FALSE)
cat("saved: tradeoff_by_allocation.csv\n")

write.csv(by_cluster, "output/csv/tradeoff_by_cluster.csv", row.names = FALSE)
cat("saved: tradeoff_by_cluster.csv\n")

write.csv(by_external, "output/csv/tradeoff_by_external.csv", row.names = FALSE)
cat("saved: tradeoff_by_external.csv\n")

if (nrow(tradeoff_magnitude) > 0) {
  write.csv(tradeoff_magnitude, "output/csv/tradeoff_magnitude.csv", row.names = FALSE)
  cat("saved: tradeoff_magnitude.csv\n")
}

# ---- 10. 打印汇总 -----------------------------------------------------------

cat("\n=== External Environment Effect on Trade-offs ===\n")
print(by_external)

cat("\n=== Top 5 Policies by Full Synergy Rate ===\n")
print(head(by_policy, 5))

cat("\n=== Cluster-level Trade-off Patterns ===\n")
print(by_cluster)

cat("\n=== 05 complete ===\n")
