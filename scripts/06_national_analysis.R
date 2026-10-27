#!/usr/bin/env Rscript
# =============================================================================
# 06 — 全国层面聚合分析
# =============================================================================
#
# 输入：output/csv/scenario_results_cluster_bn.csv
#       output/csv/tradeoff_county_level.csv
# 输出：output/csv/ 下的全国层面汇总
#
# =============================================================================

library(dplyr)
library(tidyr)

source("lib/change_reference.R")

cat("=== 06: national aggregation analysis ===\n\n")

# ---- 1. 加载数据 ------------------------------------------------------------

results_df  <- add_model_baseline_change(readRDS("output/csv/scenario_results_cluster_bn.RDS"))
tradeoff_df <- read.csv("output/csv/tradeoff_county_level.csv")

cat(sprintf("results: %d rows, tradeoff: %d rows\n", nrow(results_df), nrow(tradeoff_df)))

# ---- 2. 全国层面三指标效应（使用 Total_Change） ----------------------------

national_effects <- results_df %>%
  group_by(.data$External_Config, .data$Policy_Config, .data$Allocation_Strategy) %>%
  summarise(
    CPI_Mean_Total = mean(.data$BN_CPI_Total_Change, na.rm = TRUE),
    CPS_Mean_Total = mean(.data$BN_CPS_Total_Change, na.rm = TRUE),
    CPA_Mean_Total = mean(.data$BN_CPA_Total_Change, na.rm = TRUE),
    CPI_Sum_Total  = sum(.data$BN_CPI_Total_Change, na.rm = TRUE),
    CPS_Sum_Total  = sum(.data$BN_CPS_Total_Change, na.rm = TRUE),
    CPA_Sum_Total  = sum(.data$BN_CPA_Total_Change, na.rm = TRUE),
    CPI_SD_Total   = sd(.data$BN_CPI_Total_Change, na.rm = TRUE),
    CPS_SD_Total   = sd(.data$BN_CPS_Total_Change, na.rm = TRUE),
    CPA_SD_Total   = sd(.data$BN_CPA_Total_Change, na.rm = TRUE),
    CPI_Mean_PolicyEff = mean(.data$BN_CPI_Policy_Effect, na.rm = TRUE),
    CPS_Mean_PolicyEff = mean(.data$BN_CPS_Policy_Effect, na.rm = TRUE),
    CPA_Mean_PolicyEff = mean(.data$BN_CPA_Policy_Effect, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 3. 各指标的策略排名 ----------------------------------------------------

rank_by_indicator <- function(df, indicator) {
  col_sum  <- paste0(indicator, "_Sum_Total")
  col_mean <- paste0(indicator, "_Mean_Total")

  df %>%
    group_by(.data$Allocation_Strategy) %>%
    summarise(
      Mean_Sum_Total  = mean(.data[[col_sum]], na.rm = TRUE),
      Mean_Mean_Total = mean(.data[[col_mean]], na.rm = TRUE),
      SD_Sum_Total    = sd(.data[[col_sum]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Indicator = indicator) %>%
    arrange(desc(.data$Mean_Sum_Total))
}

ranking_CPI <- rank_by_indicator(national_effects, "CPI")
ranking_CPS <- rank_by_indicator(national_effects, "CPS")
ranking_CPA <- rank_by_indicator(national_effects, "CPA")

all_rankings <- bind_rows(ranking_CPI, ranking_CPS, ranking_CPA)

# ---- 4. 跨指标排名一致性 ----------------------------------------------------

rank_wide <- all_rankings %>%
  select("Allocation_Strategy", "Indicator", "Mean_Sum_Total") %>%
  pivot_wider(names_from = .data$Indicator, values_from = .data$Mean_Sum_Total, names_prefix = "Val_")

rank_wide <- rank_wide %>%
  mutate(
    Rank_CPI = rank(-.data$Val_CPI),
    Rank_CPS = rank(-.data$Val_CPS),
    Rank_CPA = rank(-.data$Val_CPA),
    Mean_Rank = (.data$Rank_CPI + .data$Rank_CPS + .data$Rank_CPA) / 3
  ) %>%
  arrange(.data$Mean_Rank)

# ---- 5. 政策配置分析 --------------------------------------------------------

policy_effects <- national_effects %>%
  group_by(.data$Policy_Config) %>%
  summarise(
    CPI_Mean = mean(.data$CPI_Mean_Total, na.rm = TRUE),
    CPS_Mean = mean(.data$CPS_Mean_Total, na.rm = TRUE),
    CPA_Mean = mean(.data$CPA_Mean_Total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$CPI_Mean + .data$CPS_Mean + .data$CPA_Mean))

# ---- 6. 外部环境分析（Total_Change下E1/E2/E3现在有差异） --------------------

external_effects <- national_effects %>%
  group_by(.data$External_Config) %>%
  summarise(
    CPI_Mean_Total = mean(.data$CPI_Mean_Total, na.rm = TRUE),
    CPS_Mean_Total = mean(.data$CPS_Mean_Total, na.rm = TRUE),
    CPA_Mean_Total = mean(.data$CPA_Mean_Total, na.rm = TRUE),
    CPI_Mean_PolicyEff = mean(.data$CPI_Mean_PolicyEff, na.rm = TRUE),
    CPS_Mean_PolicyEff = mean(.data$CPS_Mean_PolicyEff, na.rm = TRUE),
    CPA_Mean_PolicyEff = mean(.data$CPA_Mean_PolicyEff, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 7. 协同最优情景 --------------------------------------------------------

tradeoff_scenario <- tradeoff_df %>%
  group_by(.data$External_Config, .data$Policy_Config, .data$Allocation_Strategy) %>%
  summarise(
    Pct_Full_Synergy = mean(.data$Tradeoff_Class == "full_synergy") * 100,
    .groups = "drop"
  )

best_synergy <- national_effects %>%
  left_join(tradeoff_scenario, by = c("External_Config", "Policy_Config", "Allocation_Strategy")) %>%
  arrange(desc(.data$Pct_Full_Synergy)) %>%
  head(20)

# ---- 8. 保存 ----------------------------------------------------------------

write.csv(national_effects, "output/csv/national_effects_by_indicator.csv", row.names = FALSE)
cat("saved: national_effects_by_indicator.csv\n")

write.csv(all_rankings, "output/csv/strategy_ranking_by_indicator.csv", row.names = FALSE)
cat("saved: strategy_ranking_by_indicator.csv\n")

write.csv(rank_wide, "output/csv/cross_indicator_ranking.csv", row.names = FALSE)
cat("saved: cross_indicator_ranking.csv\n")

write.csv(policy_effects, "output/csv/policy_effects_by_indicator.csv", row.names = FALSE)
cat("saved: policy_effects_by_indicator.csv\n")

write.csv(external_effects, "output/csv/external_effects_by_indicator.csv", row.names = FALSE)
cat("saved: external_effects_by_indicator.csv\n")

write.csv(best_synergy, "output/csv/best_synergy_scenarios.csv", row.names = FALSE)
cat("saved: best_synergy_scenarios.csv\n")

# ---- 9. 打印 ----------------------------------------------------------------

cat("\n=== Top 5 Strategies by Cross-Indicator Mean Rank ===\n")
print(head(rank_wide[, c("Allocation_Strategy", "Rank_CPI", "Rank_CPS", "Rank_CPA", "Mean_Rank")], 5))

cat("\n=== Policy Effects (Total Change) ===\n")
print(policy_effects)

cat("\n=== External Environment Effects ===\n")
print(external_effects)

cat("\n=== 06 complete ===\n")
