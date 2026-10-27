#!/usr/bin/env Rscript
# =============================================================================
# 13 — 消融：把驱动研究里的异质性拿掉，模拟的答案会不会变
# =============================================================================
#
# Full : 按 τ̂ 剖面分 7 簇，各簇一个 BN（主管线）
# A1   : 全国一个 BN（常规做法：一套全国系数）
# A2   : 按农业区划（Zones，8 区）分组各拟合一个 BN（按地理分，不按响应分）
#
# 三组用同一个修正 DAG、同一套情景。只看现状环境：线性 BN 中政策效应不随外部环境变。
# 效应 = 干预预测 − 同一模型的现状预测。
#
# 输出：output/csv/ablation_summary.csv, ablation_config_metrics.csv,
#       ablation_group_synergy.csv
# =============================================================================

suppressMessages({ library(bnlearn); library(dplyr) })
source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")
source("lib/scenario_functions.R")

basic_data <- read.csv("data/basic_data.csv")
cpi_mean   <- read.csv("data/CPI_20year_mean_by_county.csv")
combined   <- merge(basic_data, cpi_mean[, c("Code", "CPI_20year_mean")], by = "Code", all.x = TRUE)
assign_df  <- read.csv("models/cluster_results/cluster_assignments.csv")
dag        <- readRDS("models/corrected_bn/corrected_dag.RDS")

factors <- calculate_ml_guided_factors(combined, verbose = FALSE)
network_data <- data.frame(
  城市化 = factors$城市化, 气候变化 = factors$气候变化, 保护政策 = factors$保护政策,
  土地利用 = factors$土地利用, 利用强度 = factors$利用强度, 产权制度 = factors$产权制度,
  CPI = combined$CPI, CPS = combined$CPS, CPA = combined$CPA
)
network_data[] <- lapply(network_data, as.numeric)

tau_ids  <- assign_df$Cluster[match(combined$Code, assign_df$Code)]
zone_ids <- as.integer(factor(combined$Zones))
stopifnot(!any(is.na(tau_ids)), !any(is.na(zone_ids)))

arms <- list(
  Full = list(bns = lapply(seq_len(max(tau_ids)), function(k)
                readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k))),
              ids = tau_ids),
  A1_national = list(bns = list(bn.fit(dag, network_data, method = "mle-g")),
                     ids = rep(1L, nrow(combined))),
  A2_zones = list(bns = fit_cluster_bns(dag, network_data, zone_ids), ids = zone_ids)
)

# ---- 模拟 -------------------------------------------------------------------

sims <- lapply(names(arms), function(a) {
  cat(sprintf("simulating %s ...\n", a))
  s <- simulate_policy_effects(arms[[a]]$bns, arms[[a]]$ids, combined)
  s$Arm <- a
  s
})
sims <- do.call(rbind, sims)
sims$Synergy <- sims$dCPI > 0 & sims$dCPS > 0 & sims$dCPA > 0

# 主管线自检：Full 与 04 的现状环境政策效应一致
chk <- readRDS("output/csv/scenario_results_cluster_bn.RDS") %>%
  filter(External_Config == "E2", Policy_Config == "I04", Allocation_Strategy == "统一投资")
f04 <- sims %>% filter(Arm == "Full", Policy_Config == "I04", Allocation_Strategy == "统一投资")
dev <- max(abs(f04$dCPS - chk$BN_CPS_Policy_Effect[match(f04$County_Code, chk$County_Code)]))
cat(sprintf("Full vs 04 (I04, uniform, CPS) max abs diff: %.2e\n", dev))
stopifnot(dev < 1e-8)

# ---- 指标 -------------------------------------------------------------------

scen <- sims %>%
  group_by(Arm, Policy_Config, Allocation_Strategy) %>%
  summarise(CPI = mean(dCPI), CPS = mean(dCPS), CPA = mean(dCPA),
            Synergy = mean(Synergy) * 100, .groups = "drop")

config_metrics <- scen %>%
  group_by(Arm, Policy_Config) %>%
  summarise(CPI = mean(CPI), CPS = mean(CPS), CPA = mean(CPA),
            Synergy = mean(Synergy), .groups = "drop") %>%
  group_by(Arm) %>%
  mutate(Rank_Mean = rank(-(CPI + CPS + CPA)), Rank_Synergy = rank(-Synergy)) %>%
  ungroup()

ranges <- function(df, by) {
  df %>% group_by(Arm, .data[[by]]) %>%
    summarise(across(c(CPI, CPS, CPA, Synergy), mean), .groups = "drop") %>%
    group_by(Arm) %>%
    summarise(across(c(CPI, CPS, CPA, Synergy), ~ diff(range(.x))), .groups = "drop")
}
sec <- ranges(scen, "Policy_Config")
spa <- ranges(scen, "Allocation_Strategy")

top_of <- function(arm, col) {
  d <- config_metrics[config_metrics$Arm == arm, ]
  paste(d$Policy_Config[order(d[[col]])][1:3], collapse = " ")
}

i04_spatial <- scen %>% filter(Policy_Config == "I04") %>%
  group_by(Arm) %>% summarise(I04_spatial_synergy_range = diff(range(Synergy)), .groups = "drop")

group_syn <- sims %>%
  group_by(Arm, Policy_Config, Group) %>%
  summarise(Synergy = mean(Synergy) * 100, n = n_distinct(County_Code), .groups = "drop")
group_spread <- group_syn %>% filter(Policy_Config %in% c("I04", "I12")) %>%
  group_by(Arm, Policy_Config) %>%
  summarise(spread = sprintf("%.0f-%.0f", min(Synergy), max(Synergy)), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Policy_Config, values_from = spread,
                     names_prefix = "Group_synergy_")

summary_df <- data.frame(Arm = names(arms)) %>%
  left_join(sec %>% transmute(Arm, Sec_CPI = CPI, Sec_CPS = CPS, Sec_CPA = CPA, Sec_Syn = Synergy), by = "Arm") %>%
  left_join(spa %>% transmute(Arm, Spa_CPI = CPI, Spa_CPS = CPS, Spa_CPA = CPA, Spa_Syn = Synergy), by = "Arm") %>%
  mutate(Ratio_CPI = Sec_CPI / Spa_CPI, Ratio_CPS = Sec_CPS / Spa_CPS, Ratio_CPA = Sec_CPA / Spa_CPA,
         Ratio_Syn = Sec_Syn / Spa_Syn) %>%
  left_join(i04_spatial, by = "Arm") %>%
  left_join(group_spread, by = "Arm")
summary_df$Top3_by_mean    <- vapply(summary_df$Arm, top_of, "", col = "Rank_Mean")
summary_df$Top3_by_synergy <- vapply(summary_df$Arm, top_of, "", col = "Rank_Synergy")

write.csv(summary_df, "output/csv/ablation_summary.csv", row.names = FALSE)
write.csv(config_metrics, "output/csv/ablation_config_metrics.csv", row.names = FALSE)
write.csv(group_syn, "output/csv/ablation_group_synergy.csv", row.names = FALSE)

options(width = 200)
print(as.data.frame(summary_df), digits = 3)
cat("\nconfig synergy (%) by arm:\n")
print(as.data.frame(tidyr::pivot_wider(config_metrics[, c("Arm", "Policy_Config", "Synergy")],
                                       names_from = Arm, values_from = Synergy)), digits = 3)
cat("\n=== 13 complete ===\n")
