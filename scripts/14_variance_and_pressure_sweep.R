#!/usr/bin/env Rscript
# =============================================================================
# 14 — 县级效应的类型间方差分解；外部压力连续梯度（无投资）
# =============================================================================
# 由 12c_si_figures.R 中的两段计算抽出（2026-10-27），12c 的旧版 SI 图已由 31 取代。
# 输出：
#   output/csv/cate_variance_decomposition.csv  表 S4、结果第 1 节（类型解释的方差比例）
#   output/csv/external_multiplier_sweep.csv    图 7A
# =============================================================================

suppressMessages({ library(dplyr); library(tidyr) })
source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")

cn2en <- c("保护政策" = "Conservation", "土地利用" = "Landscape",
           "利用强度" = "Use intensity", "产权制度" = "Soil-water")

# ---- 方差分解 ------------------------------------------------------------------
tau <- read.csv("models/causal_forest_results/cf_tau_matrix.csv")
asg <- read.csv("models/cluster_results/cluster_assignments.csv")
tau_long <- merge(tau, asg[, c("Code", "Cluster")], by = "Code") %>%
  pivot_longer(-c(Code, Cluster), names_to = "Effect", values_to = "tau") %>%
  mutate(Effect_label = paste0(cn2en[gsub("_on_.*", "", Effect)], " \u2192 ", gsub(".*_on_", "", Effect)))

var_decomp <- tau_long %>%
  group_by(Effect_label) %>%
  summarise(total_var = var(tau),
            between_var = {
              m <- tapply(tau, Cluster, mean); n <- tapply(tau, Cluster, length)
              sum(n * (m - mean(tau))^2) / (sum(n) - 1)
            }, .groups = "drop") %>%
  mutate(pct_between = between_var / total_var * 100)
write.csv(var_decomp, "output/csv/cate_variance_decomposition.csv", row.names = FALSE)

# ---- 外部压力梯度 ----------------------------------------------------------------
# 城镇化乘数 0.7–1.5；气候乘数沿 E1/E2/E3 锚点 (0.85,0.90)、(1,1)、(1.30,1.10) 线性内插外推，
# 使 urb = 1 恰为现状。参照为模型现状预测而非观测值（样本内残差均值为零，会把曲线压过零点）。
basic <- read.csv("data/basic_data.csv")
cpi_mean <- read.csv("data/CPI_20year_mean_by_county.csv")
combined <- merge(basic, cpi_mean[, c("Code", "CPI_20year_mean")], by = "Code", all.x = TRUE)
cl <- asg$Cluster[match(combined$Code, asg$Code)]
bns <- lapply(seq_len(max(cl)), function(k) readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k)))
urb <- grep("URB", names(combined), value = TRUE)
clim <- grep("Clim", names(combined), value = TRUE)
clim_multiplier <- function(u) ifelse(u <= 1, 1 + (u - 1) * (0.10 / 0.15), 1 + (u - 1) * (0.10 / 0.30))
targets <- c("CPI", "CPS", "CPA")
ref <- predict_by_cluster(bns, cl, targets, calculate_ml_guided_factors(combined, verbose = FALSE))

sweep <- bind_rows(lapply(seq(0.7, 1.5, by = 0.05), function(u) {
  d <- combined
  d[, urb] <- combined[, urb] * u
  d[, clim] <- combined[, clim] * clim_multiplier(u)
  p <- predict_by_cluster(bns, cl, targets, calculate_ml_guided_factors(d, verbose = FALSE))
  data.frame(urb_multiplier = u, clim_multiplier = clim_multiplier(u),
             CPI_mean = mean(p$CPI - ref$CPI), CPS_mean = mean(p$CPS - ref$CPS), CPA_mean = mean(p$CPA - ref$CPA))
}))
write.csv(sweep, "output/csv/external_multiplier_sweep.csv", row.names = FALSE)
