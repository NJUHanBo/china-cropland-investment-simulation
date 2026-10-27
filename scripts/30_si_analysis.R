# =============================================================================
# 30_si_analysis.R — 补充材料所需的全部数值表（output/si/*.csv）
#
# 输入：当前版本的模型与情景结果（04、08a–d、11、13 的输出）
# 输出：
#   indicator_weights.csv     表 S1 的指标权重（四个结果上 ML 权重的均值，维度内归一）
#   robustness_confounding.csv 表 S4：sensemakr 稳健性值 + 按 1 s.d. 处理变化重算的 E 值
#   clustering.csv / type_sizes.csv  表 S5
#   dag_links.csv             表 S6
#   perturbation_summary.csv  表 S7：系数扰动下组合排序与三维共赢比例
#   calibration_by_type.csv   图 S1：网络现状预测 vs 观测
#   targeting_range.csv       图 S7：各组合在 21 种空间配置下的三维共赢比例
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("lib/ml_guided_factor_calculation.R")
source("lib/scenario_functions.R")

dir.create("output/si", showWarnings = FALSE, recursive = TRUE)
DIM_EN <- c("保护政策" = "Conservation", "土地利用" = "Landscape pattern",
            "利用强度" = "Use intensity", "产权制度" = "Soil–water conditions",
            "城市化" = "Urbanisation", "气候变化" = "Climate change")
NODE_EN <- c(DIM_EN, CPI = "Productivity change (CPI)", CPS = "Productivity stability (CPS)",
             CPA = "Productivity acceleration (CPA)")

basic    <- read.csv("data/basic_data.csv", check.names = FALSE)
cpi_mean <- read.csv("data/CPI_20year_mean_by_county.csv")
combined <- merge(basic, cpi_mean[, c("Code", "CPI_20year_mean")], by = "Code", all.x = TRUE)
assign   <- read.csv("models/cluster_results/cluster_assignments.csv")

# ---- 表 S1：指标权重 ---------------------------------------------------------
w <- read.csv("models/stage3_factor_weights_detail.csv", stringsAsFactors = FALSE)
wt <- w %>% group_by(Dimension, Feature) %>% summarise(Weight = mean(ML_Weight), .groups = "drop") %>%
  group_by(Dimension) %>% mutate(Weight = Weight / sum(Weight)) %>% ungroup() %>%
  mutate(Dimension_EN = DIM_EN[Dimension])
write.csv(wt, "output/si/indicator_weights.csv", row.names = FALSE)

# ---- 表 S4：未观测混杂的敏感性 ------------------------------------------------
# 原 11 号脚本按"处理变量 0→1（整个取值范围）"标准化，d 可达 6–7，E 值失去解释力。
# 这里改为处理变量变化一个标准差：d = ATE × s.d.(W) / s.d.(Y)，RR ≈ exp(0.91|d|)。
fac <- calculate_ml_guided_factors(combined, verbose = FALSE)
ate <- read.csv("models/causal_forest_results/cf_ate_summary.csv", stringsAsFactors = FALSE)
rv  <- read.csv("output/csv/oster_evalue_sensitivity.csv", stringsAsFactors = FALSE)
ev_fun <- function(rr) ifelse(rr <= 1, 1, rr + sqrt(rr * (rr - 1)))
s4 <- ate %>% rowwise() %>% mutate(
  sd_W = sd(fac[[Treatment]], na.rm = TRUE),
  sd_Y = sd(combined[[Outcome]], na.rm = TRUE),
  d_1sd = ATE * sd_W / sd_Y,
  d_lo  = abs(d_1sd) - 1.96 * ATE_SE * sd_W / sd_Y,
  RR    = exp(0.91 * abs(d_1sd)),
  E_point = ev_fun(RR),
  E_CI    = ifelse(d_lo > 0, ev_fun(exp(0.91 * d_lo)), 1)) %>% ungroup() %>%
  left_join(rv %>% select(Label, RV_q, RV_qa), by = c("Model_Label" = "Label")) %>%
  mutate(Treatment_EN = DIM_EN[Treatment], Outcome_EN = NODE_EN[Outcome])
write.csv(s4, "output/si/robustness_confounding.csv", row.names = FALSE)

# ---- 表 S5：聚类诊断与类型规模 ------------------------------------------------
file.copy("models/cluster_results/cluster_diagnostics.csv", "output/si/clustering.csv", overwrite = TRUE)
ts <- assign %>% count(Cluster, name = "n") %>% mutate(share = n / sum(n) * 100)
zones <- basic %>% select(Code, Zones) %>% left_join(assign %>% select(Code, Cluster), by = "Code") %>%
  count(Cluster, Zones) %>% group_by(Cluster) %>% slice_max(n, n = 2, with_ties = FALSE) %>%
  summarise(Main_zones = paste(sprintf("%s (%d)", Zones, n), collapse = "; "), .groups = "drop")
write.csv(left_join(ts, zones, by = "Cluster"), "output/si/type_sizes.csv", row.names = FALSE)

# ---- 表 S6：因果图连边 -------------------------------------------------------
dag <- read.csv("models/corrected_bn/dag_correction_report.csv", stringsAsFactors = FALSE)
cfa <- ate %>% select(Treatment, Outcome, ATE, CI_Lower, CI_Upper)
s6 <- dag %>% select(From, To, CF_Status) %>% left_join(cfa, by = c("From" = "Treatment", "To" = "Outcome")) %>%
  mutate(From_EN = NODE_EN[From], To_EN = NODE_EN[To],
         Link_class = case_when(From %in% c("CPI", "CPS", "CPA") ~ "Productivity chain",
                                To %in% c("CPI", "CPS", "CPA") ~ "Driver → productivity",
                                TRUE ~ "Driver → driver"),
         In_prior = CF_Status != "added_by_cf", In_final = CF_Status != "removed_by_cf")
write.csv(s6, "output/si/dag_links.csv", row.names = FALSE)
cat(sprintf("DAG: prior %d, removed %d, added %d, final %d\n", sum(s6$In_prior),
            sum(s6$CF_Status == "removed_by_cf"), sum(s6$CF_Status == "added_by_cf"), sum(s6$In_final)))
print(table(s6$Link_class[s6$In_prior]))

# ---- 情景结果（现状压力） -----------------------------------------------------
res <- readRDS("output/csv/scenario_results_cluster_bn.RDS") %>%
  filter(External_Config == "E2", Policy_Config != "Baseline")
pe <- res %>% mutate(tg = BN_CPI_Policy_Effect > 0 & BN_CPS_Policy_Effect > 0 & BN_CPA_Policy_Effect > 0,
                     s = BN_CPI_Policy_Effect + BN_CPS_Policy_Effect + BN_CPA_Policy_Effect)
by_ps <- pe %>% group_by(Policy_Config, Allocation_Strategy) %>%
  summarise(Mean_gain = mean(s), Triple_gain = mean(tg) * 100, .groups = "drop")
write.csv(by_ps, "output/si/targeting_range.csv", row.names = FALSE)
rng <- by_ps %>% group_by(Policy_Config) %>%
  summarise(TG_min = min(Triple_gain), TG_max = max(Triple_gain), TG_range = TG_max - TG_min,
            Gain_range = max(Mean_gain) - min(Mean_gain), .groups = "drop")
cat("Triple-gain range across 21 strategies (pp): median", median(rng$TG_range),
    " min", min(rng$TG_range), " max", max(rng$TG_range), "\n")

# ---- 表 S7：系数扰动 ---------------------------------------------------------
ref <- by_ps %>% filter(Allocation_Strategy == "统一投资") %>%
  transmute(Policy = Policy_Config, Ref_gain = Mean_gain, Ref_TG = Triple_gain,
            Ref_rank = rank(-Ref_gain))
draws <- readRDS("output/csv/sensitivity/bootstrap_perturbation_draws.RDS")
pert <- bind_rows(lapply(draws, function(br) {
  rk <- t(apply(br$rankings, 1, function(x) rank(-x)))
  rho <- apply(br$rankings, 1, function(x) cor(x, ref$Ref_gain[match(colnames(br$rankings), ref$Policy)],
                                                method = "spearman"))
  top_ref <- ref$Policy[which.min(ref$Ref_rank)]
  data.frame(Noise = br$noise, Policy = colnames(br$rankings),
             Gain_lo = apply(br$rankings, 2, quantile, 0.025), Gain_hi = apply(br$rankings, 2, quantile, 0.975),
             Rank_lo = apply(rk, 2, quantile, 0.025), Rank_hi = apply(rk, 2, quantile, 0.975),
             TG_lo = apply(br$synergy, 2, quantile, 0.025), TG_hi = apply(br$synergy, 2, quantile, 0.975),
             Rho_median = median(rho), Rho_q025 = quantile(rho, 0.025),
             Top_kept = mean(rk[, top_ref] == 1) * 100)
}))
pert <- left_join(pert, ref, by = "Policy")
write.csv(pert, "output/si/perturbation_summary.csv", row.names = FALSE)
print(pert %>% distinct(Noise, Rho_median, Rho_q025, Top_kept))

# 两个评价标准是否仍选出不同组合：每次扰动后共赢比例第一名是否仍不是均值第一名
crit <- bind_rows(lapply(draws, function(br) {
  top_mean  <- colnames(br$rankings)[apply(br$rankings, 1, which.max)]
  top_broad <- colnames(br$synergy)[apply(br$synergy, 1, which.max)]
  tb <- sort(table(top_broad), decreasing = TRUE)
  data.frame(Noise = br$noise, Pct_criteria_differ = mean(top_mean != top_broad) * 100,
             Breadth_leaders = paste(sprintf("%s %d%%", names(tb), as.integer(tb)), collapse = "; "))
}))
write.csv(crit, "output/si/perturbation_criteria.csv", row.names = FALSE)
print(crit)

# ---- 图 S1：校准 -------------------------------------------------------------
cal <- res %>% filter(Policy_Config == "I01", Allocation_Strategy == "统一投资") %>%
  select(County_Code, Cluster_ID, starts_with("Observed_"), ends_with("_Baseline"))
cal_long <- bind_rows(lapply(c("CPI", "CPS", "CPA"), function(y)
  data.frame(Code = cal$County_Code, Type = cal$Cluster_ID, Outcome = y,
             Observed = cal[[paste0("Observed_", y)]], Predicted = cal[[paste0("BN_", y, "_Baseline")]])))
write.csv(cal_long, "output/si/calibration_points.csv", row.names = FALSE)
cal_stats <- cal_long %>% group_by(Outcome, Type) %>%
  summarise(R2 = cor(Observed, Predicted)^2, RMSE = sqrt(mean((Observed - Predicted)^2)), n = n(), .groups = "drop")
write.csv(cal_stats, "output/si/calibration_by_type.csv", row.names = FALSE)
print(cal_stats %>% group_by(Outcome) %>% summarise(R2_min = min(R2), R2_max = max(R2)))
print(cal_long %>% group_by(Outcome) %>% summarise(R2 = cor(Observed, Predicted)^2, resid_sd = sd(Observed - Predicted)))
cat("=== 30 complete ===\n")
