# =============================================================================
# 33_si_checks.R — 因果森林诊断与闭式预测核验（补充材料用）
#
#   1. grf 校准检验（test_calibration）：平均效应项与异质性项
#   2. 县级效应 95% 置信区间不含零的比例
#   3. 各路径效应异质性的主要来源（变量重要性，按条件变量）
#   4. 最终 7 个类型网络上：闭式预测 vs 似然加权抽样（cpdist, n = 10,000），含耗时
# 输出：output/si/cf_diagnostics.csv、cf_heterogeneity_sources.csv、closed_form_check.csv
# =============================================================================

suppressPackageStartupMessages({ library(grf); library(bnlearn); library(dplyr) })
source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")

m   <- readRDS("models/causal_forest_results/cf_models.RDS")
tau <- read.csv("models/causal_forest_results/cf_tau_matrix.csv", check.names = FALSE)
se  <- read.csv("models/causal_forest_results/cf_tau_se_matrix.csv", check.names = FALSE)
names(m) <- setdiff(names(tau), "Code")   # 列表无名；顺序与效应矩阵列一致（预测值逐一核对过）

diag <- bind_rows(lapply(names(m), function(k) {
  tc <- test_calibration(m[[k]])
  lo <- tau[[k]] - 1.96 * se[[k]]; hi <- tau[[k]] + 1.96 * se[[k]]
  data.frame(Model = k,
             Mean_coef = tc["mean.forest.prediction", 1], Mean_p = tc["mean.forest.prediction", 4],
             Diff_coef = tc["differential.forest.prediction", 1], Diff_p = tc["differential.forest.prediction", 4],
             Pct_county_pos = mean(lo > 0) * 100, Pct_county_neg = mean(hi < 0) * 100)
}))
write.csv(diag, "output/si/cf_diagnostics.csv", row.names = FALSE)
print(diag, digits = 3)

vi <- read.csv("models/causal_forest_results/cf_variable_importance.csv", stringsAsFactors = FALSE)
src <- vi %>% group_by(Model) %>% slice_max(Importance, n = 1) %>% ungroup()
write.csv(vi, "output/si/cf_heterogeneity_sources.csv", row.names = FALSE)
print(src)
print(vi %>% group_by(Variable) %>% summarise(mean_imp = mean(Importance)) %>% arrange(-mean_imp))

# ---- 闭式预测核验 --------------------------------------------------------------
basic <- read.csv("data/basic_data.csv")
asg   <- read.csv("models/cluster_results/cluster_assignments.csv")
fac   <- calculate_ml_guided_factors(basic, verbose = FALSE)
drv   <- c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度")
set.seed(2026)
chk <- bind_rows(lapply(1:7, function(k) {
  bn  <- readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k))
  idx <- sample(which(asg$Cluster == k), 20)
  ev  <- fac[idx, drv]
  invisible(predict_bn_targets_analytical(bn, c("CPI", "CPS", "CPA"), ev[1, , drop = FALSE]))  # 预热
  invisible(cpdist(bn, nodes = "CPI", evidence = as.list(ev[1, ]), method = "lw", n = 100))
  t0 <- Sys.time(); cf <- predict_bn_targets_analytical(bn, c("CPI", "CPS", "CPA"), ev)
  t_cf <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  t0 <- Sys.time()
  mc <- t(sapply(seq_len(nrow(ev)), function(i) {
    e <- as.list(ev[i, ]); set.seed(i)
    d <- cpdist(bn, nodes = c("CPI", "CPS", "CPA"), evidence = e, method = "lw", n = 10000)
    colMeans(d[, c("CPI", "CPS", "CPA")])
  }))
  t_mc <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  dd <- abs(as.matrix(cf[, c("CPI", "CPS", "CPA")]) - mc)
  data.frame(Type = k, n = nrow(ev), Max_abs_diff = max(dd), Mean_abs_diff = mean(dd),
             Sec_closed = t_cf, Sec_mc = t_mc)
}))
write.csv(chk, "output/si/closed_form_check.csv", row.names = FALSE)
print(chk, digits = 3)
cat(sprintf("Overall max |diff| = %.4f; speed ratio = %.0f\n", max(chk$Max_abs_diff),
            sum(chk$Sec_mc) / max(sum(chk$Sec_closed), 1e-6)))
