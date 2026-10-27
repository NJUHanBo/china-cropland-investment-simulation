#!/usr/bin/env Rscript
# =============================================================================
# 08c — 敏感性分析：CF直接预测 vs BN预测对比
# =============================================================================
#
# 绕过BN，直接用CF的τ̂(x_i) × 干预量做情景预测，
# 对比CF预测与BN预测在排名和分类上的一致性。
#
# =============================================================================

library(dplyr)

cat("=== 08c: sensitivity — CF vs BN comparison ===\n\n")

# ---- 1. 加载已有结果 --------------------------------------------------------

bn_results <- readRDS("output/csv/scenario_results_cluster_bn.RDS")
tau_matrix <- read.csv("models/causal_forest_results/cf_tau_matrix.csv")
assignments <- read.csv("models/cluster_results/cluster_assignments.csv")

county_codes <- tau_matrix$Code
tau_cols <- grep("_on_", names(tau_matrix), value = TRUE)
tau_mat <- as.matrix(tau_matrix[, tau_cols])

cat(sprintf("tau_matrix: %d counties x %d models\n", nrow(tau_mat), ncol(tau_mat)))

# ---- 2. CF直接预测逻辑 ------------------------------------------------------
#
# 对每个policy config，CF预测的效应 = Σ_d τ̂(治疗d→结果y, 县i) × Δ治疗d_i
# Δ治疗d_i = 政策引起的因子d变化量（从BN结果中反推）

# 提取因子变化量：BN_Prediction - BN_Baseline 除以 BN系数
# 更简洁的方式：直接用policy_configs定义的相对强度作为Δ治疗近似

policy_configs <- list(
  Baseline = c(0, 0, 0, 0), I01 = c(3, 3, 3, 3),
  I02 = c(9, 1, 1, 1), I03 = c(1, 9, 1, 1),
  I04 = c(1, 1, 9, 1), I05 = c(1, 1, 1, 9),
  I06 = c(5, 5, 1, 1), I07 = c(5, 1, 5, 1),
  I08 = c(5, 1, 1, 5), I09 = c(1, 5, 5, 1),
  I10 = c(1, 5, 1, 5), I11 = c(1, 1, 5, 5),
  I12 = c(4, 4, 4, 0), I13 = c(4, 4, 0, 4),
  I14 = c(4, 0, 4, 4), I15 = c(0, 4, 4, 4)
)

treatment_dims <- c("保护政策", "土地利用", "利用强度", "产权制度")
outcome_vars <- c("CPI", "CPS", "CPA")

cf_policy_effects <- vector("list", length(policy_configs))

for (pi in seq_along(policy_configs)) {
  p_name <- names(policy_configs)[pi]
  p_cfg <- policy_configs[[pi]]
  weights <- p_cfg / max(sum(p_cfg), 1)

  cf_effects <- matrix(0, nrow = nrow(tau_mat), ncol = 3)
  colnames(cf_effects) <- outcome_vars

  for (di in seq_along(treatment_dims)) {
    d_name <- treatment_dims[di]
    w <- weights[di]
    for (yi in seq_along(outcome_vars)) {
      y_name <- outcome_vars[yi]
      col_name <- paste0(d_name, "_on_", y_name)
      if (col_name %in% colnames(tau_mat)) {
        cf_effects[, yi] <- cf_effects[, yi] + w * tau_mat[, col_name]
      }
    }
  }

  cf_policy_effects[[pi]] <- data.frame(
    Policy_Config = p_name,
    County_Code = county_codes,
    CF_CPI_Effect = cf_effects[, "CPI"],
    CF_CPS_Effect = cf_effects[, "CPS"],
    CF_CPA_Effect = cf_effects[, "CPA"],
    stringsAsFactors = FALSE
  )
}

cf_all <- do.call(rbind, cf_policy_effects)

# ---- 3. 对比：跨部门优先级排名 ----------------------------------------------

cat("\n=== Comparison 1: Cross-sector priority ranking ===\n")

bn_by_policy <- bn_results %>%
  group_by(.data$Policy_Config) %>%
  summarise(
    BN_CPI = mean(.data$BN_CPI_Policy_Effect, na.rm = TRUE),
    BN_CPS = mean(.data$BN_CPS_Policy_Effect, na.rm = TRUE),
    BN_CPA = mean(.data$BN_CPA_Policy_Effect, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(BN_Sum = .data$BN_CPI + .data$BN_CPS + .data$BN_CPA)

cf_by_policy <- cf_all %>%
  group_by(.data$Policy_Config) %>%
  summarise(
    CF_CPI = mean(.data$CF_CPI_Effect, na.rm = TRUE),
    CF_CPS = mean(.data$CF_CPS_Effect, na.rm = TRUE),
    CF_CPA = mean(.data$CF_CPA_Effect, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(CF_Sum = .data$CF_CPI + .data$CF_CPS + .data$CF_CPA)

ranking_comp <- merge(bn_by_policy, cf_by_policy, by = "Policy_Config")
ranking_comp$BN_Rank <- rank(-ranking_comp$BN_Sum)
ranking_comp$CF_Rank <- rank(-ranking_comp$CF_Sum)

rank_corr <- cor(ranking_comp$BN_Rank, ranking_comp$CF_Rank, method = "spearman")
cat(sprintf("Policy ranking Spearman correlation: %.4f\n", rank_corr))
print(ranking_comp %>% select("Policy_Config", "BN_Sum", "CF_Sum", "BN_Rank", "CF_Rank") %>% arrange(.data$BN_Rank))

# ---- 4. 对比：县域分类一致性 ------------------------------------------------

cat("\n=== Comparison 2: County-level sign consistency ===\n")

bn_county_avg <- bn_results %>%
  group_by(.data$County_Code) %>%
  summarise(
    BN_CPI = mean(.data$BN_CPI_Policy_Effect, na.rm = TRUE),
    BN_CPS = mean(.data$BN_CPS_Policy_Effect, na.rm = TRUE),
    BN_CPA = mean(.data$BN_CPA_Policy_Effect, na.rm = TRUE),
    .groups = "drop"
  )

cf_county_avg <- cf_all %>%
  group_by(.data$County_Code) %>%
  summarise(
    CF_CPI = mean(.data$CF_CPI_Effect, na.rm = TRUE),
    CF_CPS = mean(.data$CF_CPS_Effect, na.rm = TRUE),
    CF_CPA = mean(.data$CF_CPA_Effect, na.rm = TRUE),
    .groups = "drop"
  )

county_comp <- merge(bn_county_avg, cf_county_avg, by = "County_Code")

sign_agree_CPI <- mean(sign(county_comp$BN_CPI) == sign(county_comp$CF_CPI), na.rm = TRUE)
sign_agree_CPS <- mean(sign(county_comp$BN_CPS) == sign(county_comp$CF_CPS), na.rm = TRUE)
sign_agree_CPA <- mean(sign(county_comp$BN_CPA) == sign(county_comp$CF_CPA), na.rm = TRUE)

cat(sprintf("Sign agreement: CPI=%.1f%%, CPS=%.1f%%, CPA=%.1f%%\n",
            sign_agree_CPI * 100, sign_agree_CPS * 100, sign_agree_CPA * 100))

cor_CPI <- cor(county_comp$BN_CPI, county_comp$CF_CPI, method = "spearman")
cor_CPS <- cor(county_comp$BN_CPS, county_comp$CF_CPS, method = "spearman")
cor_CPA <- cor(county_comp$BN_CPA, county_comp$CF_CPA, method = "spearman")

cat(sprintf("County-level Spearman: CPI=%.4f, CPS=%.4f, CPA=%.4f\n", cor_CPI, cor_CPS, cor_CPA))

# ---- 5. 对比：利用强度 vs 其他因素的相对重要性 ------------------------------

cat("\n=== Comparison 3: Relative importance of 利用强度 ===\n")

# I04 = (1,1,9,1) 利用强度为主, I02 = (9,1,1,1) 保护政策为主
bn_i04 <- bn_by_policy %>% filter(.data$Policy_Config == "I04")
bn_i02 <- bn_by_policy %>% filter(.data$Policy_Config == "I02")
cf_i04 <- cf_by_policy %>% filter(.data$Policy_Config == "I04")
cf_i02 <- cf_by_policy %>% filter(.data$Policy_Config == "I02")

cat(sprintf("BN:  I04 (利用强度) sum=%.4f, I02 (保护) sum=%.4f, ratio=%.1fx\n",
            bn_i04$BN_Sum, bn_i02$BN_Sum, bn_i04$BN_Sum / max(bn_i02$BN_Sum, 1e-10)))
cat(sprintf("CF:  I04 (利用强度) sum=%.4f, I02 (保护) sum=%.4f, ratio=%.1fx\n",
            cf_i04$CF_Sum, cf_i02$CF_Sum, cf_i04$CF_Sum / max(cf_i02$CF_Sum, 1e-10)))

# ---- 6. 保存 ----------------------------------------------------------------

out_dir <- "output/csv/sensitivity"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(ranking_comp, file.path(out_dir, "cf_bn_policy_ranking.csv"), row.names = FALSE)
write.csv(county_comp, file.path(out_dir, "cf_bn_county_comparison.csv"), row.names = FALSE)

summary_df <- data.frame(
  Metric = c("Policy_Rank_Spearman", "Sign_CPI", "Sign_CPS", "Sign_CPA",
             "County_Spearman_CPI", "County_Spearman_CPS", "County_Spearman_CPA"),
  Value = c(rank_corr, sign_agree_CPI, sign_agree_CPS, sign_agree_CPA,
            cor_CPI, cor_CPS, cor_CPA)
)
write.csv(summary_df, file.path(out_dir, "cf_bn_consistency_summary.csv"), row.names = FALSE)

cat("\nsaved to output/csv/sensitivity/\n")
cat("\n=== 08c complete ===\n")
