#!/usr/bin/env Rscript
# =============================================================================
# 11 — Oster Bound + E-value 敏感性分析
# =============================================================================
#
# 对因果森林识别的显著ATE边，检验其对未观测混杂的稳健性
# Oster (2019): 系数稳定性检验
# E-value (VanderWeele & Ding 2017): 最小混杂强度
#
# 输入：data/basic_data.csv, models/causal_forest_results/cf_ate_summary.csv
# 输出：output/csv/oster_evalue_sensitivity.csv
#
# =============================================================================

library(sensemakr)
library(EValue)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")

cat("=== 11: Oster Bound + E-value Sensitivity Analysis ===\n\n")

# ---- 1. 加载数据 -----------------------------------------------------------

basic_data <- read.csv("data/basic_data.csv")
ate_summary <- read.csv("models/causal_forest_results/cf_ate_summary.csv")
factors <- calculate_ml_guided_factors(basic_data, verbose = FALSE)

outcomes <- basic_data[, c("CPI", "CPS", "CPA")]
all_dims <- names(factors)

# 只对显著边做敏感性分析
sig_edges <- ate_summary[ate_summary$Significant, ]
cat(sprintf("显著边数: %d\n\n", nrow(sig_edges)))

# ---- 2. Oster Bound (sensemakr) -------------------------------------------

cat("========== Oster Bound / sensemakr ==========\n\n")

oster_results <- list()

for (i in seq_len(nrow(sig_edges))) {
  treatment <- sig_edges$Treatment[i]
  outcome <- sig_edges$Outcome[i]
  label <- sig_edges$Model_Label[i]

  W <- factors[[treatment]]
  Y <- outcomes[[outcome]]
  control_dims <- setdiff(all_dims, treatment)
  X <- factors[, control_dims, drop = FALSE]

  df_reg <- cbind(data.frame(Y = Y, W = W), X)
  df_reg <- df_reg[complete.cases(df_reg), ]

  cat(sprintf("[%d] %s -> %s\n", i, treatment, outcome))

  tryCatch({
    # 跑回归：Y ~ W + controls
    fml <- as.formula(paste("Y ~ W +", paste(names(X), collapse = " + ")))
    model <- lm(fml, data = df_reg)

    # sensemakr: 评估W对Y的效应对未观测混杂的敏感性
    sens <- sensemakr(
      model = model,
      treatment = "W",
      benchmark_covariates = names(X),
      kd = 1:3
    )

    # 提取关键指标
    # RV_q: Robustness Value - 未观测变量至少要解释多大比例的残差才能归零效应
    rv_q <- sens$sensitivity_stats$rv_q
    rv_qa <- sens$sensitivity_stats$rv_qa

    cat(sprintf("  RV_q (reduce to 0): %.3f\n", rv_q))
    cat(sprintf("  RV_qa (reduce to 0, adjusted): %.3f\n", rv_qa))

    # 如果RV > 0.1，说明未观测混杂需要解释>10%的残差方差才能归零，比较robust
    robust <- rv_q > 0.10

    oster_results[[label]] <- data.frame(
      Treatment = treatment,
      Outcome = outcome,
      Label = label,
      ATE = sig_edges$ATE[i],
      RV_q = rv_q,
      RV_qa = rv_qa,
      Robust_10pct = robust,
      stringsAsFactors = FALSE
    )

    cat(sprintf("  Robust (RV>10%%): %s\n\n", ifelse(robust, "YES", "NO")))

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n\n", e$message))
    oster_results[[label]] <<- data.frame(
      Treatment = treatment, Outcome = outcome, Label = label,
      ATE = sig_edges$ATE[i], RV_q = NA, RV_qa = NA, Robust_10pct = NA,
      stringsAsFactors = FALSE
    )
  })
}

oster_df <- do.call(rbind, oster_results)
rownames(oster_df) <- NULL

# ---- 3. E-value -----------------------------------------------------------

cat("\n========== E-value ==========\n\n")

evalue_results <- list()

for (i in seq_len(nrow(sig_edges))) {
  treatment <- sig_edges$Treatment[i]
  outcome <- sig_edges$Outcome[i]
  label <- sig_edges$Model_Label[i]

  ate <- sig_edges$ATE[i]
  ate_se <- sig_edges$ATE_SE[i]

  cat(sprintf("[%d] %s -> %s (ATE=%.4f, SE=%.4f)\n", i, treatment, outcome, ate, ate_se))

  tryCatch({
    # 对连续结果变量，需要用标准化效应量（Cohen's d近似）
    W <- factors[[treatment]]
    Y <- outcomes[[outcome]]
    complete <- complete.cases(W, Y)
    W <- W[complete]
    Y <- Y[complete]

    # 用ATE / SD(Y)作为近似标准化效应量
    sd_y <- sd(Y)
    d_approx <- ate / sd_y

    # E-value for continuous outcome (approximation via risk ratio conversion)
    # d -> RR: RR ≈ exp(0.91 * |d|)
    rr_approx <- exp(0.91 * abs(d_approx))

    # CI lower bound for RR (use absolute values for negative effects)
    d_ci_lower <- abs(d_approx) - 1.96 * ate_se / sd_y
    rr_lo <- if (d_ci_lower > 0) exp(0.91 * d_ci_lower) else 1.0

    # E-value = RR + sqrt(RR * (RR-1))
    ev <- evalues.RR(rr_approx, lo = rr_lo)

    evalue_point <- ev[2, "point"]
    evalue_ci <- ev[2, "lower"]

    cat(sprintf("  Cohen's d ≈ %.3f, RR ≈ %.3f\n", d_approx, rr_approx))
    cat(sprintf("  E-value (point): %.3f\n", evalue_point))
    cat(sprintf("  E-value (CI): %.3f\n", evalue_ci))

    # E-value > 2 通常被认为相当robust
    robust <- evalue_point > 2.0

    evalue_results[[label]] <- data.frame(
      Treatment = treatment,
      Outcome = outcome,
      Label = label,
      ATE = ate,
      Cohens_d = d_approx,
      RR_approx = rr_approx,
      Evalue_point = evalue_point,
      Evalue_CI = evalue_ci,
      Robust_Evalue_gt2 = robust,
      stringsAsFactors = FALSE
    )

    cat(sprintf("  Robust (E-value>2): %s\n\n", ifelse(robust, "YES", "NO")))

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n\n", e$message))
    evalue_results[[label]] <<- data.frame(
      Treatment = treatment, Outcome = outcome, Label = label,
      ATE = ate, Cohens_d = NA, RR_approx = NA,
      Evalue_point = NA, Evalue_CI = NA, Robust_Evalue_gt2 = NA,
      stringsAsFactors = FALSE
    )
  })
}

evalue_df <- do.call(rbind, evalue_results)
rownames(evalue_df) <- NULL

# ---- 4. 合并汇总 -----------------------------------------------------------

cat("\n========== 综合汇总 ==========\n\n")

combined <- merge(oster_df, evalue_df[, c("Label", "Cohens_d", "RR_approx",
                                           "Evalue_point", "Evalue_CI", "Robust_Evalue_gt2")],
                  by = "Label")

combined$Both_Robust <- combined$Robust_10pct & combined$Robust_Evalue_gt2

cat(sprintf("%-30s %8s %8s %8s %8s %8s %5s\n",
            "Edge", "ATE", "RV_q", "RV_qa", "E-val", "E-CI", "Pass?"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (i in seq_len(nrow(combined))) {
  r <- combined[i, ]
  cat(sprintf("%-30s %8.3f %8.3f %8.3f %8.3f %8.3f %5s\n",
              r$Label, r$ATE,
              ifelse(is.na(r$RV_q), NA, r$RV_q),
              ifelse(is.na(r$RV_qa), NA, r$RV_qa),
              ifelse(is.na(r$Evalue_point), NA, r$Evalue_point),
              ifelse(is.na(r$Evalue_CI), NA, r$Evalue_CI),
              ifelse(is.na(r$Both_Robust), "?",
                     ifelse(r$Both_Robust, "YES", "NO"))))
}

n_pass <- sum(combined$Both_Robust, na.rm = TRUE)
n_total <- nrow(combined)
cat(sprintf("\n双重通过率: %d/%d (%.0f%%)\n", n_pass, n_total, 100 * n_pass / n_total))

# ---- 5. 保存 ---------------------------------------------------------------

write.csv(combined, "output/csv/oster_evalue_sensitivity.csv", row.names = FALSE)
cat("\n结果已保存到 output/csv/oster_evalue_sensitivity.csv\n")
cat("\n=== 11 complete ===\n")
