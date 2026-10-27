# =============================================================================
# 变化量的参照：模型现状基线（E2、无干预）
# =============================================================================
#
# 情景效应一律相对"模型在现状条件下的预测"计算，而不是相对观测值。
# 观测值 = 模型预测 + 拟合残差；以观测为参照会把残差（CPS 残差 SD 约 0.19，
# 与 I04 平均政策效应同量级）混进"协同/权衡"分类。
# 原"预测 - 观测"保留为 BN_*_Change_vs_Obs，仅供核对。
#
# =============================================================================

add_model_baseline_change <- function(results_df, ref_env = "E2") {
  ref <- results_df[results_df$External_Config == ref_env,
                    c("County_Code", "BN_CPI_Baseline", "BN_CPS_Baseline", "BN_CPA_Baseline")]
  ref <- ref[!duplicated(ref$County_Code), ]
  idx <- match(results_df$County_Code, ref$County_Code)
  stopifnot("model baseline alignment failed" = !any(is.na(idx)))

  for (y in c("CPI", "CPS", "CPA")) {
    results_df[[paste0("BN_", y, "_Change_vs_Obs")]] <- results_df[[paste0("BN_", y, "_Total_Change")]]
    results_df[[paste0("BN_", y, "_Total_Change")]] <-
      results_df[[paste0("BN_", y, "_Prediction")]] - ref[[paste0("BN_", y, "_Baseline")]][idx]
  }
  results_df
}
