#!/usr/bin/env Rscript
# =============================================================================
# 08d — 敏感性分析：参数扰动Bootstrap
# =============================================================================
#
# 对BN系数施加随机扰动（±10%/20%/50%），重跑情景模拟，
# 检查核心结论的鲁棒性。
#
# =============================================================================

library(bnlearn)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")

cat("=== 08d: sensitivity — coefficient perturbation bootstrap ===\n\n")

# ---- 1. 加载数据 ------------------------------------------------------------

basic_data  <- read.csv("data/basic_data.csv")
cpi_mean    <- read.csv("data/CPI_20year_mean_by_county.csv")
combined    <- merge(basic_data, cpi_mean[, c("Code", "CPI_20year_mean")], by = "Code", all.x = TRUE)
assignments <- read.csv("models/cluster_results/cluster_assignments.csv")
dag         <- readRDS("models/corrected_bn/corrected_dag.RDS")

cluster_ids <- assignments$Cluster[match(combined$Code, assignments$Code)]
stopifnot("cluster alignment failed" = !any(is.na(cluster_ids)))
K <- max(cluster_ids)

orig_bns <- lapply(seq_len(K), function(k) {
  readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k))
})

target_nodes <- c("CPI", "CPS", "CPA")
n_counties <- nrow(combined)

# ---- 2. 辅助函数：扰动BN系数 ------------------------------------------------

perturb_bn <- function(fitted_bn, noise_level, seed) {
  set.seed(seed)

  for (node_name in names(fitted_bn)) {
    node_fit <- fitted_bn[[node_name]]
    coefs <- coef(node_fit)

    if (length(coefs) > 1) {
      noise <- rnorm(length(coefs) - 1, mean = 0, sd = noise_level)
      coefs[-1] <- coefs[-1] * (1 + noise)

      replacement <- list(
        coef = coefs,
        sd   = node_fit$sd
      )
      fitted_bn[[node_name]] <- replacement
    }
  }

  fitted_bn
}

# ---- 3. 代表性情景定义 ------------------------------------------------------

# 全部 15 种组合（2026-10-25 由 4 种扩展；均匀分配、当前环境）
source("lib/scenario_functions.R")
test_policies <- POLICY_CONFIGS

urban_vars   <- grep("URB", names(combined), value = TRUE)
climate_vars <- grep("Clim", names(combined), value = TRUE)
conserv_vars <- grep("Conserv\\.", names(combined), value = TRUE)
land_vars    <- grep("LAN", names(combined), value = TRUE)
using_vars   <- grep("Using", names(combined), value = TRUE)
prop_vars    <- grep("Prop", names(combined), value = TRUE)

# ---- 4. Bootstrap主循环 -----------------------------------------------------

noise_levels <- c(0.10, 0.20, 0.50)
n_boot <- 100

all_boot_results <- list()
boot_idx <- 0L

for (noise in noise_levels) {
  cat(sprintf("\n--- Noise level: ±%.0f%% (%d iterations) ---\n", noise * 100, n_boot))

  boot_rankings <- matrix(NA_real_, nrow = n_boot, ncol = length(test_policies))
  colnames(boot_rankings) <- names(test_policies)

  boot_synergy_rates <- matrix(NA_real_, nrow = n_boot, ncol = length(test_policies))
  colnames(boot_synergy_rates) <- names(test_policies)

  for (b in seq_len(n_boot)) {
    if (b %% 25 == 0) cat(sprintf("  boot %d/%d\n", b, n_boot))

    perturbed_bns <- lapply(seq_len(K), function(k) {
      perturb_bn(orig_bns[[k]], noise, seed = 1000 * b + k)
    })

    factors_base <- calculate_ml_guided_factors(combined, verbose = FALSE)
    baseline_pred <- predict_by_cluster(perturbed_bns, cluster_ids, target_nodes, factors_base)

    for (pi in seq_along(test_policies)) {
      p_name <- names(test_policies)[pi]
      p_cfg <- test_policies[[pi]]
      base_mults <- 1 + p_cfg / 12

      int_data <- combined
      int_data[, conserv_vars] <- int_data[, conserv_vars] * base_mults[1]
      int_data[, land_vars]    <- int_data[, land_vars]    * base_mults[2]
      int_data[, using_vars]   <- int_data[, using_vars]   * base_mults[3]
      int_data[, prop_vars]    <- int_data[, prop_vars]    * base_mults[4]

      factors_int <- calculate_ml_guided_factors(int_data, verbose = FALSE)
      int_pred <- predict_by_cluster(perturbed_bns, cluster_ids, target_nodes, factors_int)

      eff_CPI <- int_pred$CPI - baseline_pred$CPI
      eff_CPS <- int_pred$CPS - baseline_pred$CPS
      eff_CPA <- int_pred$CPA - baseline_pred$CPA

      boot_rankings[b, pi] <- mean(eff_CPI + eff_CPS + eff_CPA, na.rm = TRUE)

      n_pos <- as.integer(eff_CPI > 0) + as.integer(eff_CPS > 0) + as.integer(eff_CPA > 0)
      boot_synergy_rates[b, pi] <- mean(n_pos == 3) * 100
    }
  }

  boot_idx <- boot_idx + 1L
  all_boot_results[[boot_idx]] <- list(
    noise = noise,
    rankings = boot_rankings,
    synergy = boot_synergy_rates
  )

  # 排名稳定性
  rank_matrix <- t(apply(boot_rankings, 1, function(x) rank(-x)))
  colnames(rank_matrix) <- names(test_policies)

  cat(sprintf("\n  Ranking stability (noise=%.0f%%):\n", noise * 100))
  for (pi in seq_along(test_policies)) {
    p_name <- names(test_policies)[pi]
    modal_rank <- as.numeric(names(sort(table(rank_matrix[, pi]), decreasing = TRUE))[1])
    pct_modal <- max(table(rank_matrix[, pi])) / n_boot * 100
    cat(sprintf("    %s: modal rank=%d (%.0f%% of iterations), effect mean=%.4f [%.4f, %.4f]\n",
                p_name, modal_rank, pct_modal,
                mean(boot_rankings[, pi]),
                quantile(boot_rankings[, pi], 0.025),
                quantile(boot_rankings[, pi], 0.975)))
  }

  cat(sprintf("\n  Synergy rate stability (noise=%.0f%%):\n", noise * 100))
  for (pi in seq_along(test_policies)) {
    p_name <- names(test_policies)[pi]
    cat(sprintf("    %s: mean=%.1f%% [%.1f%%, %.1f%%]\n",
                p_name,
                mean(boot_synergy_rates[, pi]),
                quantile(boot_synergy_rates[, pi], 0.025),
                quantile(boot_synergy_rates[, pi], 0.975)))
  }
}

# ---- 5. 汇总保存 ------------------------------------------------------------

out_dir <- "output/csv/sensitivity"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

summary_rows <- list()
si <- 0L

for (br in all_boot_results) {
  rank_matrix <- t(apply(br$rankings, 1, function(x) rank(-x)))
  colnames(rank_matrix) <- names(test_policies)

  for (pi in seq_along(test_policies)) {
    si <- si + 1L
    p_name <- names(test_policies)[pi]
    summary_rows[[si]] <- data.frame(
      Noise_Level = br$noise,
      Policy = p_name,
      Effect_Mean = mean(br$rankings[, pi]),
      Effect_Q025 = quantile(br$rankings[, pi], 0.025),
      Effect_Q975 = quantile(br$rankings[, pi], 0.975),
      Modal_Rank = as.numeric(names(sort(table(rank_matrix[, pi]), decreasing = TRUE))[1]),
      Pct_Modal_Rank = max(table(rank_matrix[, pi])) / n_boot * 100,
      Synergy_Mean = mean(br$synergy[, pi]),
      Synergy_Q025 = quantile(br$synergy[, pi], 0.025),
      Synergy_Q975 = quantile(br$synergy[, pi], 0.975),
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL
write.csv(summary_df, file.path(out_dir, "bootstrap_perturbation_summary.csv"), row.names = FALSE)
saveRDS(all_boot_results, file.path(out_dir, "bootstrap_perturbation_draws.RDS"))

cat("\n=== Bootstrap Summary ===\n")
print(summary_df)

cat("\nsaved to output/csv/sensitivity/\n")
cat("\n=== 08d complete ===\n")
