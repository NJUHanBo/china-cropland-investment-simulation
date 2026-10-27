#!/usr/bin/env Rscript
# =============================================================================
# 08b — 敏感性分析：非线性检查（GAM vs 线性回归）
# =============================================================================
#
# 对BN中的关键边，在每个cluster内用GAM拟合非线性关系，
# 与线性回归R^2对比。edf接近1表明线性假设成立。
#
# =============================================================================

library(mgcv)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")

cat("=== 08b: sensitivity — nonlinearity check (GAM vs LM) ===\n\n")

# ---- 1. 加载数据 ------------------------------------------------------------

basic_data  <- read.csv("data/basic_data.csv")
assignments <- read.csv("models/cluster_results/cluster_assignments.csv")

factors <- calculate_ml_guided_factors(basic_data, verbose = FALSE)

network_data <- data.frame(
  城市化 = factors$城市化, 气候变化 = factors$气候变化,
  保护政策 = factors$保护政策, 土地利用 = factors$土地利用,
  利用强度 = factors$利用强度, 产权制度 = factors$产权制度,
  CPI = basic_data$CPI, CPS = basic_data$CPS, CPA = basic_data$CPA
)

cluster_ids <- assignments$Cluster
K <- max(cluster_ids)

# ---- 2. 定义要检查的关键边 --------------------------------------------------

key_edges <- list(
  list(from = "利用强度", to = "CPI"),
  list(from = "利用强度", to = "CPS"),
  list(from = "气候变化", to = "CPI"),
  list(from = "气候变化", to = "CPS"),
  list(from = "气候变化", to = "CPA"),
  list(from = "城市化",   to = "CPS"),
  list(from = "产权制度", to = "CPA")
)

# ---- 3. 逐群组逐边对比 ------------------------------------------------------

results <- list()
ri <- 0L

for (k in seq_len(K)) {
  mask <- cluster_ids == k
  data_k <- network_data[mask, ]
  n_k <- nrow(data_k)

  for (edge in key_edges) {
    x_var <- edge$from
    y_var <- edge$to

    x <- data_k[[x_var]]
    y <- data_k[[y_var]]

    if (sd(x) < 1e-10 || sd(y) < 1e-10) next

    lm_fit <- lm(y ~ x)
    r2_lm <- summary(lm_fit)$r.squared

    gam_fit <- tryCatch(
      gam(y ~ s(x, k = min(10, floor(n_k / 5))), method = "REML"),
      error = function(e) NULL
    )

    if (!is.null(gam_fit)) {
      r2_gam <- summary(gam_fit)$r.sq
      edf <- summary(gam_fit)$s.table[1, "edf"]
      p_smooth <- summary(gam_fit)$s.table[1, "p-value"]
      f_stat <- summary(gam_fit)$s.table[1, "F"]
    } else {
      r2_gam <- NA; edf <- NA; p_smooth <- NA; f_stat <- NA
    }

    ri <- ri + 1L
    results[[ri]] <- data.frame(
      Cluster = k,
      N = n_k,
      From = x_var,
      To = y_var,
      R2_Linear = r2_lm,
      R2_GAM = r2_gam,
      Delta_R2 = r2_gam - r2_lm,
      GAM_edf = edf,
      GAM_F = f_stat,
      GAM_p = p_smooth,
      Linear_OK = ifelse(!is.na(edf), edf < 2.0, NA),
      stringsAsFactors = FALSE
    )
  }
}

results_df <- do.call(rbind, results)

# ---- 4. 汇总判断 ------------------------------------------------------------

cat("=== Nonlinearity Check Summary ===\n\n")

edge_summary <- results_df %>%
  group_by(.data$From, .data$To) %>%
  summarise(
    Mean_R2_LM  = mean(.data$R2_Linear, na.rm = TRUE),
    Mean_R2_GAM = mean(.data$R2_GAM, na.rm = TRUE),
    Mean_Delta_R2 = mean(.data$Delta_R2, na.rm = TRUE),
    Mean_edf = mean(.data$GAM_edf, na.rm = TRUE),
    Pct_Linear_OK = mean(.data$Linear_OK, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(.data$Mean_Delta_R2))

cat("Edge-level summary (across clusters):\n")
print(edge_summary)

cat(sprintf("\nOverall linearity verdict: %.1f%% of edge-cluster pairs have edf < 2.0\n",
            mean(results_df$Linear_OK, na.rm = TRUE) * 100))

cat("\n--- Detailed results ---\n")
print(results_df)

# ---- 5. 保存 ----------------------------------------------------------------

out_dir <- "output/csv/sensitivity"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(results_df, file.path(out_dir, "nonlinearity_gam_detail.csv"), row.names = FALSE)
write.csv(edge_summary, file.path(out_dir, "nonlinearity_gam_summary.csv"), row.names = FALSE)

cat("\nsaved to output/csv/sensitivity/\n")
cat("\n=== 08b complete ===\n")
