#!/usr/bin/env Rscript
# =============================================================================
# 02 — 基于因果森林τ̂矩阵的县域聚类
# =============================================================================
#
# 输入：models/causal_forest_results/cf_tau_matrix.csv
#       data/basic_data.csv
# 输出：models/cluster_results/ 下的聚类结果
#
# =============================================================================

library(cluster)
library(dplyr)

source("lib/cluster_bn_functions.R")

# ---- 1. 加载数据 ------------------------------------------------------------

cat("=== 02: county clustering by causal effect profiles ===\n\n")

tau_df <- read.csv("models/causal_forest_results/cf_tau_matrix.csv")
basic_data <- read.csv("data/basic_data.csv")

county_codes <- tau_df$Code
tau_matrix   <- as.matrix(tau_df[, -1])  # 去掉Code列

cat(sprintf("tau matrix: %d counties x %d models\n", nrow(tau_matrix), ncol(tau_matrix)))

# ---- 2. 聚类 ----------------------------------------------------------------

cat("\nclustering ...\n")
cluster_result <- cluster_counties_by_tau(tau_matrix, k_range = 2:8, seed = 2024)

cat(sprintf("\ncluster sizes: %s\n", paste(cluster_result$sizes, collapse = ", ")))

# ---- 3. 组装输出 -------------------------------------------------------------

# 县域→群组映射
assignments_df <- data.frame(
  Code     = county_codes,
  Cluster  = cluster_result$assignments,
  Province = basic_data$Province,
  Zones    = basic_data$Zones,
  stringsAsFactors = FALSE
)

# 群组profile
profiles_df <- as.data.frame(cluster_result$profiles)
profiles_df$Cluster  <- seq_len(cluster_result$optimal_k)
profiles_df$Size     <- cluster_result$sizes

# 每群组的τ̂描述统计
cluster_desc <- assignments_df %>%
  group_by(Cluster) %>%
  summarise(
    N = n(),
    N_Provinces = n_distinct(Province),
    Top_Zones = paste(names(sort(table(Zones), decreasing = TRUE))[1:min(3, n_distinct(Zones))], collapse = "/"),
    .groups = "drop"
  )

# 每列τ̂的ANOVA检验（验证聚类有效性）
anova_results <- vapply(colnames(tau_matrix), function(col) {
  fit <- aov(tau_matrix[, col] ~ factor(cluster_result$assignments))
  summary(fit)[[1]][["Pr(>F)"]][1]
}, numeric(1))

anova_df <- data.frame(
  Model = colnames(tau_matrix),
  ANOVA_P = anova_results,
  Significant = anova_results < 0.05,
  stringsAsFactors = FALSE
)

# ---- 4. 保存 ----------------------------------------------------------------

out_dir <- "models/cluster_results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(assignments_df, file.path(out_dir, "cluster_assignments.csv"), row.names = FALSE)
cat("saved: cluster_assignments.csv\n")

write.csv(profiles_df, file.path(out_dir, "cluster_profiles.csv"), row.names = FALSE)
cat("saved: cluster_profiles.csv\n")

write.csv(cluster_result$diagnostics, file.path(out_dir, "cluster_diagnostics.csv"), row.names = FALSE)
cat("saved: cluster_diagnostics.csv\n")

write.csv(cluster_desc, file.path(out_dir, "cluster_description.csv"), row.names = FALSE)
cat("saved: cluster_description.csv\n")

write.csv(anova_df, file.path(out_dir, "cluster_anova_validation.csv"), row.names = FALSE)
cat("saved: cluster_anova_validation.csv\n")

# ---- 5. 打印汇总 ------------------------------------------------------------

cat("\n=== Cluster Profiles (mean tau-hat) ===\n")
print(round(cluster_result$profiles, 4))

cat("\n=== Cluster Description ===\n")
print(cluster_desc)

cat("\n=== ANOVA Validation ===\n")
cat(sprintf("%d / %d models show significant between-cluster differences (p<0.05)\n",
            sum(anova_df$Significant), nrow(anova_df)))

cat("\n=== 02 complete ===\n")
