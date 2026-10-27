#!/usr/bin/env Rscript
# =============================================================================
# 03 — DAG结构修正 + 分群贝叶斯网络拟合
# =============================================================================
#
# 输入：models/causal_forest_results/cf_ate_summary.csv, cf_bn_edge_lists.RDS
#       models/enhanced_network_results/detailed_edge_statistics.csv
#       models/cluster_results/cluster_assignments.csv
#       data/basic_data.csv
# 输出：models/corrected_bn/ 下的修正DAG和K个群组BN
#
# =============================================================================

library(bnlearn)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")

# ---- 1. 加载数据 ------------------------------------------------------------

cat("=== 03: DAG correction + cluster-specific BN fitting ===\n\n")

original_edges <- read.csv("models/enhanced_network_results/detailed_edge_statistics.csv")
edge_lists     <- readRDS("models/causal_forest_results/cf_bn_edge_lists.RDS")
ate_summary    <- read.csv("models/causal_forest_results/cf_ate_summary.csv")
assignments    <- read.csv("models/cluster_results/cluster_assignments.csv")
basic_data     <- read.csv("data/basic_data.csv")

cat(sprintf("original edges: %d\n", nrow(original_edges)))
cat(sprintf("CF whitelist: %d, blacklist: %d\n", nrow(edge_lists$whitelist), nrow(edge_lists$blacklist)))

# ---- 2. DAG结构修正 ---------------------------------------------------------

# 去掉CPDS，保留9个节点
target_nodes <- c("城市化", "气候变化", "保护政策", "土地利用", "利用强度", "产权制度",
                  "CPI", "CPS", "CPA")

cat("\nbuilding corrected DAG ...\n")
dag_result <- build_corrected_dag(
  original_edges = original_edges,
  whitelist      = edge_lists$whitelist,
  blacklist      = edge_lists$blacklist,
  target_nodes   = target_nodes
)

dag <- dag_result$dag

# ---- 3. 准备网络训练数据 -----------------------------------------------------

cat("\npreparing network data ...\n")

factors <- calculate_ml_guided_factors(basic_data, verbose = FALSE)

network_data <- data.frame(
  城市化   = factors$城市化,
  气候变化 = factors$气候变化,
  保护政策 = factors$保护政策,
  土地利用 = factors$土地利用,
  利用强度 = factors$利用强度,
  产权制度 = factors$产权制度,
  CPI = basic_data$CPI,
  CPS = basic_data$CPS,
  CPA = basic_data$CPA
)
network_data[] <- lapply(network_data, as.numeric)

cat(sprintf("network data: %d rows x %d cols\n", nrow(network_data), ncol(network_data)))

# 节点一致性检查
dag_nodes  <- nodes(dag)
data_cols  <- names(network_data)
missing_in_data    <- setdiff(dag_nodes, data_cols)
missing_in_network <- setdiff(data_cols, dag_nodes)
if (length(missing_in_data) > 0) stop("DAG nodes missing in data: ", paste(missing_in_data, collapse = ", "))
if (length(missing_in_network) > 0) stop("Data cols missing in DAG: ", paste(missing_in_network, collapse = ", "))

# ---- 4. 分群BN拟合 ----------------------------------------------------------

cluster_ids <- assignments$Cluster
K <- max(cluster_ids)
cat(sprintf("\nfitting %d cluster-specific BNs ...\n", K))

cluster_bns <- fit_cluster_bns(dag, network_data, cluster_ids)

# ---- 5. 提取边系数对比 ------------------------------------------------------

cat("\nextracting edge coefficients ...\n")

arcs_df <- arcs(dag)
coef_rows <- vector("list", nrow(arcs_df) * K)
idx <- 0L

for (k in seq_len(K)) {
  bn_k <- cluster_bns[[k]]
  # VECTORIZATION CHECK: 遍历边和群组提取系数，无法向量化（不同模型对象）
  for (i in seq_len(nrow(arcs_df))) {
    idx <- idx + 1L
    from <- arcs_df[i, "from"]
    to   <- arcs_df[i, "to"]
    coefs_node <- coef(bn_k[[to]])
    coef_val <- if (from %in% names(coefs_node)) coefs_node[from] else NA_real_
    coef_rows[[idx]] <- data.frame(
      Cluster = k,
      From    = from,
      To      = to,
      Coefficient = coef_val,
      stringsAsFactors = FALSE
    )
  }
}

coef_df <- do.call(rbind, coef_rows)

# ---- 6. 保存 ----------------------------------------------------------------

out_dir <- "models/corrected_bn"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 修正后的DAG
write.csv(dag_result$edge_report, file.path(out_dir, "dag_correction_report.csv"), row.names = FALSE)
cat("saved: dag_correction_report.csv\n")

corrected_edges <- arcs(dag)
write.csv(corrected_edges, file.path(out_dir, "corrected_dag_edges.csv"), row.names = FALSE)
cat("saved: corrected_dag_edges.csv\n")

# 分群BN模型
for (k in seq_len(K)) {
  fname <- sprintf("cluster_bn_k%d.RDS", k)
  saveRDS(cluster_bns[[k]], file.path(out_dir, fname))
  cat(sprintf("saved: %s\n", fname))
}

# 边系数对比
write.csv(coef_df, file.path(out_dir, "cluster_bn_coefficients.csv"), row.names = FALSE)
cat("saved: cluster_bn_coefficients.csv\n")

# DAG对象
saveRDS(dag, file.path(out_dir, "corrected_dag.RDS"))
cat("saved: corrected_dag.RDS\n")

# ---- 7. 打印汇总 ------------------------------------------------------------

cat("\n=== DAG Correction Summary ===\n")
status_table <- table(dag_result$edge_report$CF_Status)
for (s in names(status_table)) {
  cat(sprintf("  %s: %d edges\n", s, status_table[s]))
}

cat(sprintf("\nfinal DAG: %d arcs\n", narcs(dag)))

cat("\n=== Coefficient Comparison (top edges by variance across clusters) ===\n")
coef_var <- coef_df %>%
  group_by(From, To) %>%
  summarise(
    Mean_Coef = mean(Coefficient, na.rm = TRUE),
    SD_Coef   = sd(Coefficient, na.rm = TRUE),
    CV        = abs(sd(Coefficient, na.rm = TRUE) / mean(Coefficient, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(SD_Coef))

print(head(coef_var, 10))

cat("\n=== 03 complete ===\n")
