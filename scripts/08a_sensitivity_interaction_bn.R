#!/usr/bin/env Rscript
# =============================================================================
# 08a — 敏感性分析：交互项BN
# =============================================================================
#
# 在BN因子中加入交互项（利用强度×气候变化、利用强度×城市化），
# 重新拟合cluster BN，用代表性情景子集对比效应差异。
#
# =============================================================================

library(bnlearn)
library(dplyr)

source("lib/ml_guided_factor_calculation.R")
source("lib/cluster_bn_functions.R")

cat("=== 08a: sensitivity — interaction BN ===\n\n")

# ---- 1. 加载原始数据和模型 --------------------------------------------------

basic_data  <- read.csv("data/basic_data.csv")
assignments <- read.csv("models/cluster_results/cluster_assignments.csv")
dag_orig    <- readRDS("models/corrected_bn/corrected_dag.RDS")

cluster_ids <- assignments$Cluster
K <- max(cluster_ids)

factors <- calculate_ml_guided_factors(basic_data, verbose = FALSE)

# ---- 2. 构建含交互项的数据 --------------------------------------------------

network_data_int <- data.frame(
  城市化   = factors$城市化,
  气候变化 = factors$气候变化,
  保护政策 = factors$保护政策,
  土地利用 = factors$土地利用,
  利用强度 = factors$利用强度,
  产权制度 = factors$产权制度,
  利用x气候 = factors$利用强度 * factors$气候变化,
  利用x城市 = factors$利用强度 * factors$城市化,
  CPI = basic_data$CPI,
  CPS = basic_data$CPS,
  CPA = basic_data$CPA,
  stringsAsFactors = FALSE
)
network_data_int[] <- lapply(network_data_int, as.numeric)

cat(sprintf("interaction data: %d rows x %d cols\n", nrow(network_data_int), ncol(network_data_int)))

# ---- 3. 构建含交互项的DAG ---------------------------------------------------

int_nodes <- c(nodes(dag_orig), "利用x气候", "利用x城市")
dag_int <- empty.graph(int_nodes)

orig_arcs <- arcs(dag_orig)
for (i in seq_len(nrow(orig_arcs))) {
  dag_int <- set.arc(dag_int, orig_arcs[i, "from"], orig_arcs[i, "to"])
}

int_edges <- list(
  c("利用x气候", "CPI"), c("利用x气候", "CPS"), c("利用x气候", "CPA"),
  c("利用x城市", "CPI"), c("利用x城市", "CPS"), c("利用x城市", "CPA")
)

for (edge in int_edges) {
  tryCatch({
    temp <- set.arc(dag_int, edge[1], edge[2])
    if (acyclic(temp)) dag_int <- temp
  }, error = function(e) NULL)
}

cat(sprintf("interaction DAG: %d nodes, %d arcs\n", length(nodes(dag_int)), narcs(dag_int)))

# ---- 4. 分群拟合交互项BN ---------------------------------------------------

cat("\nfitting interaction BNs ...\n")
int_bns <- fit_cluster_bns(dag_int, network_data_int, cluster_ids)

# ---- 5. 代表性情景对比 ------------------------------------------------------

cat("\ncomparing with original BN on representative scenarios ...\n")

orig_bns <- lapply(seq_len(K), function(k) {
  readRDS(sprintf("models/corrected_bn/cluster_bn_k%d.RDS", k))
})

external_configs <- list(E1 = c(0.85, 0.90), E2 = c(1.00, 1.00), E3 = c(1.30, 1.10))
target_nodes <- c("CPI", "CPS", "CPA")

urban_vars   <- grep("URB", names(basic_data), value = TRUE)
climate_vars <- grep("Clim", names(basic_data), value = TRUE)

comparison_rows <- vector("list", length(external_configs))
idx <- 0L

for (e_name in names(external_configs)) {
  idx <- idx + 1L
  ext_cfg <- external_configs[[e_name]]

  ext_data <- basic_data
  ext_data[, urban_vars]   <- ext_data[, urban_vars]   * ext_cfg[1]
  ext_data[, climate_vars] <- ext_data[, climate_vars] * ext_cfg[2]

  fct <- calculate_ml_guided_factors(ext_data, verbose = FALSE)

  # 原始BN证据
  evidence_orig <- data.frame(
    城市化 = fct$城市化, 气候变化 = fct$气候变化,
    保护政策 = fct$保护政策, 土地利用 = fct$土地利用,
    利用强度 = fct$利用强度, 产权制度 = fct$产权制度
  )

  # 交互项BN证据
  evidence_int <- evidence_orig
  evidence_int$利用x气候 <- fct$利用强度 * fct$气候变化
  evidence_int$利用x城市 <- fct$利用强度 * fct$城市化

  pred_orig <- predict_by_cluster(orig_bns, cluster_ids, target_nodes, evidence_orig)
  pred_int  <- predict_by_cluster(int_bns, cluster_ids, target_nodes, evidence_int)

  comparison_rows[[idx]] <- data.frame(
    External = e_name,
    Orig_CPI = mean(pred_orig$CPI, na.rm = TRUE),
    Orig_CPS = mean(pred_orig$CPS, na.rm = TRUE),
    Orig_CPA = mean(pred_orig$CPA, na.rm = TRUE),
    Int_CPI  = mean(pred_int$CPI, na.rm = TRUE),
    Int_CPS  = mean(pred_int$CPS, na.rm = TRUE),
    Int_CPA  = mean(pred_int$CPA, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

comp_df <- do.call(rbind, comparison_rows)
comp_df$Diff_CPI <- comp_df$Int_CPI - comp_df$Orig_CPI
comp_df$Diff_CPS <- comp_df$Int_CPS - comp_df$Orig_CPS
comp_df$Diff_CPA <- comp_df$Int_CPA - comp_df$Orig_CPA

# ---- 6. 交互项系数提取 ------------------------------------------------------

cat("\nextracting interaction coefficients ...\n")

int_coef_rows <- list()
ci <- 0L
for (k in seq_len(K)) {
  bn_k <- int_bns[[k]]
  for (target in target_nodes) {
    coefs <- coef(bn_k[[target]])
    for (int_name in c("利用x气候", "利用x城市")) {
      ci <- ci + 1L
      int_coef_rows[[ci]] <- data.frame(
        Cluster = k,
        Target = target,
        Interaction = int_name,
        Coefficient = if (int_name %in% names(coefs)) coefs[int_name] else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
}

int_coef_df <- do.call(rbind, int_coef_rows)

# ---- 7. 保存与打印 ----------------------------------------------------------

out_dir <- "output/csv/sensitivity"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(comp_df, file.path(out_dir, "interaction_bn_comparison.csv"), row.names = FALSE)
write.csv(int_coef_df, file.path(out_dir, "interaction_bn_coefficients.csv"), row.names = FALSE)

cat("\n=== Baseline Prediction Comparison (Orig vs Interaction BN) ===\n")
print(comp_df)

cat("\n=== Interaction Coefficients (by cluster) ===\n")
int_wide <- int_coef_df %>%
  mutate(Label = paste0(.data$Interaction, " → ", .data$Target)) %>%
  select("Label", "Cluster", "Coefficient") %>%
  tidyr::pivot_wider(names_from = "Cluster", values_from = "Coefficient", names_prefix = "C")
print(int_wide)

cat("\n=== E1 vs E3 baseline difference ===\n")
cat("Original BN:\n")
cat(sprintf("  CPI: E1=%.4f, E3=%.4f, diff=%.4f\n",
  comp_df$Orig_CPI[comp_df$External == "E1"],
  comp_df$Orig_CPI[comp_df$External == "E3"],
  comp_df$Orig_CPI[comp_df$External == "E1"] - comp_df$Orig_CPI[comp_df$External == "E3"]))
cat("Interaction BN:\n")
cat(sprintf("  CPI: E1=%.4f, E3=%.4f, diff=%.4f\n",
  comp_df$Int_CPI[comp_df$External == "E1"],
  comp_df$Int_CPI[comp_df$External == "E3"],
  comp_df$Int_CPI[comp_df$External == "E1"] - comp_df$Int_CPI[comp_df$External == "E3"]))

cat("\n=== 08a complete ===\n")
