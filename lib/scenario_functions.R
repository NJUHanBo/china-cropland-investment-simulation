# =============================================================================
# 情景模拟函数：部门配置 × 空间策略（与 04 的逻辑一致）
# =============================================================================

POLICY_CONFIGS <- list(
  I01 = c(3, 3, 3, 3), I02 = c(9, 1, 1, 1), I03 = c(1, 9, 1, 1), I04 = c(1, 1, 9, 1),
  I05 = c(1, 1, 1, 9), I06 = c(5, 5, 1, 1), I07 = c(5, 1, 5, 1), I08 = c(5, 1, 1, 5),
  I09 = c(1, 5, 5, 1), I10 = c(1, 5, 1, 5), I11 = c(1, 1, 5, 5), I12 = c(4, 4, 4, 0),
  I13 = c(4, 4, 0, 4), I14 = c(4, 0, 4, 4), I15 = c(0, 4, 4, 4)
)

ALLOCATION_STRATEGIES <- c(
  "统一投资", "CPDS雪中送炭", "CPDS锦上添花", "CPI雪中送炭", "CPI锦上添花",
  as.vector(outer(outer(paste0("保护", c("锦", "雪")), paste0("_土地", c("锦", "雪")), paste0),
                  outer(paste0("_利用", c("锦", "雪")), paste0("_产权", c("锦", "雪")), paste0),
                  paste0))
)

DIMENSION_NAMES <- c("保护", "土地", "利用", "产权")

driver_var_groups <- function(data) {
  list(grep("Conserv\\.", names(data), value = TRUE),
       grep("LAN", names(data), value = TRUE),
       grep("Using", names(data), value = TRUE),
       grep("Prop", names(data), value = TRUE))
}

# 某维度的空间分配乘数（预算守恒）；锦 = 高基线县多投，雪 = 低基线县多投
compute_spatial_mults <- function(allocation_strategy, dim_idx, base_mult, policy_data,
                                  external_data, var_group, cpds_values, cpi_values) {
  n <- nrow(external_data)
  if (base_mult <= 1) return(rep(1.0, n))
  policy_part <- base_mult - 1.0

  is_old <- allocation_strategy %in% c("CPDS雪中送炭", "CPDS锦上添花", "CPI雪中送炭", "CPI锦上添花")
  criterion <- if (is_old) {
    if (grepl("CPDS", allocation_strategy)) cpds_values else cpi_values
  } else {
    rowMeans(policy_data[, var_group, drop = FALSE], na.rm = TRUE)
  }
  rng <- range(criterion, na.rm = TRUE)
  crit <- (criterion - rng[1]) / max(rng[2] - rng[1], 1e-10)
  crit[!is.finite(crit)] <- 0

  is_snow <- if (is_old) {
    grepl("雪", allocation_strategy)
  } else {
    m <- regmatches(allocation_strategy,
                    regexec(paste0(DIMENSION_NAMES[dim_idx], "([锦雪])"), allocation_strategy))
    length(m[[1]]) > 1 && m[[1]][2] == "雪"
  }
  raw <- 1.0 + if (is_snow) policy_part * (2 - 2 * crit) else policy_part * (2 * crit)

  base_vals <- rowMeans(external_data[, var_group, drop = FALSE], na.rm = TRUE)
  target <- sum(base_vals * base_mult, na.rm = TRUE)
  total  <- sum(base_vals * raw, na.rm = TRUE)
  raw * (if (total > 0 && total != target) target / total else 1.0)
}

# 现状环境下所有 部门×空间 情景的县级政策效应（干预预测 − 模型基线）
simulate_policy_effects <- function(bn_list, group_ids, combined_data) {
  vg <- driver_var_groups(combined_data)
  targets <- c("CPI", "CPS", "CPA")
  base_f <- calculate_ml_guided_factors(combined_data, verbose = FALSE)
  base_p <- predict_by_cluster(bn_list, group_ids, targets, base_f)

  out <- vector("list", length(POLICY_CONFIGS) * length(ALLOCATION_STRATEGIES))
  i <- 0L
  for (p in names(POLICY_CONFIGS)) {
    bm <- 1 + POLICY_CONFIGS[[p]] / 12
    pol <- combined_data
    for (d in 1:4) if (bm[d] > 1) pol[, vg[[d]]] <- combined_data[, vg[[d]]] * bm[d]
    for (a in ALLOCATION_STRATEGIES) {
      fin <- pol
      if (a != "统一投资") {
        for (d in 1:4) if (bm[d] > 1) {
          sm <- compute_spatial_mults(a, d, bm[d], pol, combined_data, vg[[d]],
                                      combined_data$CPDS, combined_data$CPI_20year_mean)
          fin[, vg[[d]]] <- sweep(as.matrix(combined_data[, vg[[d]]]), 1, sm, `*`)
        }
      }
      pr <- predict_by_cluster(bn_list, group_ids, targets,
                               calculate_ml_guided_factors(fin, verbose = FALSE))
      i <- i + 1L
      out[[i]] <- data.frame(Policy_Config = p, Allocation_Strategy = a,
                             County_Code = combined_data$Code, Group = group_ids,
                             dCPI = pr$CPI - base_p$CPI, dCPS = pr$CPS - base_p$CPS,
                             dCPA = pr$CPA - base_p$CPA, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}
