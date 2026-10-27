# =============================================================================
# 因果森林函数库
# =============================================================================
#
# 职责：训练因果森林、提取县级异质性效应、ATE显著性检验
# 依赖：grf
#
# =============================================================================

library(grf)

# -----------------------------------------------------------------------------
# 训练单个因果森林
# W = 连续处理变量（某政策维度因子）
# Y = 结果变量（CPI/CPS/CPA）
# X = 混杂控制变量（其余5个维度因子）
# 返回：模型对象 + 县级τ̂ + ATE + 变量重要性
# -----------------------------------------------------------------------------
train_single_causal_forest <- function(Y, W, X,
                                       num.trees = 4000,
                                       min.node.size = 15,
                                       sample.fraction = 0.35,
                                       honesty = TRUE,
                                       seed = 2024) {
  cf <- causal_forest(
    X = as.matrix(X),
    Y = Y,
    W = W,
    num.trees = num.trees,
    min.node.size = min.node.size,
    sample.fraction = sample.fraction,
    honesty = honesty,
    honesty.fraction = 0.5,
    seed = seed
  )

  tau_hat_obj <- predict(cf, estimate.variance = TRUE)
  tau_hat <- drop(tau_hat_obj$predictions)
  tau_var <- drop(tau_hat_obj$variance.estimates)
  tau_se  <- sqrt(tau_var)

  ate_obj <- average_treatment_effect(cf)
  ate     <- ate_obj["estimate"]
  ate_se  <- ate_obj["std.err"]

  vi <- variable_importance(cf)
  vi_named <- setNames(as.numeric(vi), colnames(X))

  list(
    model     = cf,
    tau_hat   = tau_hat,
    tau_se    = tau_se,
    ate       = ate,
    ate_se    = ate_se,
    vi        = vi_named
  )
}

# -----------------------------------------------------------------------------
# 批量训练所有因果森林（4政策维度 × 3结果变量 = 12模型）
# factors: data.frame, 列名 = 6个维度因子（中文名）
# outcomes: data.frame, 列名含 CPI/CPS/CPA
# 返回：list of results + tau_matrix + ate_summary
# -----------------------------------------------------------------------------
run_all_causal_forests <- function(factors, outcomes, ...) {

  treatment_dims <- c("保护政策", "土地利用", "利用强度", "产权制度")
  all_dims       <- names(factors)
  outcome_vars   <- c("CPI", "CPS", "CPA")

  n_counties <- nrow(factors)
  n_models   <- length(treatment_dims) * length(outcome_vars)

  tau_matrix   <- matrix(NA_real_, nrow = n_counties, ncol = n_models)
  tau_se_matrix <- matrix(NA_real_, nrow = n_counties, ncol = n_models)
  col_names    <- character(n_models)
  ate_rows     <- vector("list", n_models)
  vi_list      <- vector("list", n_models)
  model_list   <- vector("list", n_models)

  idx <- 0L
  # VECTORIZATION CHECK: 外层循环遍历不同模型，每个模型的W/Y/X不同，无法向量化
  for (w_name in treatment_dims) {
    W <- factors[[w_name]]
    control_dims <- setdiff(all_dims, w_name)
    X <- factors[, control_dims, drop = FALSE]

    for (y_name in outcome_vars) {
      idx <- idx + 1L
      col_names[idx] <- paste0(w_name, "_on_", y_name)

      cat(sprintf("  [%d/%d] W=%s, Y=%s ...\n", idx, n_models, w_name, y_name))

      res <- train_single_causal_forest(
        Y = outcomes[[y_name]],
        W = W,
        X = X,
        ...
      )

      tau_matrix[, idx]    <- res$tau_hat
      tau_se_matrix[, idx] <- res$tau_se
      model_list[[idx]]    <- res$model
      vi_list[[idx]]       <- res$vi

      ci_lower <- res$ate - 1.96 * res$ate_se
      ci_upper <- res$ate + 1.96 * res$ate_se
      ate_rows[[idx]] <- data.frame(
        Treatment   = w_name,
        Outcome     = y_name,
        Model_Label = col_names[idx],
        ATE         = res$ate,
        ATE_SE      = res$ate_se,
        CI_Lower    = ci_lower,
        CI_Upper    = ci_upper,
        Significant = (ci_lower > 0) | (ci_upper < 0),
        stringsAsFactors = FALSE
      )

      cat(sprintf("    ATE = %.4f (SE=%.4f), significant = %s\n",
                  res$ate, res$ate_se,
                  ifelse(ate_rows[[idx]]$Significant, "YES", "NO")))
    }
  }

  colnames(tau_matrix) <- col_names
  colnames(tau_se_matrix) <- col_names
  ate_summary <- do.call(rbind, ate_rows)
  rownames(ate_summary) <- NULL

  # 变量重要性汇总
  vi_df <- do.call(rbind, lapply(seq_along(vi_list), function(i) {
    data.frame(
      Model    = col_names[i],
      Variable = names(vi_list[[i]]),
      Importance = as.numeric(vi_list[[i]]),
      stringsAsFactors = FALSE
    )
  }))

  list(
    tau_matrix    = tau_matrix,
    tau_se_matrix = tau_se_matrix,
    ate_summary   = ate_summary,
    vi_summary    = vi_df,
    models        = model_list,
    col_names     = col_names
  )
}

# -----------------------------------------------------------------------------
# 从ATE汇总表生成BN结构修正的白名单/黑名单
# ate_summary: run_all_causal_forests()的产出
# 返回：list(whitelist, blacklist) 每个是2列矩阵(from, to)
# -----------------------------------------------------------------------------
generate_bn_edge_lists <- function(ate_summary) {

  # 因子中文名 → BN节点名的映射（本项目中一致，无需转换）
  sig <- ate_summary[ate_summary$Significant, ]
  nonsig <- ate_summary[!ate_summary$Significant, ]

  whitelist <- if (nrow(sig) > 0) {
    as.matrix(sig[, c("Treatment", "Outcome")])
  } else {
    matrix(character(0), ncol = 2, dimnames = list(NULL, c("from", "to")))
  }

  blacklist <- if (nrow(nonsig) > 0) {
    as.matrix(nonsig[, c("Treatment", "Outcome")])
  } else {
    matrix(character(0), ncol = 2, dimnames = list(NULL, c("from", "to")))
  }

  colnames(whitelist) <- c("from", "to")
  colnames(blacklist) <- c("from", "to")

  list(whitelist = whitelist, blacklist = blacklist)
}

cat("causal_forest_functions.R loaded\n")
