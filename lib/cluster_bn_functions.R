# =============================================================================
# 分群贝叶斯网络函数库
# =============================================================================
#
# 职责：县域聚类、分群BN拟合、高斯BN解析预测（替代cpdist循环）
# 依赖：bnlearn, stats
#
# =============================================================================

library(bnlearn)

# =============================================================================
# 第一部分：县域聚类
# =============================================================================

# -----------------------------------------------------------------------------
# 基于τ̂矩阵的县域聚类
# tau_matrix: n_counties × n_models 的矩阵
# k_range:   搜索的K值范围
# 返回：list(assignments, optimal_k, diagnostics)
# -----------------------------------------------------------------------------
cluster_counties_by_tau <- function(tau_matrix, k_range = 2:8, seed = 2024) {
  set.seed(seed)

  tau_scaled <- scale(tau_matrix)
  tau_scaled[!is.finite(tau_scaled)] <- 0

  # 计算每个K的轮廓系数
  sil_scores <- vapply(k_range, function(k) {
    km <- kmeans(tau_scaled, centers = k, nstart = 25, iter.max = 100)
    ss <- cluster::silhouette(km$cluster, dist(tau_scaled))
    mean(ss[, "sil_width"])
  }, numeric(1))

  optimal_k <- k_range[which.max(sil_scores)]
  cat(sprintf("  optimal K = %d (silhouette = %.3f)\n", optimal_k, max(sil_scores)))

  # 用最优K重新聚类
  km_final <- kmeans(tau_scaled, centers = optimal_k, nstart = 50, iter.max = 200)

  # 每群组的τ̂均值 profile
  cluster_profiles <- do.call(rbind, lapply(seq_len(optimal_k), function(k) {
    members <- km_final$cluster == k
    colMeans(tau_matrix[members, , drop = FALSE])
  }))
  rownames(cluster_profiles) <- paste0("Cluster_", seq_len(optimal_k))

  diagnostics <- data.frame(
    K = k_range,
    Silhouette = sil_scores,
    stringsAsFactors = FALSE
  )

  list(
    assignments = km_final$cluster,
    optimal_k   = optimal_k,
    profiles    = cluster_profiles,
    diagnostics = diagnostics,
    sizes       = as.integer(table(km_final$cluster))
  )
}

# =============================================================================
# 第二部分：DAG结构修正
# =============================================================================

# -----------------------------------------------------------------------------
# 从原始边表 + CF白名单/黑名单构建修正后的DAG
# original_edges: data.frame(From, To, ...)
# whitelist/blacklist: 2列矩阵(from, to)，来自 generate_bn_edge_lists()
# target_nodes: 保留的节点集合（去掉CPDS后为9个）
# 返回：bnlearn dag 对象
# -----------------------------------------------------------------------------
build_corrected_dag <- function(original_edges, whitelist, blacklist, target_nodes) {

  # 过滤：只保留target_nodes中的边
  edges_filtered <- original_edges[
    original_edges$From %in% target_nodes & original_edges$To %in% target_nodes, ]

  cat(sprintf("  original edges in scope: %d\n", nrow(edges_filtered)))

  # 分类每条原始边
  edge_status <- vapply(seq_len(nrow(edges_filtered)), function(i) {
    from <- edges_filtered$From[i]
    to   <- edges_filtered$To[i]

    in_wl <- any(whitelist[, 1] == from & whitelist[, 2] == to)
    in_bl <- any(blacklist[, 1] == from & blacklist[, 2] == to)

    if (in_bl) return("removed_by_cf")
    if (in_wl) return("confirmed_by_cf")
    return("kept_theoretical")
  }, character(1))

  edges_filtered$CF_Status <- edge_status

  # 只保留未被黑名单移除的边
  edges_keep <- edges_filtered[edge_status != "removed_by_cf", ]
  cat(sprintf("  edges after CF correction: %d (removed %d)\n",
              nrow(edges_keep), sum(edge_status == "removed_by_cf")))

  # 识别白名单中原始DAG没有的新边，需要添加
  existing_pairs <- paste(edges_filtered$From, edges_filtered$To, sep = "->")
  wl_pairs <- paste(whitelist[, 1], whitelist[, 2], sep = "->")
  new_from_wl <- setdiff(wl_pairs, existing_pairs)

  if (length(new_from_wl) > 0) {
    parts <- strsplit(new_from_wl, "->")
    new_edges <- data.frame(
      From = vapply(parts, `[`, character(1), 1),
      To   = vapply(parts, `[`, character(1), 2),
      stringsAsFactors = FALSE
    )
    # 只保留两端都在target_nodes中的
    new_edges <- new_edges[new_edges$From %in% target_nodes &
                           new_edges$To %in% target_nodes, ]
    cat(sprintf("  new edges from CF whitelist (not in theoretical DAG): %d\n",
                nrow(new_edges)))
    if (nrow(new_edges) > 0) {
      for (i in seq_len(nrow(new_edges))) {
        cat(sprintf("    + %s -> %s\n", new_edges$From[i], new_edges$To[i]))
      }
    }
  } else {
    new_edges <- data.frame(From = character(0), To = character(0),
                            stringsAsFactors = FALSE)
    cat("  no new edges from CF whitelist\n")
  }

  # 构建DAG：先加原始保留边，再加白名单新边
  dag <- empty.graph(target_nodes)
  failed <- character(0)

  # VECTORIZATION CHECK: set.arc必须逐条添加并检查环，无法向量化
  all_edges_to_add <- rbind(
    data.frame(From = as.character(edges_keep$From),
               To = as.character(edges_keep$To),
               Source = "theoretical",
               stringsAsFactors = FALSE),
    if (nrow(new_edges) > 0) {
      data.frame(From = as.character(new_edges$From),
                 To = as.character(new_edges$To),
                 Source = "cf_discovered",
                 stringsAsFactors = FALSE)
    }
  )

  for (i in seq_len(nrow(all_edges_to_add))) {
    from <- all_edges_to_add$From[i]
    to   <- all_edges_to_add$To[i]
    tryCatch({
      temp <- set.arc(dag, from, to)
      if (acyclic(temp)) {
        dag <- temp
      } else {
        failed <- c(failed, sprintf("%s -> %s (%s, cycle)", from, to,
                                    all_edges_to_add$Source[i]))
      }
    }, error = function(e) {
      failed <<- c(failed, sprintf("%s -> %s (%s, error: %s)", from, to,
                                   all_edges_to_add$Source[i], e$message))
    })
  }

  if (length(failed) > 0) {
    cat(sprintf("  %d edges skipped: %s\n", length(failed),
                paste(failed, collapse = ", ")))
  }

  cat(sprintf("  final DAG: %d nodes, %d arcs\n", length(nodes(dag)), narcs(dag)))

  # 完整的边报告（包含新增边）
  if (nrow(new_edges) > 0) {
    new_report <- data.frame(
      From = new_edges$From, To = new_edges$To,
      CF_Status = "added_by_cf",
      stringsAsFactors = FALSE
    )
    # 对齐列：原始报告可能有额外列
    for (col in setdiff(names(edges_filtered), names(new_report))) {
      new_report[[col]] <- NA
    }
    edge_report <- rbind(edges_filtered[, names(edges_filtered)],
                         new_report[, names(edges_filtered)])
  } else {
    edge_report <- edges_filtered
  }

  list(dag = dag, edge_report = edge_report)
}

# =============================================================================
# 第三部分：分群BN拟合
# =============================================================================

# -----------------------------------------------------------------------------
# 对每个群组分别拟合BN参数
# dag: bnlearn dag 对象
# network_data: data.frame, 列 = DAG节点
# cluster_assignments: integer vector, 群组归属
# 返回：list of fitted bn objects
# -----------------------------------------------------------------------------
fit_cluster_bns <- function(dag, network_data, cluster_assignments) {
  K <- max(cluster_assignments)
  bn_list <- vector("list", K)

  # VECTORIZATION CHECK: 每个群组拟合不同参数，模型训练无法向量化
  for (k in seq_len(K)) {
    members <- cluster_assignments == k
    n_k <- sum(members)
    cat(sprintf("  Cluster %d: %d counties\n", k, n_k))

    if (n_k < 30) {
      warning(sprintf("Cluster %d has only %d counties, BN fit may be unstable", k, n_k))
    }

    data_k <- network_data[members, , drop = FALSE]
    data_k[] <- lapply(data_k, as.numeric)

    bn_list[[k]] <- bn.fit(dag, data_k, method = "mle-g")
  }

  bn_list
}

# =============================================================================
# 第四部分：高斯BN解析预测（核心性能优化）
# =============================================================================

# -----------------------------------------------------------------------------
# 单节点解析预测（向量化，一次处理所有样本）
# fitted_bn: bn.fit 对象
# node: 要预测的节点名
# evidence_df: data.frame, 列含该节点的所有父节点
# 返回：list(mean = numeric vector, sd = scalar)
# -----------------------------------------------------------------------------
predict_bn_node_analytical <- function(fitted_bn, node, evidence_df) {
  node_fit <- fitted_bn[[node]]
  coefs    <- coef(node_fit)
  sigma    <- node_fit$sd

  parent_names <- names(coefs)[-1]  # 第一个是截距

  if (length(parent_names) == 0) {
    # 无父节点：所有样本预测为截距
    pred_mean <- rep(coefs[1], nrow(evidence_df))
  } else {
    # 矩阵乘法：pred = intercept + X %*% beta
    X <- as.matrix(evidence_df[, parent_names, drop = FALSE])
    pred_mean <- as.numeric(coefs[1] + X %*% coefs[-1])
  }

  list(mean = pred_mean, sd = sigma)
}

# -----------------------------------------------------------------------------
# 多节点批量预测（按拓扑序传播）
# fitted_bn: bn.fit 对象
# target_nodes: 要预测的节点名向量（如 c("CPI","CPS","CPA")）
# evidence_df: data.frame, 列含所有非目标节点
# 返回：data.frame, 列 = target_nodes 的预测均值 + _sd 列
# -----------------------------------------------------------------------------
predict_bn_targets_analytical <- function(fitted_bn, target_nodes, evidence_df) {
  # 构建完整数据帧（先填入证据，目标节点先占位为NA）
  all_nodes <- names(fitted_bn)
  n <- nrow(evidence_df)

  full_data <- data.frame(matrix(NA_real_, nrow = n, ncol = length(all_nodes)))
  names(full_data) <- all_nodes

  # 填入已知证据
  evidence_cols <- intersect(names(evidence_df), all_nodes)
  full_data[, evidence_cols] <- evidence_df[, evidence_cols]

  # 按拓扑序预测（确保父节点在子节点之前被预测）
  dag <- bn.net(fitted_bn)
  topo_order <- node.ordering(dag)

  result_sd <- list()

  for (node in topo_order) {
    if (node %in% evidence_cols) next  # 已有证据，跳过

    parent_names <- parents(dag, node)

    if (length(parent_names) == 0) {
      pred <- predict_bn_node_analytical(fitted_bn, node, full_data)
    } else {
      # 确保所有父节点已有值
      missing_parents <- parent_names[vapply(parent_names, function(p) all(is.na(full_data[[p]])), logical(1))]
      if (length(missing_parents) > 0) {
        warning(sprintf("Node %s: parents %s have no values, using unconditional mean",
                       node, paste(missing_parents, collapse = ", ")))
      }
      pred <- predict_bn_node_analytical(fitted_bn, node, full_data)
    }

    full_data[[node]] <- pred$mean
    result_sd[[node]] <- pred$sd
  }

  # 组装输出
  result <- full_data[, target_nodes, drop = FALSE]
  for (tn in target_nodes) {
    result[[paste0(tn, "_sd")]] <- result_sd[[tn]] %||% NA_real_
  }

  result
}

# -----------------------------------------------------------------------------
# 分群预测：根据每县所属群组选择对应BN进行预测
# cluster_bns: list of bn.fit objects
# cluster_assignments: integer vector
# target_nodes: 要预测的节点名
# evidence_df: data.frame, 所有县的证据数据
# 返回：data.frame, 含 target_nodes 预测值 + sd
# -----------------------------------------------------------------------------
predict_by_cluster <- function(cluster_bns, cluster_assignments, target_nodes, evidence_df) {
  n <- nrow(evidence_df)
  K <- length(cluster_bns)

  # 预分配结果
  result_cols <- c(target_nodes, paste0(target_nodes, "_sd"))
  result <- data.frame(matrix(NA_real_, nrow = n, ncol = length(result_cols)))
  names(result) <- result_cols

  # 按群组批量预测（避免逐县循环）
  # VECTORIZATION CHECK: 按群组分批，群组内是向量化矩阵运算
  for (k in seq_len(K)) {
    mask <- cluster_assignments == k
    if (!any(mask)) next

    pred_k <- predict_bn_targets_analytical(
      cluster_bns[[k]],
      target_nodes,
      evidence_df[mask, , drop = FALSE]
    )

    result[mask, ] <- pred_k
  }

  result
}

# %||% 运算符（如果左侧为NULL则返回右侧）
`%||%` <- function(a, b) if (is.null(a)) b else a

cat("cluster_bn_functions.R loaded\n")
