# =============================================================================
# ML引导因子计算函数库
# =============================================================================
#
# 功能：使用真正的ML权重计算因子，替代PCA方法
# 权重来源：stage3_factor_weights_detail.csv（104个特征权重）
# 输出：6维因子得分 data.frame（城市化/气候变化/保护政策/土地利用/利用强度/产权制度）
#
# 重建说明：此文件根据 output.log 运行日志和调用脚本
#   000_final_完整1008组合双重方法样本级分析_21策略优化_正确BN版本.R
#   的接口规范反向工程重建，原文件已丢失。
#
# =============================================================================

cat("=== ML引导因子计算函数库 ===\n")
cat("使用真正的ML权重计算因子，替代PCA方法\n\n")

# 定义变量组（与 consistent_factor_calculation.R 保持一致）
.ml_factor_groups <- list(
  城市化 = c("ΔC_URB1", "ΔC_URB2", "ΔC_URB3", "ΔC_URB4",
             "ΔR_URB1", "ΔR_URB2", "ΔR_URB3", "ΔR_URB4"),
  气候变化 = c("Clim1", "Clim2", "Clim3"),
  保护政策 = c("Conserv.1", "Conserv.2", "Conserv.3"),
  土地利用 = c("ΔLAN1", "ΔLAN2", "ΔLAN3", "ΔLAN4"),
  利用强度 = c("ΔUsing1", "ΔUsing2", "ΔUsing3", "ΔUsing4", "ΔUsing5"),
  产权制度 = c("ΔProp1", "ΔProp2", "ΔProp3")
)

# 加载ML权重（跨目标变量取均值，得到每个原始变量的综合权重）
.load_ml_weights <- function(weights_path = "models/stage3_factor_weights_detail.csv") {
  if (!file.exists(weights_path)) {
    stop("未找到ML权重文件: ", weights_path)
  }
  
  weights_raw <- read.csv(weights_path, stringsAsFactors = FALSE)
  cat(sprintf("成功加载ML权重数据，包含 %d 个特征权重\n", nrow(weights_raw)))
  
  # 按 Feature + Dimension 分组，跨 Target_Variable 取均值
  agg <- aggregate(ML_Weight ~ Feature + Dimension, data = weights_raw, FUN = mean)
  
  # 转为嵌套 list: dimension -> named vector(feature -> weight)
  weight_list <- list()
  for (dim_name in unique(agg$Dimension)) {
    sub <- agg[agg$Dimension == dim_name, ]
    w <- sub$ML_Weight
    names(w) <- sub$Feature
    # 归一化使权重和为1
    w <- w / sum(w)
    weight_list[[dim_name]] <- w
  }
  
  return(weight_list)
}

# 核心函数：使用ML权重计算因子得分
calculate_ml_guided_factors <- function(data, verbose = TRUE,
                                        weights_path = "models/stage3_factor_weights_detail.csv") {
  
  if (verbose) {
    cat("使用ML权重计算因子...\n")
  }
  
  # 加载权重
  ml_weights <- .load_ml_weights(weights_path)
  
  n <- nrow(data)
  factors <- data.frame(
    城市化 = numeric(n),
    气候变化 = numeric(n),
    保护政策 = numeric(n),
    土地利用 = numeric(n),
    利用强度 = numeric(n),
    产权制度 = numeric(n)
  )
  
  for (dim_name in names(.ml_factor_groups)) {
    raw_vars <- .ml_factor_groups[[dim_name]]
    available_vars <- intersect(raw_vars, names(data))
    
    if (length(available_vars) == 0) {
      if (verbose) cat(sprintf("  维度 %s 的变量未找到，使用默认值\n", dim_name))
      factors[[dim_name]] <- rep(0, n)
      next
    }
    
    # 获取该维度的ML权重
    dim_weights <- ml_weights[[dim_name]]
    
    if (is.null(dim_weights)) {
      if (verbose) cat(sprintf("  维度 %s 的权重未找到，退回等权\n", dim_name))
      dim_data <- data[, available_vars, drop = FALSE]
      factors[[dim_name]] <- rowMeans(dim_data, na.rm = TRUE)
      next
    }
    
    # 只保留数据中存在的变量对应的权重
    common_vars <- intersect(available_vars, names(dim_weights))
    if (length(common_vars) == 0) {
      factors[[dim_name]] <- rep(0, n)
      next
    }
    
    w <- dim_weights[common_vars]
    w <- w / sum(w)  # 重新归一化
    
    dim_data <- as.matrix(data[, common_vars, drop = FALSE])
    factors[[dim_name]] <- as.numeric(dim_data %*% w)
    
    if (verbose) {
      cat(sprintf("  维度 %s: 使用 %d 个变量的ML权重\n", dim_name, length(common_vars)))
    }
  }
  
  if (verbose) {
    cat(sprintf("ML引导因子计算完成，%d 个样本\n\n", n))
  }
  
  return(factors)
}

# 别名函数
calculate_factors_ml_guided <- function(data, ...) {
  return(calculate_ml_guided_factors(data, ...))
}

cat("ML引导因子计算函数库加载完成\n")
cat("使用方法：\n")
cat("   - calculate_ml_guided_factors(data, verbose = TRUE): 使用ML权重计算因子\n")
cat("   - calculate_factors_ml_guided(data): 别名函数\n")
cat("   - 替代PCA方法，使用真正的机器学习权重\n\n")
