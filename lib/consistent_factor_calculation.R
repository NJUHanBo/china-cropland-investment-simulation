# =============================================================================
# 一致性因子计算函数
# =============================================================================
# 
# 功能：确保因子计算与训练时完全一致
# 方法：使用PCA降维，与stage4a_regression_forest_CPI_CPS_CPA.R中的方法完全一致
# 
# =============================================================================

library(dplyr)

# 定义与训练时完全一致的因子组
get_factor_groups <- function() {
  list(
    城市化 = c("ΔC_URB1", "ΔC_URB2", "ΔC_URB3", "ΔC_URB4", 
              "ΔR_URB1", "ΔR_URB2", "ΔR_URB3", "ΔR_URB4"),
    气候变化 = c("Clim1", "Clim2", "Clim3"),
    保护政策 = c("Conserv.1", "Conserv.2", "Conserv.3"),
    土地利用 = c("ΔLAN1", "ΔLAN2", "ΔLAN3", "ΔLAN4"),
    利用强度 = c("ΔUsing1", "ΔUsing2", "ΔUsing3", "ΔUsing4", "ΔUsing5"),
    产权制度 = c("ΔProp1", "ΔProp2", "ΔProp3")
  )
}

# 核心因子计算函数（与训练时完全一致）
calculate_factors <- function(data, verbose = FALSE) {
  
  if(verbose) {
    cat("🔧 计算一致性因子（使用PCA降维）...\n")
  }
  
  # 获取因子组定义
  factor_groups <- get_factor_groups()
  
  # 计算因子（用于回归森林输入）
  factors <- data.frame(
    城市化 = numeric(nrow(data)),
    气候变化 = numeric(nrow(data)),
    保护政策 = numeric(nrow(data)),
    土地利用 = numeric(nrow(data)),
    利用强度 = numeric(nrow(data)),
    产权制度 = numeric(nrow(data))
  )
  
  for(dim_name in names(factor_groups)) {
    
    if(verbose) {
      cat(sprintf("  处理维度: %s\n", dim_name))
    }
    
    # 获取该维度的原始变量
    raw_vars <- factor_groups[[dim_name]]
    available_vars <- intersect(raw_vars, names(data))
    
    if(length(available_vars) == 0) {
      if(verbose) {
        cat(sprintf("    ⚠️ 维度 %s 的变量未找到，使用默认值\n", dim_name))
      }
      factors[[dim_name]] <- rep(0, nrow(data))
      next
    }
    
    # 提取该维度的数据
    dim_data <- data[, available_vars, drop = FALSE]
    
    # 处理缺失值
    dim_data <- dim_data[complete.cases(dim_data), , drop = FALSE]
    
    if(nrow(dim_data) < 10) {
      if(verbose) {
        cat(sprintf("    ⚠️ 维度 %s 有效样本不足，使用默认值\n", dim_name))
      }
      factors[[dim_name]] <- rep(0, nrow(data))
      next
    }
    
    # 使用PCA降维得到因子得分（与训练时完全一致）
    if(ncol(dim_data) > 1) {
      pca_result <- prcomp(dim_data, scale. = TRUE, center = TRUE)
      
      # 使用第一主成分作为因子得分
      factor_score <- pca_result$x[, 1]
      
      if(verbose) {
        # 解释方差比例
        var_explained <- summary(pca_result)$importance[2, 1]
        cat(sprintf("    ✅ PCA解释方差: %.3f\n", var_explained))
      }
      
    } else {
      # 只有一个变量时直接标准化
      factor_score <- as.numeric(scale(dim_data[, 1])[, 1])
      if(verbose) {
        cat(sprintf("    ✅ 单变量标准化\n"))
      }
    }
    
    # 保存因子得分
    factors[[dim_name]] <- factor_score
  }
  
  if(verbose) {
    cat(sprintf("✅ 一致性因子计算完成，%d个样本\n\n", nrow(factors)))
  }
  
  return(factors)
}

# 为了向后兼容，保留简化版本的函数名
calculate_simplified_factors <- function(data) {
  return(calculate_factors(data, verbose = FALSE))
}

cat("✅ 一致性因子计算函数加载完成\n")
cat("🔧 使用方法：\n")
cat("   - calculate_factors(data, verbose = TRUE): 完整PCA计算（带详细信息）\n")
cat("   - calculate_simplified_factors(data): 简化版本（用于回归森林，默认静默）\n")
cat("   - 确保与训练时的方法完全一致\n\n")
