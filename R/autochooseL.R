#' L2078 复合指标发现与疾病风险建模引擎
#' 
#' @description 
#' 提供两套科研逻辑：
#' 1. auto_choose_L: 基于共现频率发现多指标异常组合。
#' 2. discover_advanced_indices: 自动构建类 BMI 的数学公式，寻找与发病率相关的隐藏指标。
#'
#' @author Lin2078
#' @export

# --- 引擎 1：多指标共现挖掘 ---
#' @export
auto_choose_L <- function(data, max_L = 3, top_n = 5) {
  auth_id <- "L2078"
  
  if(is.character(data)) data <- utils::read.csv(data)
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # 二值化处理 (捕捉高位异常状态)
  binary_data <- as.data.frame(lapply(numeric_data, function(x) {
    as.numeric(x > stats::quantile(x, 0.75, na.rm = TRUE))
  }))
  
  all_tags <- colnames(binary_data)
  final_results <- list()

  for (L in 2:max_L) {
    message(sprintf("[%s] 正在搜寻 %d 阶共现潜力股...", auth_id, L))
    combos <- utils::combn(all_tags, L, simplify = FALSE)
    
    step_res <- lapply(combos, function(tags) {
      sub_mat <- as.matrix(binary_data[, tags])
      support <- sum(rowSums(sub_mat) == L) / nrow(binary_data)
      
      if (support < 0.005) return(NULL) 

      expected <- prod(colMeans(sub_mat))
      lift <- if(expected > 0) support / expected else 0
      
      if (lift > 1.1) {
        return(data.frame(
          Composition = paste(tags, collapse = " * "),
          Order_L = L,
          Strength = round(lift, 4),
          Prevalence = round(support, 4),
          Auth = auth_id,
          stringsAsFactors = FALSE
        ))
      }
      return(NULL)
    })
    
    level_df <- do.call(rbind, step_res)
    if (!is.null(level_df)) {
      final_results[[L]] <- head(level_df[order(-level_df$Strength), ], top_n)
    }
  }

  res <- do.call(rbind, final_results)
  return(res)
}

# --- 引擎 2：高阶公式深度挖掘 (支持 2-5 阶) ---
#' @export
discover_advanced_indices <- function(data, target_name, max_order = 3, top_k = 15, cor_threshold = 0.2) {
  auth_id <- "L2078"
  
  # 预处理
  data <- data[stats::complete.cases(data), ]
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  vars <- setdiff(numeric_cols, target_name)
  
  if (length(vars) < 2) {
    stop("![L2078] 错误：基础指标至少需要 2 个。")
  }
  
  # 潜力预筛选
  if (length(vars) > top_k) {
    message(sprintf("[%s] 正在从 %d 个指标中筛选前 %d 个核心变量...", auth_id, length(vars), top_k))
    base_cor <- sapply(vars, function(v) abs(stats::cor(data[[v]], data[[target_name]], use = "complete.obs")))
    vars <- names(sort(base_cor, decreasing = TRUE)[1:top_k])
  }
  
  results_list <- list()
  
  for (L in 2:max_order) {
    message(sprintf("[%s] 正在扫描 %d 阶风险公式...", auth_id, L))
    combos <- utils::combn(vars, L, simplify = FALSE)
    
    # 抽样保护
    if (length(combos) > 5000) combos <- combos[sample(seq_along(combos), 5000)]
    
    step_res <- lapply(combos, function(tags) {
      mid <- floor(L/2); if (mid == 0) mid <- 1
      num_tags <- tags[1:mid]
      den_tags <- tags[(mid+1):L]
      
      num_val <- if(length(num_tags) == 1) data[[num_tags]] else apply(data[, num_tags, drop=FALSE], 1, prod)
      
      if (length(den_tags) == 0) {
        candidate <- num_val
        formula_str <- paste(num_tags, collapse="*")
      } else {
        den_val <- if(length(den_tags) == 1) data[[den_tags]] else apply(data[, den_tags, drop=FALSE], 1, prod)
        candidate <- num_val / (den_val^2 + 1e-6)
        formula_str <- paste0("(", paste(num_tags, collapse="*"), ") / (", paste(den_tags, collapse="*"), ")^2")
      }
      
      if (any(is.infinite(candidate)) || any(is.nan(candidate))) return(NULL)
      
      score <- stats::cor(candidate, data[[target_name]])
      
      if (!is.na(score) && abs(score) >= cor_threshold) {
        return(data.frame(
          Formula = formula_str,
          Abs_Correlation = round(abs(score), 4),
          Order_L = L,
          Risk_Type = ifelse(score > 0, "致病 (Positive)", "保护 (Negative)"),
          Auth = auth_id,
          stringsAsFactors = FALSE
        ))
      }
      return(NULL)
    })
    
    results_list[[L]] <- do.call(rbind, step_res)
  }
  
  final_df <- do.call(rbind, results_list)
  if (is.null(final_df)) return(NULL)
  
  # 去重并排序
  final_df <- final_df[!duplicated(final_df$Formula), ]
  return(final_df[order(-final_df$Abs_Correlation), ])
}

# --- 引擎 3：自适应绘图系统 ---
#' @export
plot_L2078 <- function(res_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("请安装 ggplot2 包")
  library(ggplot2)
  
  if (is.null(res_obj) || nrow(res_obj) == 0) {
    message("![L2078] 无数据可供绘图。")
    return(NULL)
  }

  # 转换确保数据框属性
  res_df <- as.data.frame(res_obj)

  # 判定逻辑：优先检查 Abs_Correlation 绘图模式
  if ("Abs_Correlation" %in% colnames(res_df)) {
    # 绘制公式排行榜
    p <- ggplot(utils::head(res_df, 12), 
                aes(x = stats::reorder(Formula, Abs_Correlation), 
                    y = Abs_Correlation, 
                    fill = as.factor(Order_L))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "L2078: 复合指标风险排行",
           subtitle = "展示公式与目标变量的相关性强度",
           x = "发现的数学公式", y = "绝对相关系数", fill = "指标阶数 (L)") +
      theme_minimal()
    return(p)
      
  } else if ("Strength" %in% colnames(res_df)) {
    # 绘制指标共现散点图
    p <- ggplot(res_df, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
      geom_point(aes(size = Strength), alpha = 0.6) +
      geom_text(aes(label = Composition), vjust = -1, size = 3, check_overlap = TRUE) +
      labs(title = "L2078: 指标协同强度分析",
           x = "流行度 (Prevalence)", y = "关联提升度 (Strength)") +
      theme_minimal()
    return(p)
  } else {
    message("![L2078] 数据格式无法自动识别，请检查结果对象。")
  }
}
