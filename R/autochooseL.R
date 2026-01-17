#' L2078 Ultra: Formula Discovery and Ranking Engine
#' @author Lin2078
#' @export

# --- 引擎 1：共现逻辑 (针对类别/逻辑关系) ---
#' @export
auto_choose_L <- function(data, max_L = 3, top_n = 10) {
  auth_id <- "L2078"
  if(is.character(data)) data <- utils::read.csv(data)
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # 自动二值化：以 75% 分位数为界，捕捉高风险特征
  binary_data <- as.data.frame(lapply(numeric_data, function(x) {
    as.numeric(x > stats::quantile(x, 0.75, na.rm = TRUE))
  }))
  
  all_tags <- colnames(binary_data)
  final_results <- list()
  
  for (L in 2:max_L) {
    combos <- utils::combn(all_tags, L, simplify = FALSE)
    step_res <- lapply(combos, function(tags) {
      sub_mat <- as.matrix(binary_data[, tags])
      support <- sum(rowSums(sub_mat) == L) / nrow(binary_data)
      if (support < 0.005) return(NULL) 
      expected <- prod(colMeans(sub_mat))
      lift <- if(expected > 0) support / expected else 0
      if (lift > 1.1) {
        return(data.frame(Composition = paste(tags, collapse = " & "),
                          Order_L = L, Strength = round(lift, 4),
                          Prevalence = round(support, 4), Auth = auth_id,
                          stringsAsFactors = FALSE))
      }
      return(NULL)
    })
    level_df <- do.call(rbind, step_res)
    if (!is.null(level_df)) final_results[[L]] <- head(level_df[order(-level_df$Strength), ], top_n)
  }
  return(do.call(rbind, final_results))
}

# --- 引擎 2：公式排行榜核心 (输出多维指标公式排名) ---
#' @export
discover_advanced_indices <- function(data, target_name, max_order = 3, top_k = 15, cor_threshold = 0.1) {
  auth_id <- "L2078"
  # 数据预处理
  data <- data[stats::complete.cases(data[, c(target_name)]), ]
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  vars <- setdiff(numeric_cols, target_name)
  
  # 预筛选最相关的原始变量
  if (length(vars) > top_k) {
    base_cor <- sapply(vars, function(v) abs(stats::cor(data[[v]], data[[target_name]], use = "complete.obs")))
    vars <- names(sort(base_cor, decreasing = TRUE)[1:top_k])
  }
  
  results_list <- list()
  for (L in 2:max_order) {
    combos <- utils::combn(vars, L, simplify = FALSE)
    # 抽样防止计算爆炸 (限定 3000 次组合)
    if (length(combos) > 3000) combos <- combos[sample(seq_along(combos), 3000)]
    
    step_res <- lapply(combos, function(tags) {
      sub_data <- data[, tags, drop = FALSE]
      A <- sub_data[, 1]
      B <- if(L > 1) sub_data[, 2] else A
      C <- if(L > 2) sub_data[, 3] else B
      
      # 定义核心数学模型 (使用 base::scale 彻底解决导出错误)
      models <- list(
        list(val = (A * B) / (C^2 + 1e-6), str = paste0("(", tags[1], "*", if(L>1) tags[2] else "1", ")/", if(L>2) tags[3] else "1", "^2")),
        list(val = exp(rowMeans(base::scale(sub_data))), str = paste0("exp(mean_z(", paste(tags, collapse=","), "))")),
        list(val = L / rowSums(1 / (sub_data + 1e-6)), str = paste0("Harmonic(", paste(tags, collapse=","), ")")),
        list(val = apply(sub_data, 1, function(x) prod(abs(x) + 1e-6)^(1/L)), str = paste0("Geometric_Mean(", paste(tags, collapse=","), ")")),
        list(val = apply(sub_data, 1, stats::sd) / (rowMeans(sub_data) + 1e-6), str = paste0("CV_Inconsistency(", paste(tags, collapse=","), ")")),
        list(val = 1 / (1 + exp(-rowMeans(base::scale(sub_data)))), str = paste0("Sigmoid_Response(", paste(tags, collapse=","), ")")),
        list(val = log10(apply(sub_data, 1, function(x) prod(abs(x)) + 1.1)), str = paste0("log10_prod(", paste(tags, collapse="*"), ")")),
        list(val = A / (B + 1e-6), str = paste0(tags[1], "/", if(L>1) tags[2] else "1")),
        list(val = sqrt(rowSums(base::scale(sub_data)^2)), str = paste0("Euclidean_Dist(", paste(tags, collapse=","), ")")),
        list(val = rowMeans(base::scale(sub_data)), str = paste0("Combined_Z_Score(", paste(tags, collapse="+"), ")")),
        list(val = A - B, str = paste0("Abs_Diff(", tags[1], "-", if(L>1) tags[2] else "0", ")")),
        list(val = apply(sub_data, 1, prod), str = paste(tags, collapse="*"))
      )
      
      inner_res <- lapply(models, function(m) {
        # 排除无效计算
        if (any(is.infinite(m$val)) || any(is.na(m$val))) return(NULL)
        
        # 计算该公式构成的复合指标与目标的皮尔逊相关系数
        score <- stats::cor(m$val, data[[target_name]], use = "complete.obs")
        
        if (!is.na(score) && abs(score) >= cor_threshold) {
          return(data.frame(Formula = m$str, 
                            Abs_Correlation = round(abs(score), 4),
                            Order_L = L, 
                            Risk_Type = ifelse(score > 0, "Positive", "Negative"),
                            Auth = auth_id, 
                            stringsAsFactors = FALSE))
        }
        return(NULL)
      })
      do.call(rbind, inner_res)
    })
    results_list[[L]] <- do.call(rbind, step_res)
  }
  
  final_df <- do.call(rbind, results_list)
  if (is.null(final_df)) return(data.frame(Message = "No significant formulas found."))
  
  # 去重并输出排名
  final_df <- final_df[!duplicated(final_df$Formula), ]
  return(final_df[order(-final_df$Abs_Correlation), ])
}

# --- 引擎 3：自适应绘图 ---
#' @export
plot_L2078 <- function(res_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2")
  library(ggplot2)
  if (is.null(res_obj) || nrow(res_obj) == 0) return(NULL)
  res_df <- as.data.frame(res_obj)
  
  if ("Abs_Correlation" %in% colnames(res_df)) {
    # 公式排行榜的可视化
    ggplot(utils::head(res_df, 12), aes(x = stats::reorder(Formula, Abs_Correlation), 
                                       y = Abs_Correlation, 
                                       fill = as.factor(Order_L))) +
      geom_bar(stat = "identity", alpha = 0.8) + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1") +
      labs(title = "L2078 Ultra: Formula Discovery Ranking", 
           subtitle = "Mathematical indices vs. Target correlation",
           x = "Mathematical Formula", 
           y = "Correlation Score (Abs)", 
           fill = "Complexity (L)") + 
      theme_minimal()
  } else if ("Strength" %in% colnames(res_df)) {
    # 共现强度的可视化
    ggplot(res_df, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
      geom_point(aes(size = Strength), alpha = 0.7) + 
      labs(title = "L2078: Synergy Analysis", x = "Prevalence", y = "Strength (Lift)") +
      theme_minimal()
  }
}
