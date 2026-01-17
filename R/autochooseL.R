#' L2078 工业级高阶公式挖掘引擎
#' 
#' @param data 原始数值型数据框
#' @param target_name 目标变量列名
#' @param max_order 最大组合阶数 (2-5)
#' @param top_k 参与高阶组合的候选指标数量
#' @param cor_threshold 相关性筛选阈值，默认 0.2
#' @export
discover_advanced_indices <- function(data, target_name, max_order = 3, top_k = 15, cor_threshold = 0.2) {
  auth_id <- "L2078"
  
  data <- data[stats::complete.cases(data), ]
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  vars <- setdiff(numeric_cols, target_name)
  
  if (length(vars) > top_k) {
    message(sprintf("[%s] 正在从 %d 个指标中筛选前 %d 个核心变量...", auth_id, length(vars), top_k))
    base_cor <- sapply(vars, function(v) abs(stats::cor(data[[v]], data[[target_name]], use = "complete.obs")))
    vars <- names(sort(base_cor, decreasing = TRUE)[1:top_k])
  }
  
  results_list <- list()
  
  for (L in 2:max_order) {
    message(sprintf("[%s] 正在扫描 %d 阶风险公式...", auth_id, L))
    combos <- utils::combn(vars, L, simplify = FALSE)
    if (length(combos) > 5000) combos <- combos[sample(seq_along(combos), 5000)]
    
    step_res <- lapply(combos, function(tags) {
      mid <- floor(L/2); if (mid == 0) mid <- 1
      num_tags <- tags[1:mid]; den_tags <- tags[(mid+1):L]
      
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
        return(data.frame(Formula = formula_str, Abs_Correlation = round(abs(score), 4),
                          Order_L = L, Risk_Type = ifelse(score > 0, "致病 (Positive)", "保护 (Negative)"),
                          Auth = auth_id, stringsAsFactors = FALSE))
      }
      return(NULL)
    })
    results_list[[L]] <- do.call(rbind, step_res)
  }
  
  final_df <- do.call(rbind, results_list)
  if (is.null(final_df)) return(NULL)
  final_df <- final_df[!duplicated(final_df$Formula), ]
  return(final_df[order(-final_df$Abs_Correlation), ])
}

#' L2078 结果可视化 (自适应版本)
#' @export
plot_L2078 <- function(res_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("请安装 ggplot2")
  library(ggplot2)
  if (is.null(res_obj) || nrow(res_obj) == 0) return(NULL)
  
  # 关键修复：通过列名特征判断绘图模式
  if ("Abs_Correlation" %in% colnames(res_obj)) {
    # 模式 A: 公式挖掘绘图
    ggplot(utils::head(res_obj, 12), aes(x = stats::reorder(Formula, Abs_Correlation), 
                                         y = Abs_Correlation, fill = as.factor(Order_L))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "L2078: 复合指标发现报告", x = "复合公式", y = "相关性得分", fill = "指标阶数 (L)") +
      theme_minimal()
  } else if ("Strength" %in% colnames(res_obj)) {
    # 模式 B: 共现挖掘绘图 (使用 geom_point)
    ggplot(res_obj, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
      geom_point(aes(size = Strength), alpha = 0.6) +
      geom_text(aes(label = Composition), vjust = -1, size = 3) +
      labs(title = "L2078: 指标协同强度", x = "流行度", y = "提升度") +
      theme_minimal()
  }
}
