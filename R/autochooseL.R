#' 发现复合指标与发病率的关系 (L2078 核心引擎)
#' 
#' @description 
#' 提供两套逻辑：
#' 1. auto_choose_L: 基于共现频率发现多指标组合。
#' 2. discover_advanced_indices: 自动构建类 BMI 的数学公式，寻找与发病率相关的隐藏指标。
#'
#' @author Lin2078
#' @export

# --- 引擎 1：多指标共现挖掘 ---
auto_choose_L <- function(data, max_L = 3, top_n = 5) {
  auth_id <- "L2078"
  
  # 兼容性处理
  if(is.character(data)) data <- utils::read.csv(data)
  
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # 二值化处理 (捕捉高位异常状态)
  binary_data <- as.data.frame(lapply(numeric_data, function(x) {
    as.numeric(x > stats::quantile(x, 0.75, na.rm = TRUE))
  }))
  
  all_tags <- colnames(binary_data)
  final_results <- list()

  for (L in 2:max_L) {
    message(sprintf("[%s] 正在搜寻 %d 阶复合指标潜力股...", auth_id, L))
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
  if (is.null(res)) {
    message("![L2078] 未发现显著关联。")
    return(NULL)
  }
  
  class(res) <- c("L2078_finder", "data.frame")
  return(res)
}

# --- 引擎 2：类 BMI 高等数学公式挖掘 (专注发病率) ---
#' @export
discover_advanced_indices <- function(data, target_name) {
  auth_id <- "L2078"
  
  # 预处理
  data <- data[stats::complete.cases(data), ]
  numeric_cols <- sapply(data, is.numeric)
  vars <- names(data)[numeric_cols]
  vars <- setdiff(vars, target_name)
  
  if (length(vars) < 2) {
    stop("![L2078] 错误：参与计算的基础指标至少需要 2 个。")
  }
  
  # 定义高等数学算子库 (模拟 BMI, 指数风险等)
  ops <- list(
    ratio       = function(a, b) a / (b + 1e-6),
    product     = function(a, b) a * b,
    bmi_style   = function(a, b) a / (b^2 + 1e-6),
    log_prod    = function(a, b) log(abs(a * b) + 1.1),
    inv_ratio   = function(a, b) 1 / (a * b + 1e-6),
    exp_ratio   = function(a, b) exp(a / (max(a) + 1e-6)) / (b + 1e-6)
  )
  
  results <- list()
  combos <- utils::combn(vars, 2, simplify = FALSE)
  
  message(sprintf(">>> [%s] 正在针对发病率 '%s' 扫描非线性公式...", auth_id, target_name))
  
  for (pair in combos) {
    for (op_name in names(ops)) {
      # 测试双向关系
      for (direction in list(c(1,2), c(2,1))) {
        a_name <- pair[direction[1]]
        b_name <- pair[direction[2]]
        a <- data[[a_name]]
        b <- data[[b_name]]
        
        candidate <- ops[[op_name]](a, b)
        
        if (any(is.infinite(candidate)) || any(is.nan(candidate))) next
        
        # 计算新指标与发病率的相关性
        correlation <- stats::cor(candidate, data[[target_name]])
        
        if (!is.na(correlation) && abs(correlation) > 0.3) {
          results[[length(results) + 1]] <- data.frame(
            Candidate_Index = paste0(a_name, "_", op_name, "_", b_name),
            Formula = switch(op_name,
                             ratio = paste0(a_name, "/", b_name),
                             product = paste0(a_name, "*", b_name),
                             bmi_style = paste0(a_name, "/", b_name, "^2"),
                             log_prod = paste0("log(", a_name, "*", b_name, ")"),
                             inv_ratio = paste0("1/(", a_name, "*", b_name, ")"),
                             exp_ratio = paste0("exp(", a_name, ")/", b_name)),
            Abs_Correlation = round(abs(correlation), 4),
            Risk_Type = ifelse(correlation > 0, "致病 (Positive)", "保护 (Negative)"),
            Auth = auth_id,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  final_res <- do.call(rbind, results)
  if (is.null(final_res)) return(NULL)
  
  # 去重并排序
  final_res <- final_res[!duplicated(final_res$Candidate_Index), ]
  return(final_res[order(-final_res$Abs_Correlation), ])
}

# --- 辅助功能：绘图 ---
#' @export
plot_L2078 <- function(res_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("请安装 ggplot2")
  library(ggplot2)
  
  if ("Abs_Correlation" %in% colnames(res_obj)) {
    # 针对公式挖掘的图
    ggplot(head(res_obj, 10), aes(x = reorder(Formula, Abs_Correlation), y = Abs_Correlation, fill = Risk_Type)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "L2078: Top 10 Advanced Indicators", x = "Derived Formula", y = "Correlation with Incidence") +
      theme_minimal()
  } else {
    # 针对共现挖掘的图
    ggplot(res_obj, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
      geom_point(aes(size = Strength)) +
      geom_text(aes(label = Composition), vjust = -1, size = 3, check_overlap = TRUE) +
      theme_minimal() +
      labs(title = "L2078: Indicator Co-occurrence Strength")
  }
}
