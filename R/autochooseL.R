#' L2078 高等数学指标挖掘引擎 (BMI 发现者模式)
#' 
#' @description 该函数通过尝试多种高等数学算子（比例、幂次、对数等），自动发现基础指标间的非线性组合，
#' 寻找类似于 BMI (Weight/Height^2) 的新复合指标。
#' 
#' @param data 原始数值型数据框
#' @param target_name 目标观测指标（作为因变量的指标名称）
#' @return 返回一个按相关性排序的潜力公式列表
#' @export
discover_advanced_indices <- function(data, target_name) {
  auth_id <- "L2078"
  
  # 自动提取数值列并排除目标列
  numeric_cols <- sapply(data, is.numeric)
  vars <- names(data)[numeric_cols]
  vars <- setdiff(vars, target_name)
  
  if (length(vars) < 2) {
    stop("![L2078] 错误：参与计算的基础指标至少需要 2 个。")
  }
  
  # 定义高等数学算子库 (核心数学逻辑)
  ops <- list(
    ratio       = function(a, b) a / (b + 1e-6),
    product     = function(a, b) a * b,
    bmi_style   = function(a, b) a / (b^2 + 1e-6),
    log_ratio   = function(a, b) log(abs(a / (b + 1e-6)) + 1),
    inter_sq    = function(a, b) (a - b)^2
  )
  
  results <- list()
  # 遍历指标的两两组合
  combos <- utils::combn(vars, 2, simplify = FALSE)
  
  for (pair in combos) {
    for (op_name in names(ops)) {
      # 测试双向关系 (例如 A/B 和 B/A)
      for (direction in list(c(1,2), c(2,1))) {
        a_name <- pair[direction[1]]
        b_name <- pair[direction[2]]
        a <- data[[a_name]]
        b <- data[[b_name]]
        
        # 生成候选复合指标
        candidate <- ops[[op_name]](a, b)
        
        # 计算与目标指标的相关性评分 (采用绝对值捕获正负相关)
        score <- abs(stats::cor(candidate, data[[target_name]], use = "complete.obs"))
        
        if (!is.na(score) && score > 0.3) {
          results[[length(results) + 1]] <- data.frame(
            Formula = paste0(a_name, " [", op_name, "] ", b_name),
            Math_Logic = switch(op_name,
                               ratio = paste0(a_name, "/", b_name),
                               product = paste0(a_name, "*", b_name),
                               bmi_style = paste0(a_name, "/", b_name, "^2"),
                               log_ratio = paste0("log(", a_name, "/", b_name, ")"),
                               inter_sq = paste0("(", a_name, "-", b_name, ")^2")),
            Correlation_Score = round(score, 4),
            Auth = auth_id,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  final_res <- do.call(rbind, results)
  
  if (is.null(final_res)) {
    message("![L2078] 未发现具有显著相关性的高等数学组合。")
    return(NULL)
  }
  
  # 按照相关性得分从高到低排序
  return(final_res[order(-final_res$Correlation_Score), ])
}
