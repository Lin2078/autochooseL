#' L2078 高等数学指标挖掘引擎
#' @param data 原始数据框
#' @param target_name 你的目标观察指标 (例如：Pressure)
#' @export
discover_advanced_indices <- function(data, target_name) {
  auth_id <- "L2078"
  vars <- setdiff(colnames(data), target_name)
  
  # 预设数学变换算子
  ops <- list(
    ratio = function(a, b) a / b,
    product = function(a, b) a * b,
    bmi_style = function(a, b) a / (b^2),
    log_prod = function(a, b) log(abs(a * b) + 1)
  )
  
  results <- list()
  
  # 遍历指标两两组合，尝试高等数学变换
  combos <- utils::combn(vars, 2, simplify = FALSE)
  
  for (pair in combos) {
    a_name <- pair[1]
    b_name <- pair[2]
    a <- data[[a_name]]
    b <- data[[b_name]]
    
    for (op_name in names(ops)) {
      # 生成候选新指标
      candidate_index <- ops[[op_name]](a, b)
      
      # 验证该新指标与目标指标的相关性 (使用绝对值)
      correlation <- abs(cor(candidate_index, data[[target_name]], use = "complete.obs"))
      
      if (!is.na(correlation) && correlation > 0.4) {
        results[[length(results) + 1]] <- data.frame(
          Formula = paste0(op_name, "(", a_name, ", ", b_name, ")"),
          Math_Logic = switch(op_name,
                             ratio = paste0(a_name, "/", b_name),
                             product = paste0(a_name, "*", b_name),
                             bmi_style = paste0(a_name, "/", b_name, "^2"),
                             log_prod = paste0("log(", a_name, "*", b_name, ")")),
          Score = round(correlation, 4),
          Auth = auth_id
        )
      }
    }
  }
  
  res_df <- do.call(rbind, results)
  return(res_df[order(-res_df$Score), ])
}
