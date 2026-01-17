#' 打印发现报告
#' @export
print.L2078_index_finder <- function(x, ...) {
  cat("\n>>> [L2078] 复合指标发现报告 <<<\n")
  cat("==========================================\n")
  if(nrow(x) == 0) return(cat("无发现。\n"))
  
  formatted_df <- x[, c("Composition", "Order_L", "Strength")]
  colnames(formatted_df) <- c("潜力组合 (New Index)", "阶数", "关联强度(Lift)")
  print(formatted_df)
  cat("\n[建议]: 尝试对上述指标进行乘除运算以构建新指标。\n")
}

#' 可视化复合指标潜力图
#' @export
plot_L2078 <- function(res_obj) {
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("需要 ggplot2")
  library(ggplot2)
  
  ggplot(res_obj, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
    geom_point(aes(size = Strength), alpha = 0.7) +
    geom_text(aes(label = Composition), vjust = -1, size = 3, check_overlap = TRUE) +
    labs(title = "L2078: New Indicator Candidates",
         x = "Occurrence Rate (Prevalence)", y = "Discovery Strength (Lift)") +
    theme_minimal()
}
