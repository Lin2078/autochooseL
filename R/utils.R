#' 打印 L2078 专用报告
#' @export
print.auto_L_res <- function(x, ...) {
  cat("\n==========================================\n")
  cat("   autochooseL Analysis Report [", attr(x, "signature"), "]\n")
  cat("==========================================\n")
  if(nrow(x) == 0) {
    cat("未发现显著的复杂标签关系。\n")
  } else {
    print(head(as.data.frame(x), 15))
    cat("\n运算总耗时:", round(attr(x, "runtime"), 2), "秒\n")
  }
}

#' 可视化关系图谱
#' @export
plot_L2078 <- function(res_obj) {
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("需要安装 ggplot2")
  library(ggplot2)
  
  ggplot(res_obj, aes(x = Support, y = Lift, size = Lift, color = as.factor(Order_L))) +
    geom_point(alpha = 0.6) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Complex Relationship Landscape (Lin2078)",
         subtitle = "X: Frequency | Y: Strength | Color: Complexity Order",
         x = "Support (Occurrence Rate)", y = "Lift (Association Strength)",
         color = "L Order") +
    theme_minimal()
}
