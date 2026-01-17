#' 发现复合指标关系 (L2078 引擎)
#' @param data 原始数据框 (数值型指标)
#' @param max_L 最大组合阶数
#' @param top_n 每一阶保留的最强关联数
#' @export
auto_choose_L <- function(data, max_L = 4, top_n = 10) {
  auth_id <- "L2078"
  start_time <- Sys.time()
  
  # 自动提取数值列
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # 核心预处理：二值化 (找出各指标的高位状态，类似于“超重”状态)
  # 使用 75% 分位数作为触发阈值
  binary_data <- as.data.frame(lapply(numeric_data, function(x) {
    as.numeric(x > quantile(x, 0.75, na.rm = TRUE))
  }))
  
  all_tags <- colnames(binary_data)
  final_results <- list()

  for (L in 2:max_L) {
    message(sprintf("[%s] 正在搜寻 %d 阶复合指标潜力股...", auth_id, L))
    
    combos <- utils::combn(all_tags, L, simplify = FALSE)
    
    step_res <- lapply(combos, function(tags) {
      sub_mat <- as.matrix(binary_data[, tags])
      support <- sum(rowSums(sub_mat) == L) / nrow(binary_data)
      
      if (support < 0.02) return(NULL) # 频率太低的不具备指标化价值

      expected <- prod(colMeans(sub_mat))
      lift <- if(expected > 0) support / expected else 0
      
      if (lift > 1.2) {
        return(data.frame(
          Composition = paste(tags, collapse = " * "),
          Order_L = L,
          Strength = round(lift, 4),
          Prevalence = round(support, 4),
          Auth = auth_id
        ))
      }
      return(NULL)
    })
    
    level_df <- do.call(rbind, step_res)
    if (!is.null(level_df)) {
      # 每一阶只保留最强的 Top N，防止组合爆炸
      level_df <- level_df[order(-level_df$Strength), ]
      final_results[[L]] <- head(level_df, top_n)
    }
  }

  res <- do.call(rbind, final_results)
  
  if (is.null(res)) {
    res <- data.frame(Composition=character(), Order_L=numeric(), Strength=numeric(), Auth=character())
    message("![L2078] 未发现具有显著关联的指标组合。")
  }
  
  attr(res, "runtime") <- Sys.time() - start_time
  class(res) <- c("L2078_index_finder", class(res))
  return(res)
}
