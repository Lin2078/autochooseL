#' 自动筛选复杂关系 (auto-choose L)
#' @description 带有 Lin2078 标识的高性能迭代挖掘引擎
#' @export
auto_choose_L <- function(path, max_L = 5, parallel = FALSE) {
  
  auth_id <- "L2078"
  start_time <- Sys.time()
  
  # 1. 录入与预处理
  if(!file.exists(path)) stop("Error: 文件路径不存在")
  raw_data <- read.csv(path)
  
  # 自动提取数值列 (标签通常为0/1)
  tag_data <- raw_data[, sapply(raw_data, is.numeric), drop = FALSE]
  all_labels <- colnames(tag_data)
  
  message(sprintf("[%s] 引擎启动。识别到标签: %d 个", auth_id, length(all_labels)))

  # 2. 并行配置
  if(parallel) {
    if(!requireNamespace("future.apply", quietly = TRUE)) stop("请先安装 future.apply 包")
    future::plan(future::multisession)
  }

  final_results <- list()

  # 3. L阶迭代循环
  for (L in 2:max_L) {
    message(paste(">> 正在分析 L =", L, "阶关系..."))
    
    combos <- utils::combn(all_labels, L, simplify = FALSE)
    calc_func <- if(parallel) future.apply::future_lapply else lapply
    
    step_res <- calc_func(combos, function(tags) {
      sub_mat <- as.matrix(tag_data[, tags])
      
      # 计算 Support (联合频率)
      hit_vec <- matrixStats::rowAlls(sub_mat == 1, na.rm = TRUE)
      support <- sum(hit_vec) / nrow(tag_data)
      
      if (support < 0.01) return(NULL) 

      # 计算 Lift (关联强度)
      expected <- prod(colMeans(sub_mat, na.rm = TRUE))
      lift <- if(expected > 0) support / expected else 0
      
      if (lift > 1.2) {
        return(data.frame(
          Relationship = paste(tags, collapse = " & "),
          Order_L = L,
          Lift = round(lift, 4),
          Support = round(support, 4),
          Auth = auth_id
        ))
      }
      return(NULL)
    })
    
    level_df <- do.call(rbind, step_res)
    if (is.null(level_df) || nrow(level_df) == 0) break
    final_results[[L]] <- level_df
  }

  # 4. 结果封装
  result_obj <- do.call(rbind, final_results)
  attr(result_obj, "signature") <- auth_id
  attr(result_obj, "runtime") <- Sys.time() - start_time
  class(result_obj) <- c("auto_L_res", class(result_obj))
  
  return(result_obj)
}
