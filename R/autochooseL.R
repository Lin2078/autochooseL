#' L2078 High-Order Formula Discovery & Ultra Risk Modeling Engine
#' @author Lin2078
#' @export

# --- 引擎 1：共现挖掘 (保持逻辑一致性) ---
#' @export
auto_choose_L <- function(data, max_L = 3, top_n = 5) {
  auth_id <- "L2078"
  if(is.character(data)) data <- utils::read.csv(data)
  numeric_data <- data[, sapply(data, is.numeric)]
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
        return(data.frame(Composition = paste(tags, collapse = " * "),
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

# --- 引擎 2：L2078 Ultra 高阶公式挖掘 (10种数学模型) ---
#' @export
discover_advanced_indices <- function(data, target_name, max_order = 3, top_k = 15, cor_threshold = 0.2) {
  auth_id <- "L2078"
  data <- data[stats::complete.cases(data), ]
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  vars <- setdiff(numeric_cols, target_name)
  
  if (length(vars) > top_k) {
    base_cor <- sapply(vars, function(v) abs(stats::cor(data[[v]], data[[target_name]], use = "complete.obs")))
    vars <- names(sort(base_cor, decreasing = TRUE)[1:top_k])
  }
  
  results_list <- list()
  for (L in 2:max_order) {
    combos <- utils::combn(vars, L, simplify = FALSE)
    if (length(combos) > 2000) combos <- combos[sample(seq_along(combos), 2000)]
    
    step_res <- lapply(combos, function(tags) {
      sub_data <- data[, tags, drop=FALSE]
      # 准备变量引用
      A <- sub_data[, 1]; 
      B <- if(L > 1) sub_data[, 2] else A; 
      C <- if(L > 2) sub_data[, 3] else B
      
      # --- L2078 Ultra: 10 种核心关系模型定义 ---
      models <- list(
        # 1. 幂律缩放 (如 BMI)
        list(val = (A * B) / (C^2 + 1e-6), str = paste0("(", tags[1], "*", B_name <- if(L>1) tags[2] else "", ")/", C_name <- if(L>2) tags[3] else "1", "^2")),
        # 2. 指数协同 (捕捉风险爆发)
        list(val = exp(rowMeans(scale(sub_data))), str = paste0("exp(mean_std(", paste(tags, collapse=","), "))")),
        # 3. 调和平均 (短板效应)
        list(val = L / rowSums(1 / (sub_data + 1e-6)), str = paste0("Harmonic(", paste(tags, collapse=","), ")")),
        # 4. 几何平均 (均衡贡献)
        list(val = apply(sub_data, 1, function(x) prod(abs(x) + 1e-6)^(1/L)), str = paste0("Geometric_Mean(", paste(tags, collapse=","), ")")),
        # 5. 变异系数 (指标波动性风险)
        list(val = apply(sub_data, 1, stats::sd) / (rowMeans(sub_data) + 1e-6), str = paste0("CV_Inconsistency(", paste(tags, collapse=","), ")")),
        # 6. 逻辑回归型 (阈值响应)
        list(val = 1 / (1 + exp(-rowMeans(scale(sub_data)))), str = paste0("Sigmoid_Response(", paste(tags, collapse=","), ")")),
        # 7. 对数乘积 (压缩量级)
        list(val = log(apply(sub_data, 1, function(x) prod(abs(x)) + 1)), str = paste0("log(prod(", paste(tags, collapse="*"), "))")),
        # 8. 交互失衡 (剪刀差)
        list(val = A / (B + 1e-6), str = paste0(tags[1], " / ", if(L>1) tags[2] else "1")),
        # 9. 欧几里得距离 (偏离健康原点)
        list(val = sqrt(rowSums(scale(sub_data)^2)), str = paste0("Euclidean_Dist(", paste(tags, collapse=","), ")")),
        # 10. 交互乘积 (经典交互)
        list(val = apply(sub_data, 1, prod), str = paste(tags, collapse=" * "))
      )
      
      inner_res <- lapply(models, function(m) {
        if (any(is.infinite(m$val)) || any(is.na(m$val))) return(NULL)
        score <- stats::cor(m$val, data[[target_name]])
        if (!is.na(score) && abs(score) >= cor_threshold) {
          return(data.frame(Formula = m$str, Abs_Correlation = round(abs(score), 4),
                            Order_L = L, Risk_Type = ifelse(score > 0, "Positive", "Negative"),
                            Auth = auth_id, stringsAsFactors = FALSE))
        }
        return(NULL)
      })
      do.call(rbind, inner_res)
    })
    results_list[[L]] <- do.call(rbind, step_res)
  }
  
  final_df <- do.call(rbind, results_list)
  if (is.null(final_df)) return(NULL)
  final_df <- final_df[!duplicated(final_df$Formula), ]
  return(final_df[order(-final_df$Abs_Correlation), ])
}

# --- 引擎 3：自适应绘图系统 ---
#' @export
plot_L2078 <- function(res_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2")
  library(ggplot2)
  if (is.null(res_obj) || nrow(res_obj) == 0) return(NULL)
  res_df <- as.data.frame(res_obj)

  if ("Abs_Correlation" %in% colnames(res_df)) {
    p <- ggplot(utils::head(res_df, 12), 
                aes(x = stats::reorder(Formula, Abs_Correlation), 
                    y = Abs_Correlation, 
                    fill = as.factor(Order_L))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "L2078 Ultra: Mathematical Risk Discovery",
           subtitle = "Top Non-linear Composite Formulas",
           x = "Discovered Formula", y = "Abs Correlation Score", fill = "Order (L)") +
      theme_minimal()
    return(p)
  } else if ("Strength" %in% colnames(res_df)) {
    p <- ggplot(res_df, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
      geom_point(aes(size = Strength), alpha = 0.7) + 
      labs(title = "L2078: Synergy Strength Analysis", x = "Prevalence", y = "Strength (Lift)") +
      theme_minimal()
    return(p)
  }
}
