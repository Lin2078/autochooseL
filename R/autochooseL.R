#' L2078 High-Order Formula Discovery & Risk Modeling Engine
#' @author Lin2078
#' @export

# --- Engine 1: Co-occurrence Discovery (Association Rules) ---
#' @export
auto_choose_L <- function(data, max_L = 3, top_n = 5) {
  auth_id <- "L2078"
  if(is.character(data)) data <- utils::read.csv(data)
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # Binarization (Capturing extreme high-risk states)
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

# --- Engine 2: High-Order Formula Mining (2-5 Orders) ---
#' @export
discover_advanced_indices <- function(data, target_name, max_order = 3, top_k = 15, cor_threshold = 0.2) {
  auth_id <- "L2078"
  data <- data[stats::complete.cases(data), ]
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  vars <- setdiff(numeric_cols, target_name)
  
  if (length(vars) > top_k) {
    message(sprintf("[%s] Screening top %d variables from %d candidates...", auth_id, top_k, length(vars)))
    base_cor <- sapply(vars, function(v) abs(stats::cor(data[[v]], data[[target_name]], use = "complete.obs")))
    vars <- names(sort(base_cor, decreasing = TRUE)[1:top_k])
  }
  
  results_list <- list()
  for (L in 2:max_order) {
    message(sprintf("[%s] Scanning Order %d formulas...", auth_id, L))
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
                          Order_L = L, 
                          Risk_Type = ifelse(score > 0, "Positive", "Negative"),
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

# --- Engine 3: Adaptive Visualization (Fixing Logic & Font Issues) ---
#' @export
plot_L2078 <- function(res_obj) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2")
  library(ggplot2)
  if (is.null(res_obj) || nrow(res_obj) == 0) {
    message("No results found to plot.")
    return(NULL)
  }
  
  res_df <- as.data.frame(res_obj)

  # Logic Branch A: Formula Mining Results (Using Abs_Correlation)
  if ("Abs_Correlation" %in% colnames(res_df)) {
    p <- ggplot(utils::head(res_df, 12), 
                aes(x = stats::reorder(Formula, Abs_Correlation), 
                    y = Abs_Correlation, 
                    fill = as.factor(Order_L))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "L2078: Composite Index Risk Ranking",
           subtitle = "Correlation between Formula and Target Variable",
           x = "Mathematical Formula", 
           y = "Absolute Correlation Score", 
           fill = "Order (L)") +
      theme_minimal()
    return(p)
      
  } 
  # Logic Branch B: Indicator Co-occurrence Results (Using Strength/Prevalence)
  else if ("Strength" %in% colnames(res_df)) {
    p <- ggplot(res_df, aes(x = Prevalence, y = Strength, color = as.factor(Order_L))) +
      geom_point(aes(size = Strength), alpha = 0.7) +
      geom_text(aes(label = Composition), vjust = -1, size = 3, check_overlap = TRUE) +
      labs(title = "L2078: Synergy Strength Analysis",
           x = "Prevalence (Occurrence)", 
           y = "Strength (Lift)", 
           color = "Order") +
      theme_minimal()
    return(p)
  } else {
    message("Error: Data structure not recognized by L2078 plotting system.")
  }
}
