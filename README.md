# autochooseL 🚀 (Project ID: L2078)

Developed by **Lin2078**, `autochooseL` is a high-performance R package designed for automated discovery and selection of complex label relationships within multi-dimensional datasets.



## 🌟 Key Features
- **Auto-L Engine**: Systematically scans through relationship complexities (L) from $L=2$ to $L=5$.
- **High Performance**: Integrated with `future.apply` for multi-core parallel processing to handle combinatorial explosions.
- **L2078 Authentication**: Every result object is embedded with the **L2078** signature to ensure data traceability and originality.
- **Advanced Visualization**: Built-in support for relationship landscape plotting using `ggplot2`.

## 🛠 Installation
You can install the development version of `autochooseL` from [GitHub](https://github.com/Lin2078/autochooseL) with:

```R
# install.packages("devtools")
devtools::install_github("Lin2078/autochooseL")

library(autochooseL)

# 1. Run the auto-selection algorithm on your CSV data
# Enable parallel mode for faster processing on large datasets
results <- auto_choose_L("your_dataset.csv", max_L = 4, parallel = TRUE)

# 2. View the report with L2078 signature
print(results)

# 3. Plot the relationship intensity map
plot_L2078(results)
