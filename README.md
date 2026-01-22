# AutochooseL 🧪
Nonlinear Biomarker "Complex Formula" Analysis and Generation Engine
---
# 🌟 Purpose
In cross-sectional studies, automatically arranging different metrics into high-order nonlinear mathematical combinations allows for the exploration of "optimal formulas" that possess superior predictive power compared to single variables.

# 🚀 Core Features
 * High-Order Discovery: Supports complex interaction modeling for 2 to 5 variables.
 * Nonlinear Operator Library: Features 12 built-in mathematical models simulating biological logic:
 * Sigmoid Response: Detects threshold and "switch" effects in disease onset.
 * Euclidean Distance: Measures the deviation of physiological indicators from a healthy equilibrium point (homeostasis).
 * Harmonic/Geometric Mean: Captures "bottleneck effects" and synergistic balance within biological systems.
---
# 📦 Installation
Ensure you have devtools installed, then run the following in R:
# Install the latest stable version
devtools::install_github("Lin2078/autochooseL", force = TRUE)

🛠️ Quick Start
1. Automated Formula Mining
Input your dataset and target variable to begin the "alchemy" process:
library(autochooseL)

# Discover high-order formula rankings
# target_name can be a continuous variable or a binary outcome (0/1)
results <- discover_advanced_indices(
  data = my_clinical_data, 
  target_name = "Outcome_A", 
  max_order = 5,    # Scans combinations from 2 to 5 metrics
  top_k = 12        # Pre-screens the top 12 correlated metrics for efficiency
)

# View the top 10 most predictive complex formulas
print(head(results, 10))

2. Visualization
Generate a ranking chart of the discovered formulas with a single command:
plot_L2078(results)
---
📊 Formula Models Reference
| Model Name | Mathematical Logic | Biological Interpretation |
|---|---|---|
| Sigmoid_Response | 1 / (1 + e^{-\sum Z}) | Trigger thresholds or saturation effects |
| Euclidean_Dist | \sqrt{\sum Z^2} | Total displacement from multidimensional homeostasis |
| Harmonic | n / \sum (1/x) | Rate-limiting steps or "Liebig's Law of the Minimum" |
| log10_prod | \log_{10}(\prod x) | Exponential risk amplification in cascade reactions |
📖 Citation
If you use this tool in your research, please cite it as follows:
> Lin. autochooseL: An R package for automated nonlinear formula discovery in complex biological systems. GitHub Repository.
> 
🤝 Contribution & Feedback
If you discover interesting mathematical models or encounter issues, please feel free to submit an Issue.
Author: Lin2078
Algorithm: L2078 Ultra Engine
