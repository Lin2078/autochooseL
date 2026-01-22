# Auto-chooseL 🧪
Automated Discovery Engine for Non-linear Biomarker "Complex Formulas"
autochooseL is an R package specifically designed for cross-sectional clinical research. Powered by the L2078 Ultra Engine, it automates the scanning and generation of highly interpretable, high-order non-linear mathematical formulas to uncover hidden biological patterns and optimal risk scoring models.
🌟 Research Logic
In complex biological systems, single indicators often fail to capture the full spectrum of disease pathology. Traditional linear models frequently ignore synergistic effects between variables. The core logic of autochooseL is:
 * High-Order Combinatorics: Automatically permute all possible combinations of 2 to 5 indicators.
 * Mathematical Mapping: Apply non-linear operators (e.g., Sigmoid, Harmonic) to simulate biological feedback mechanisms.
 * Efficiency Competition: Rank formulas based on correlation and AUC to identify "Super Formulas" with the strongest clinical predictive power.
🚀 Core Features
 * High-Order Discovery: Supports cross-dimensional interaction modeling for 2 to 5 variables.
 * Non-linear Operator Library: Built-in 12 mathematical models simulating biological logic:
   * Sigmoid Response: Detects "all-or-none" switch effects or receptor saturation thresholds.
   * Euclidean Distance: Measures the total spatial displacement of physiological indicators from a healthy steady state (homeostasis).
   * Harmonic/Geometric Mean: Captures "bottleneck effects" or multi-pathway synergistic balances in biological systems.
   * CV Inconsistency: Evaluates the rhythmic synchronization and consistency among a group of indicators.
 * SCI Ready: Generates publication-quality formula rankings and statistical visualizations.
📦 Installation
Ensure you have installed devtools, then run the following in R:
# Install the latest stable version (v3.1.0)
devtools::install_github("Lin2078/autochooseL", force = TRUE)

🛠️ Quick Start
1. Automated Formula Mining (Order 2-5)
Set max_order = 5 to allow the engine to automatically find the optimal dimensionality:
library(autochooseL)

# Start global automated discovery
# target_name supports continuous variables or binary outcomes (0/1)
results <- discover_advanced_indices(
  data = my_clinical_data, 
  target_name = "Outcome_A", 
  max_order = 5,    # Scans combinations from order 2 to 5
  top_k = 12        # Pre-filters top 12 correlated metrics for efficiency
)

# View the top 10 most predictive complex formulas
print(head(results, 10))

2. Research-Grade Visualization
Generate formula ranking charts with one command:
# Plot the formula performance ranking
plot_L2078(results)

📊 Formula Model Reference
| Model Name | Mathematical Logic | Biological Interpretation |
|---|---|---|
| Sigmoid_Response | 1 / (1 + e^{-\sum Z}) | Trigger thresholds for disease onset / Saturation effects |
| Euclidean_Dist | \sqrt{\sum Z^2} | Total deviation from healthy homeostasis |
| Harmonic | n / \sum (1/x) | "Rate-limiting steps" or bottlenecks in metabolic pathways |
| log10_prod | \log_{10}(\prod x) | Exponential risk amplification in cascade reactions |
📖 Citation
If you use this tool in your SCI publication, please cite it as follows:
> Lin. autochooseL: An R package for automated nonlinear formula discovery in complex biological systems. v3.1.0. GitHub Repository, 2026.
> 
🤝 Contribution & Feedback
If you discover more interesting mathematical models or encounter issues, feel free to submit an Issue.
Author: Lin2078
Algorithm: L2078 Ultra Engine
Would you like me to also provide a full English documentation for the specific functions (like a manual) or help you write the English version of the R session log for a publication?
