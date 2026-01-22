# 非线性生物标志物“复杂公式”分析、生成引擎🧪

---

## 🌟 设计目的

在横断面研究中，将不同指标自动化排列为高阶非线性数学组合并进行分析，或许可以探索出具备临床预测力的“最适公式”。

---

## 🚀 核心特性

* **🧬 High-Order Discovery**：支持 2～5 个变量的高阶复杂交互建模，突破人工筛选极限。
* **🧮 算子库 (Operators)**：内置 12 种模拟生物逻辑的数学模型：
    * **Sigmoid Response**：捕捉疾病爆发的阈值开关效应。
    * **Euclidean Distance**：量化生理指标对健康平衡点（Homeostasis）的偏离。
    * **Harmonic/Geometric Mean**：捕捉系统中的“短板效应”与协同平衡。

---

## 📦 安装指南

确保你已经安装了 `devtools`，然后在 R 中运行：

```r
# 安装最新稳定版 (v2.2.0)
devtools::install_github("Lin2078/autochooseL", force = TRUE)

```

---

## 🛠️ 快速上手

### 1. 自动化公式挖掘

只需输入数据集和你的目标变量（结局），开始“炼金”：

```r

# 发现高阶公式排行榜
# target_name 可以是连续变量，也可以是 0/1 结局
results <- discover_advanced_indices(
  data = my_clinical_data, 
  target_name = "Outcome_A", 
  max_order = 3
)

# 查看最具预测力的前 10 个公式
print(head(results, 10))

```

### 2. 科研绘图

一键生成公式排行榜图表：

```r
plot_L2078(results)

```

---

## 📖 引用 (Citation)

如果你在 SCI 论文中使用了本工具，请按以下方式引用：

> *Lin. autochooseL: An R package for automated nonlinear formula discovery in complex biological systems. v2.2.0. GitHub Repository.*

---

## 🤝 贡献与反馈

如果你发现了更有趣的数学模型，或者在使用中遇到问题，欢迎提交 Issue。
**Author:** Lin2078
**Algorithm:** L2078 Ultra Engine