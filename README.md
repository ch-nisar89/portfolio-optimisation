# 📈 Portfolio Optimization using Genetic Algorithm (GA) in R

This project demonstrates how to apply a **Genetic Algorithm (GA)** to optimize a portfolio of selected stocks from various sectors, using R programming. The aim is to **maximize return and minimize risk** by applying evolutionary optimization techniques to real-world financial data.

---

## 📁 Contents

- `Portfolio Optimization using GA.Rmd` – Complete R Markdown script including:
  - Asset selection
  - Data acquisition
  - Return & risk calculations
  - GA-based portfolio optimization
  - Visualization of optimal weights

---

## 🔧 Technologies Used

- **R** programming
- **R packages:**
  - `GA`
  - `quantmod`
  - `TTR`
  - `xts`, `zoo`
  - `PerformanceAnalytics`
  - `dplyr`, `reshape2`, `tidyr`
  - `ggplot2`

---

## 🚀 How to Run

1. Clone this repository.
2. Open `Portfolio Optimization using GA.Rmd` in RStudio.
3. Knit the file or run chunks step-by-step.

> 💡 Make sure you have an active internet connection to fetch stock data via `quantmod`.

---

## 📊 Features

- Selection of 10 diverse stocks from multiple sectors
- Daily returns and risk (standard deviation) calculation
- Fitness function design for GA: Maximize Sharpe Ratio
- Visualization of:
  - Stock returns over time
  - Risk-return scatter plot
  - Optimal weights of the portfolio
- Reproducible workflow with tidyverse-style data handling

---

## 🧠 Learning Objectives

- Understand how genetic algorithms work for numerical optimization
- Apply GA to portfolio management problems
- Learn real-world financial data processing in R

---

## 🧑‍💻 Author

**Nisar Ahmed**  

---
