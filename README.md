# Exploring Interactions Between Financial Indicators in S&P 1500 Firms

Replication materials for my undergraduate dissertation, University of St Andrews,
School of Mathematics and Statistics (BSc Mathematics, 2026).

**Author:** Christy Cargill
**Supervisor:** Dr Michail Papathomas

## Overview

This project evaluates a range of modelling approaches for predicting EBIT margin
across 1,500 US-listed firms in the S&P Composite 1500 index, using financial data
for FY2024. Methods compared include linear regression, gamma GLM, penalised
regression (ridge, lasso, elastic net), random forests, and a hybrid random
forest + ridge approach. Missing data are handled using Multiple Imputation by
Chained Equations (MICE), and a Monte Carlo simulation study examines coefficient
recovery under multicollinearity.

## Repository contents

```
├── data/
│   └── SPGlobal_1500.xlsx      # Raw dataset exported from S&P Capital IQ Pro
├── code/
│   ├── 01_cleaning.R           # Data cleaning and filtering
│   ├── 02_ida.R                # Initial data analysis and plots
│   ├── 03_mice.R               # Multiple imputation
│   ├── 04_linear_gamma.R       # Linear regression and gamma GLM
│   ├── 05_penalised.R          # Ridge, lasso, elastic net
│   ├── 06_random_forest.R      # Random forest model
│   ├── 07_hybrid.R             # Hybrid RF + ridge model
│   └── 08_simulation.R         # Monte Carlo simulation study
├── figures/                    # Figures used in the report
├── report/
│   └── dissertation.pdf        # Final submitted report
└── README.md
```

## Data

The dataset is a single export from the S&P Capital IQ Pro screener covering
the 1,500 constituents of the S&P Composite 1500 for fiscal year 2024.
Full extraction instructions, including the exact Capital IQ metric codes
for every variable, are provided in Appendix A of the report.

Access to S&P Capital IQ Pro is required to re-extract the data. The raw
export used in this analysis is included here for reproducibility.

## Requirements

R (≥ 4.2) with the following packages:

```
mice, lattice, glmnet, randomForest, stabs,
MASS, ggplot2, dplyr, readxl
```

Install with:

```r
install.packages(c("mice", "lattice", "glmnet", "randomForest",
                   "stabs", "MASS", "ggplot2", "dplyr", "readxl"))
```

## Reproducing the results

Run the scripts in numerical order from the `code/` directory. Each script
loads the output of the previous one, so the full pipeline can be reproduced
end-to-end from the raw Excel file. A fixed random seed is set at the top of
each script to ensure exact replication of MICE imputations, train/test
splits, and simulation results.

## Citation

If you refer to this work, please cite as:

> Cargill, C. (2025). *Exploring Interactions Between Financial Indicators in
> S&P 1500 Firms.* BSc dissertation, School of Mathematics and Statistics,
> University of St Andrews.

## Licence

Code is released under the MIT Licence. The dataset is redistributed here
solely for academic reproducibility; all underlying data remain the property
of S&P Global Market Intelligence and are subject to their terms of use.
