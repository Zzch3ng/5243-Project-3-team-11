# Project 3 – A/B Test: UI Layout and User Engagement in a Shiny Data Wrangling App

## Team 11
- Yuhan Guo (yg2695)
- Jason Zhao (zz3390)
- Sebastian Hoornweg (smh2289)
- Omari Motta (osm2116)

## Overview

This repository contains the Shiny applications and statistical analysis code for our A/B test comparing two UI layouts for *Data Wrangling Studio*, a six-tab interactive data wrangling application.

**Research question:** Does placing the primary data-loading controls at the **top** of the interface (Version B) versus in a **left sidebar** (Version A) lead to higher user engagement, measured by sections visited per session and session engagement time?

**Finding:** No statistically significant difference was detected for any primary or secondary metric at α = 0.05. The top-panel layout (B) showed a numerically higher bounce rate (20.0 % vs 5.7 %), though this was not significant given the sample size.

---

## Repository Structure

```
.
├── 5243A.R          # Shiny app — Version A (control: left-panel upload)
├── 5243B.R          # Shiny app — Version B (treatment: top-panel upload)
├── analysis.py      # Statistical analysis: descriptive stats, hypothesis tests, figures
├── figures/         # Generated plots (created by analysis.py)
├── results.json     # Machine-readable results (created by analysis.py)
└── README.md
```



---

## How to Run the Analysis

### Requirements

Python 3.8+ with the following packages:

```
pandas
scipy
numpy
matplotlib
seaborn
```

Install with:

```bash
pip install pandas scipy numpy matplotlib seaborn
```

### Run

From the repo root:

```bash
python analysis.py
```

This will:
1. Load `../ab_test_data_wide.csv`
2. Print descriptive statistics, normality tests, and hypothesis test results to stdout
3. Save four figures to `figures/`
4. Save a summary to `results.json`

---

## Shiny Apps

Both apps require R with the following packages: `shiny`, `bslib`, `plotly`, `DT`, `readxl`, `jsonlite`.

```r
# Run Version A (control)
shiny::runApp("5243A.R")

# Run Version B (treatment)
shiny::runApp("5243B.R")
```

Live deployments (tracking window March 18 – April 14, 2026):
- Version A: https://5243project.shinyapps.io/new_project/
- Version B: https://5243project.shinyapps.io/new_project_b/

---

## Key Results Summary

| Metric | Version A | Version B | Test | p-value | Effect (r) |
|--------|-----------|-----------|------|---------|------------|
| Sections visited (mean ± SD) | 2.77 ± 1.50 | 2.66 ± 1.81 | Mann-Whitney U | 0.872 | −0.023 |
| Engagement time, s (median) | 209.6 | 136.5 | Mann-Whitney U | 0.336 | −0.135 |
| Bounce rate | 5.7 % | 20.0 % | Fisher's exact | 0.151 | OR = 4.13 |
| Export rate | 5.7 % | 2.9 % | Fisher's exact | 1.000 | OR = 0.49 |
| Workflow completion | 5.7 % | 2.9 % | Fisher's exact | 1.000 | OR = 0.49 |

n = 35 per group. α = 0.05. All tests two-sided.
