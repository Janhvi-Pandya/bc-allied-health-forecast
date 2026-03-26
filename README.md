# BC Allied Health Workforce Forecasting Analysis

An R-based analysis of allied health workforce trends across British Columbia's five Health Authorities, featuring regression forecasting, vacancy gap analysis, and policy recommendations.

## Overview

This project examines 15 years (2010–2024) of workforce data for eight allied health professions across BC's regional Health Authorities. It answers three key questions:

1. **Where are the gaps?** — Which professions and regions face the most severe workforce shortages?
2. **Where are we headed?** — What do linear forecasts suggest about workforce supply through 2028?
3. **Did COVID make it worse?** — Is there statistical evidence that the pandemic elevated vacancy rates?

## Key Findings

- All professions show positive growth, but **Medical Lab Technologists** are growing the slowest
- **Northern Health** has per-capita supply rates 30–50% below the provincial average across nearly all professions
- **Speech-Language Pathologists** and **Dietitians** have the highest vacancy rates province-wide
- Welch's t-tests confirm statistically significant post-COVID increases in vacancy rates for several professions
- Forecasts project continued growth through 2028, but existing gaps will not close without policy intervention

## Project Structure

```
bc-allied-health-forecast/
├── 00_generate_data.R                  # Synthetic data generation (reproducible)
├── 01_analysis.R                       # Full analysis pipeline
├── data/
│   └── bc_allied_health_workforce.csv  # Dataset (~600 rows)
├── output/
│   ├── 01_provincial_trends.png        # Headcount trends by profession
│   ├── 02_growth_comparison.png        # 2010 vs 2024 growth rates
│   ├── 03_supply_by_ha.png             # Per-capita supply by Health Authority
│   ├── 04_vacancy_heatmap.png          # Vacancy rate heatmap
│   ├── 05_unfilled_positions.png       # Estimated unfilled positions
│   ├── 06_forecast_panels.png          # Regression forecasts with prediction intervals
│   ├── 07_vacancy_comparison.png       # Pre vs post-COVID vacancy rates
│   ├── forecast_summary.csv            # Forecast table (2025–2028)
│   └── t_test_results.csv             # Statistical test results
├── report.md                           # Written analysis report
└── README.md
```

## Methodology

| Step | Method | Details |
|------|--------|---------|
| Data quality | Missing value audit | ~3% missing; no systematic patterns |
| Imputation | Group-median | Median by profession × Health Authority; robust to outliers |
| EDA | 5 visualization types | Trends, growth bars, supply comparison, heatmap, gap chart |
| Forecasting | OLS linear regression | Per-profession models; COVID years excluded; 95% prediction intervals |
| Statistical testing | Welch's t-test | One-sided; pre-COVID (2015–2019) vs post-COVID (2022–2024) |

## How to Run

### Prerequisites

R 4.0+ with the following packages:

```r
install.packages(c("tidyverse", "broom", "scales"))
```

### Execution

```bash
cd bc-allied-health-forecast

# Step 1: Generate the synthetic dataset
Rscript 00_generate_data.R

# Step 2: Run the full analysis pipeline
Rscript 01_analysis.R
```

The analysis script will produce all 7 plots and 2 CSV summary tables in the `output/` directory, plus model diagnostics printed to the console.

## Data

The dataset is synthetic but modeled after publicly available workforce statistics from:

- [Canadian Institute for Health Information (CIHI)](https://www.cihi.ca/) — Health Workforce Database
- [BC Ministry of Health](https://www2.gov.bc.ca/gov/content/health) — workforce planning reports

**Professions covered:** Physiotherapist, Occupational Therapist, Speech-Language Pathologist, Pharmacist, Medical Lab Technologist, Respiratory Therapist, Dietitian, Social Worker

**Health Authorities:** Fraser, Vancouver Coastal, Island, Interior, Northern

## Technical Highlights

- **Reproducible pipeline:** `set.seed(42)` in data generation ensures identical results across runs
- **COVID-aware modeling:** 2020–2021 excluded from regression to prevent trend distortion; impact quantified separately via t-tests
- **Publication-quality plots:** All visualizations use consistent colour palettes, proper labelling, and 300 DPI output
- **Data quality workflow:** Demonstrates assessment → imputation → validation, a standard pattern in health data analytics

## Author

Keriu Pandya

## License

This project is for portfolio and educational purposes.
