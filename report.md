# BC Allied Health Workforce Forecasting Analysis

**Author:** Keriu Pandya
**Date:** March 2026
**Tools:** R (tidyverse, broom, scales)

---

## 1. Introduction

British Columbia faces persistent challenges in health workforce planning. As the province's population ages and grows — particularly in high-growth regions like Fraser Health — demand for allied health services continues to outpace supply. The COVID-19 pandemic further strained an already stretched workforce, accelerating burnout and early retirements while disrupting training pipelines.

Allied health professionals — physiotherapists, occupational therapists, pharmacists, speech-language pathologists, medical laboratory technologists, respiratory therapists, dietitians, and social workers — are essential to the continuum of care across acute, primary, and community settings. Unlike physicians and nurses, these professions often receive less attention in workforce planning despite filling critical gaps in rehabilitation, diagnostics, mental health, and chronic disease management.

This analysis examines workforce trends across BC's five regional Health Authorities from 2010 to 2024, with the aim of:

1. Identifying growth patterns and regional disparities in allied health supply
2. Quantifying vacancy gaps that signal workforce shortages
3. Producing evidence-based forecasts through 2028 to inform planning
4. Assessing whether the pandemic had a statistically significant impact on vacancy rates

## 2. Data and Methods

### 2.1 Dataset

The dataset comprises approximately 600 observations spanning 8 allied health professions, 5 Health Authorities, and 15 years (2010–2024). Variables include annual headcounts, per-capita supply rates (per 100,000 population), and vacancy rates. The data is synthetic but modeled after publicly available statistics from the Canadian Institute for Health Information (CIHI) and BC Ministry of Health workforce reports, preserving realistic patterns including:

- Higher per-capita supply in urban Health Authorities (Fraser, Vancouver Coastal)
- Chronic recruitment challenges in Northern Health
- COVID-era disruptions in 2020–2021

### 2.2 Data Quality Assessment

An initial quality audit identified missing values in approximately 3% of records across headcount, supply rate, and vacancy rate fields. Missing values were distributed across profession–Health Authority groups without systematic patterns, suggesting data-collection gaps rather than structural bias.

### 2.3 Imputation Strategy

Missing values were imputed using the **group-median** method, calculating the median within each profession × Health Authority group. This approach was chosen over alternatives for the following reasons:

- **vs. mean imputation:** The median is robust to outliers (e.g., a single year with anomalous recruitment) and does not inflate or deflate group estimates.
- **vs. global median:** A province-wide median would ignore the substantial variation between Health Authorities — for example, Northern Health consistently has lower headcounts than Fraser Health. Group-level imputation preserves these real differences.

Post-imputation validation confirmed zero remaining missing values.

### 2.4 Analytical Methods

**Exploratory analysis** included trend visualization, growth rate calculations, per-capita supply comparisons, and vacancy gap estimation. Unfilled positions were derived from the relationship: `estimated_demand = headcount / (1 - vacancy_rate)`.

**Regression modeling** used ordinary least squares (OLS) linear regression with the specification `total_headcount ~ year`, fitted separately per profession on provincial aggregates. COVID years (2020–2021) were excluded from model fitting to prevent pandemic-induced dips from distorting the underlying growth trend. Forecasts for 2025–2028 were generated with 95% prediction intervals.

**Statistical testing** used Welch's t-test (one-sided, α = 0.05) to compare mean vacancy rates in the pre-COVID period (2015–2019) against the post-COVID period (2022–2024), testing the alternative hypothesis that post-COVID rates are higher.

## 3. Findings

### 3.1 Provincial Growth Trends

All eight professions showed positive growth over the 2010–2024 period, though growth rates varied substantially. Social Workers and Occupational Therapists exhibited the strongest growth trajectories, consistent with expanded mental health and rehabilitation mandates. Medical Laboratory Technologists showed the slowest growth, reflecting well-documented national challenges in attracting entrants to the profession.

The COVID dip is visible in 2020–2021 headcount data across most professions, with recovery beginning in 2022 but not fully returning to the pre-pandemic trendline in all cases.

### 3.2 Regional Disparities

Per-capita supply analysis reveals significant regional inequities. Vancouver Coastal and Fraser Health generally have the highest supply rates, benefiting from proximity to training institutions, urban amenities, and larger employer organizations. Northern Health consistently shows the lowest per-capita supply across nearly all professions — often 30–50% below the provincial average — reflecting long-standing rural and remote recruitment challenges.

Island and Interior Health fall in between, with Interior Health showing particular softness in professions that require specialized training facilities.

### 3.3 Vacancy Analysis

The 2024 vacancy rate heatmap identifies several critical shortage areas:

- **Speech-Language Pathologists** and **Dietitians** have the highest vacancy rates province-wide, reflecting small professional pools and high demand.
- **Northern Health** shows elevated vacancy rates across all professions, with some exceeding 20%.
- **Pharmacists** have relatively low vacancy rates, likely due to larger training cohorts and private-sector workforce supply.

Estimated unfilled positions — derived from headcount and vacancy rates — highlight where the absolute gap is largest. Even professions with moderate vacancy rates (e.g., Social Workers) can have large absolute gaps due to large workforce sizes.

### 3.4 Forecast Results

Linear regression models achieved strong fit across all professions (R² > 0.90 in most cases), which is expected given the steady underlying growth trends. Key forecast findings:

- **Pharmacists** are projected to have the largest absolute workforce by 2028, continuing their position as the most numerous allied health profession in BC.
- **Social Workers** and **Occupational Therapists** show the steepest growth trajectories, suggesting increasing policy emphasis on mental health and rehabilitation.
- **Medical Lab Technologists** have the flattest forecast, with prediction intervals suggesting the profession could plateau if current trends continue.
- 95% prediction intervals widen for professions with higher year-to-year volatility, appropriately reflecting greater uncertainty.

Note: These forecasts assume continuation of recent trends and do not account for policy interventions, training program expansions, or immigration changes that could alter trajectories.

### 3.5 COVID Impact on Vacancy Rates

Welch's t-tests found statistically significant increases in post-COVID vacancy rates for several professions. The magnitude of the effect varies: professions with already-high vacancy rates (SLPs, Dietitians) showed larger absolute increases, while those with stronger recruitment pipelines (Pharmacists) showed smaller or non-significant changes.

This finding aligns with broader workforce research indicating that the pandemic had differential impacts across health professions, with smaller and more specialized disciplines experiencing longer recovery periods.

## 4. Limitations

1. **Synthetic data:** While modeled after real-world patterns, this dataset does not capture the full complexity of actual workforce dynamics. Conclusions should be interpreted as methodological demonstrations rather than policy prescriptions.

2. **Linear model assumptions:** Linear regression assumes a constant rate of change, which may not hold over longer horizons. Workforce supply is influenced by non-linear factors including retirement waves, immigration policy shifts, and training program capacity changes.

3. **Aggregation limitations:** Provincial-level regression masks sub-regional variation. A more granular model could fit trends per Health Authority, capturing divergent trajectories (e.g., Northern Health's stagnation vs. Fraser's rapid growth).

4. **No demand-side modeling:** This analysis models supply only. A complete workforce planning model would incorporate population health needs, service utilization projections, and scope-of-practice changes.

5. **COVID exclusion trade-off:** Excluding 2020–2021 from the regression prevents trend distortion but also discards information about pandemic-era structural changes that may persist.

## 5. Recommendations

Based on the analysis findings, the following actions are recommended for BC health workforce planners:

### 5.1 Targeted Recruitment for High-Vacancy Professions

Speech-Language Pathologists and Dietitians consistently show the highest vacancy rates. Targeted strategies could include: expanded training program seats at BC universities, streamlined credential recognition for internationally trained professionals, and return-of-service agreements for new graduates.

### 5.2 Northern Health Workforce Strategy

Northern Health's chronic supply deficit requires a dedicated approach beyond general provincial recruitment. Evidence from other jurisdictions suggests that distributed training models (clinical placements in rural communities), loan forgiveness programs, and community-embedded practice models are more effective than relocation incentives alone.

### 5.3 Post-COVID Vacancy Monitoring

The statistical evidence of elevated post-COVID vacancy rates warrants ongoing monitoring. The Ministry should establish a quarterly vacancy tracking mechanism — rather than relying on annual or biennial surveys — to enable earlier intervention when vacancy rates exceed critical thresholds.

### 5.4 Model Refinement with Administrative Data

This analysis demonstrates the utility of regression-based forecasting. The logical next step is to apply this framework to administrative workforce data (e.g., College of Health Professionals registration data, WorkBC labour market information) and extend the model to include:

- Retirement projections based on age distribution data
- Training program enrollment and graduation rates
- Net interprovincial and international migration of health professionals

## 6. Conclusion

BC's allied health workforce is growing but unevenly — both across professions and across regions. The data point to a province that is producing more allied health professionals each year but is not distributing them equitably, particularly to Northern and rural communities. The COVID-19 pandemic created a measurable disruption to vacancy rates that has not fully resolved.

Linear regression forecasts suggest continued growth through 2028, but growth alone will not close existing gaps without deliberate policy action. The combination of targeted recruitment, regional equity strategies, and enhanced data infrastructure will be essential to meeting BC's health workforce needs over the coming decade.

---

*Analysis conducted using R 4.3+ with tidyverse, broom, and scales packages. Full source code and reproducibility instructions available in the project repository.*
