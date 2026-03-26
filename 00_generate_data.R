# =============================================================================
# 00_generate_data.R
# BC Allied Health Workforce — Synthetic Data Generation
# =============================================================================
# Generates a reproducible synthetic dataset modeled after publicly available
# workforce statistics from CIHI and the BC Ministry of Health.
#
# Output: data/bc_allied_health_workforce.csv
# Rows:   ~600 (8 professions x 5 Health Authorities x 15 years)
# =============================================================================

library(tidyverse)
set.seed(42)

# --- Parameters ---------------------------------------------------------------

years <- 2010:2024

professions <- c(
  "Physiotherapist", "Occupational Therapist", "Speech-Language Pathologist",
  "Pharmacist", "Medical Lab Technologist", "Respiratory Therapist",
  "Dietitian", "Social Worker"
)

health_authorities <- c(
  "Fraser", "Vancouver Coastal", "Island", "Interior", "Northern"
)

# Approximate 2024 HA populations (used for per-capita calculations)
ha_populations <- tibble(
  health_authority = health_authorities,
  population_2024  = c(1950000, 1250000, 850000, 780000, 290000),
  pop_growth_rate  = c(0.020, 0.015, 0.010, 0.008, 0.002)
)

# Base headcounts per profession (provincial total, circa 2010)
# These are rough approximations informed by CIHI Health Workforce reports
base_headcounts <- tibble(
  profession = professions,
  provincial_base = c(3200, 2100, 900, 5500, 3800, 1100, 800, 4200),
  annual_growth   = c(0.035, 0.040, 0.030, 0.025, 0.015, 0.028, 0.032, 0.038),
  vacancy_base    = c(0.08, 0.10, 0.12, 0.06, 0.09, 0.11, 0.13, 0.07)
)

# HA share of provincial workforce (roughly tracks population share)
ha_shares <- tibble(
  health_authority = health_authorities,
  share = c(0.35, 0.28, 0.16, 0.14, 0.07)
)

# --- Generate grid ------------------------------------------------------------

grid <- expand_grid(
  year             = years,
  profession       = professions,
  health_authority = health_authorities
)

# --- Build headcounts ---------------------------------------------------------

df <- grid %>%
  left_join(base_headcounts, by = "profession") %>%
  left_join(ha_shares, by = "health_authority") %>%
  left_join(ha_populations, by = "health_authority") %>%
  mutate(
    # Years since baseline
    t = year - 2010,

    # Population for this year (backtrack from 2024)
    population = population_2024 / (1 + pop_growth_rate)^(2024 - year),

    # Expected headcount: base * HA share * compounding growth + noise
    expected_headcount = provincial_base * share * (1 + annual_growth)^t,

    # COVID dip: reduce headcount growth in 2020-2021
    covid_factor = case_when(
      year == 2020 ~ runif(n(), 0.94, 0.97),
      year == 2021 ~ runif(n(), 0.96, 0.99),
      TRUE         ~ 1.0
    ),

    # Northern HA gets a recruitment penalty (harder to attract professionals)
    northern_penalty = if_else(health_authority == "Northern", 0.92, 1.0),

    headcount = round(
      expected_headcount * covid_factor * northern_penalty +
        rnorm(n(), 0, expected_headcount * 0.03)
    ),
    headcount = pmax(headcount, 5),

    # Per-capita supply rate (per 100,000 population)
    supply_per_100k = round(headcount / population * 100000, 1),

    # Vacancy rate: base + time trend + COVID spike + HA variation + noise
    vacancy_rate = vacancy_base +
      t * (-0.002) +
      case_when(
        year == 2020 ~ runif(n(), 0.04, 0.08),
        year == 2021 ~ runif(n(), 0.03, 0.06),
        year == 2022 ~ runif(n(), 0.01, 0.03),
        TRUE         ~ 0
      ) +
      if_else(health_authority == "Northern", 0.05, 0) +
      rnorm(n(), 0, 0.015),

    vacancy_rate = round(pmin(pmax(vacancy_rate, 0.01), 0.30), 3)
  ) %>%
  select(year, profession, health_authority, population, headcount,
         supply_per_100k, vacancy_rate)

# --- Introduce intentional missing values (~3%) ------------------------------
# This allows the analysis script to demonstrate data quality handling.

n_missing <- round(nrow(df) * 0.03)

set.seed(99)
missing_headcount <- sample(seq_len(nrow(df)), n_missing %/% 3)
missing_supply    <- sample(seq_len(nrow(df)), n_missing %/% 3)
missing_vacancy   <- sample(seq_len(nrow(df)), n_missing %/% 3)

df$headcount[missing_headcount]        <- NA
df$supply_per_100k[missing_supply]     <- NA
df$vacancy_rate[missing_vacancy]       <- NA

# --- Write output -------------------------------------------------------------

write_csv(df, "data/bc_allied_health_workforce.csv")

cat("Data generation complete.\n")
cat(sprintf("  Rows: %d\n", nrow(df)))
cat(sprintf("  Missing headcount:  %d (%.1f%%)\n",
            sum(is.na(df$headcount)), mean(is.na(df$headcount)) * 100))
cat(sprintf("  Missing supply:     %d (%.1f%%)\n",
            sum(is.na(df$supply_per_100k)), mean(is.na(df$supply_per_100k)) * 100))
cat(sprintf("  Missing vacancy:    %d (%.1f%%)\n",
            sum(is.na(df$vacancy_rate)), mean(is.na(df$vacancy_rate)) * 100))
cat("  Output: data/bc_allied_health_workforce.csv\n")
