# =============================================================================
# 01_analysis.R
# BC Allied Health Workforce — Full Analysis Pipeline
# =============================================================================
# Sections:
#   1. Data loading & quality assessment
#   2. Missing value imputation
#   3. Exploratory data analysis & visualization
#   4. Regression modeling & forecasting
#   5. Statistical testing (pre-COVID vs post-COVID vacancy rates)
#
# Inputs:  data/bc_allied_health_workforce.csv
# Outputs: output/*.png, output/*.csv, console diagnostics
# =============================================================================

library(tidyverse)
library(broom)
library(scales)

theme_set(theme_minimal(base_size = 12))

# Consistent colour palettes
ha_colours <- c(
  "Fraser"            = "#2c7bb6",
  "Vancouver Coastal" = "#d7191c",
  "Island"            = "#fdae61",
  "Interior"          = "#abd9e9",
  "Northern"          = "#018571"
)

prof_colours <- c(
  "Physiotherapist"              = "#1b9e77",
  "Occupational Therapist"       = "#d95f02",
  "Speech-Language Pathologist"  = "#7570b3",
  "Pharmacist"                   = "#e7298a",
  "Medical Lab Technologist"     = "#66a61e",
  "Respiratory Therapist"        = "#e6ab02",
  "Dietitian"                    = "#a6761d",
  "Social Worker"                = "#666666"
)


# =============================================================================
# 1. DATA LOADING & QUALITY ASSESSMENT
# =============================================================================

df <- read_csv("data/bc_allied_health_workforce.csv",
               col_types = cols(
                 year             = col_integer(),
                 profession       = col_character(),
                 health_authority = col_character(),
                 population       = col_double(),
                 headcount        = col_double(),
                 supply_per_100k  = col_double(),
                 vacancy_rate     = col_double()
               ))

cat("=== DATA QUALITY ASSESSMENT ===\n\n")
cat(sprintf("Dataset dimensions: %d rows x %d columns\n", nrow(df), ncol(df)))
cat(sprintf("Year range: %d - %d\n", min(df$year), max(df$year)))
cat(sprintf("Professions: %d\n", n_distinct(df$profession)))
cat(sprintf("Health Authorities: %d\n\n", n_distinct(df$health_authority)))

# Missing value summary
missing_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 1))

cat("Missing values by variable:\n")
print(as.data.frame(missing_summary), row.names = FALSE)
cat("\n")

# Missing values by profession x HA (check for systematic patterns)
missing_by_group <- df %>%
  group_by(profession, health_authority) %>%
  summarise(
    n_missing_headcount = sum(is.na(headcount)),
    n_missing_vacancy   = sum(is.na(vacancy_rate)),
    .groups = "drop"
  ) %>%
  filter(n_missing_headcount > 0 | n_missing_vacancy > 0)

cat("Missing values by profession x HA (non-zero only):\n")
print(as.data.frame(missing_by_group), row.names = FALSE)
cat("\n")


# =============================================================================
# 2. MISSING VALUE IMPUTATION — GROUP-MEDIAN
# =============================================================================
# Strategy: impute using the median of the same profession x HA group.
# Rationale: group-median is robust to outliers (unlike mean) and preserves
# the local distribution better than a global median would.

df_clean <- df %>%
  group_by(profession, health_authority) %>%
  mutate(
    headcount       = if_else(is.na(headcount),
                              median(headcount, na.rm = TRUE), headcount),
    supply_per_100k = if_else(is.na(supply_per_100k),
                              median(supply_per_100k, na.rm = TRUE), supply_per_100k),
    vacancy_rate    = if_else(is.na(vacancy_rate),
                              median(vacancy_rate, na.rm = TRUE), vacancy_rate)
  ) %>%
  ungroup()

# Validation: confirm no remaining NAs
stopifnot(sum(is.na(df_clean$headcount)) == 0)
stopifnot(sum(is.na(df_clean$supply_per_100k)) == 0)
stopifnot(sum(is.na(df_clean$vacancy_rate)) == 0)
cat("Post-imputation validation: PASSED (0 remaining NAs)\n\n")


# =============================================================================
# 3. EXPLORATORY DATA ANALYSIS & VISUALIZATION
# =============================================================================

# --- Plot 1: Provincial workforce trends over time ----------------------------

provincial_trends <- df_clean %>%
  group_by(year, profession) %>%
  summarise(total_headcount = sum(headcount), .groups = "drop")

p1 <- ggplot(provincial_trends, aes(x = year, y = total_headcount,
                                     colour = profession)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = prof_colours) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "BC Allied Health Workforce by Profession (2010\u20132024)",
    subtitle = "Provincial headcount totals across all Health Authorities",
    x = NULL, y = "Total Headcount", colour = "Profession"
  ) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9)) +
  guides(colour = guide_legend(nrow = 2))

ggsave("output/01_provincial_trends.png", p1, width = 10, height = 6, dpi = 300)
cat("Saved: output/01_provincial_trends.png\n")


# --- Plot 2: Growth comparison 2010 vs 2024 ----------------------------------

growth_comparison <- df_clean %>%
  filter(year %in% c(2010, 2024)) %>%
  group_by(year, profession) %>%
  summarise(total = sum(headcount), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = total, names_prefix = "y") %>%
  mutate(
    growth_pct = round((y2024 - y2010) / y2010 * 100, 1),
    profession = fct_reorder(profession, growth_pct)
  )

p2 <- ggplot(growth_comparison, aes(x = growth_pct, y = profession)) +
  geom_col(fill = "#2c7bb6", width = 0.7) +
  geom_text(aes(label = paste0(growth_pct, "%")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Workforce Growth by Profession (2010\u20132024)",
    subtitle = "Percentage change in provincial headcount",
    x = "Growth (%)", y = NULL
  )

ggsave("output/02_growth_comparison.png", p2, width = 9, height = 5, dpi = 300)
cat("Saved: output/02_growth_comparison.png\n")


# --- Plot 3: Per-capita supply rates by HA (2024) ----------------------------

supply_2024 <- df_clean %>%
  filter(year == 2024)

p3 <- ggplot(supply_2024, aes(x = profession, y = supply_per_100k,
                               fill = health_authority)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = ha_colours) +
  labs(
    title    = "Per-Capita Supply Rates by Health Authority (2024)",
    subtitle = "Allied health professionals per 100,000 population",
    x = NULL, y = "Professionals per 100k", fill = "Health Authority"
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

ggsave("output/03_supply_by_ha.png", p3, width = 11, height = 6, dpi = 300)
cat("Saved: output/03_supply_by_ha.png\n")


# --- Plot 4: Vacancy rate heatmap (2024) -------------------------------------

vacancy_2024 <- df_clean %>%
  filter(year == 2024)

p4 <- ggplot(vacancy_2024, aes(x = health_authority, y = profession,
                                fill = vacancy_rate)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = percent(vacancy_rate, accuracy = 0.1)),
            size = 3.2) +
  scale_fill_gradient(low = "#ffffcc", high = "#d7191c",
                      labels = percent_format()) +
  labs(
    title    = "Vacancy Rate Heatmap by Profession \u00d7 Health Authority (2024)",
    subtitle = "Higher values indicate greater workforce shortage",
    x = NULL, y = NULL, fill = "Vacancy Rate"
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("output/04_vacancy_heatmap.png", p4, width = 10, height = 6, dpi = 300)
cat("Saved: output/04_vacancy_heatmap.png\n")


# --- Plot 5: Estimated unfilled positions (2024) -----------------------------

unfilled <- df_clean %>%
  filter(year == 2024) %>%
  mutate(
    estimated_demand  = round(headcount / (1 - vacancy_rate)),
    unfilled_positions = estimated_demand - headcount
  ) %>%
  group_by(profession) %>%
  summarise(total_unfilled = sum(unfilled_positions), .groups = "drop") %>%
  mutate(profession = fct_reorder(profession, total_unfilled))

p5 <- ggplot(unfilled, aes(x = total_unfilled, y = profession)) +
  geom_col(fill = "#d7191c", width = 0.7) +
  geom_text(aes(label = comma(total_unfilled)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Estimated Unfilled Positions by Profession (2024)",
    subtitle = "Derived from headcount and vacancy rates across all Health Authorities",
    x = "Estimated Unfilled Positions", y = NULL
  )

ggsave("output/05_unfilled_positions.png", p5, width = 9, height = 5, dpi = 300)
cat("Saved: output/05_unfilled_positions.png\n")


# =============================================================================
# 4. REGRESSION MODELING & FORECASTING
# =============================================================================
# Fit a linear regression per profession on provincial headcount totals.
# COVID years (2020-2021) are excluded to avoid distorting the trend.
#
# Model: total_headcount ~ year
# Forecast horizon: 2025-2028 with 95% prediction intervals

provincial_annual <- df_clean %>%
  group_by(year, profession) %>%
  summarise(total_headcount = sum(headcount), .groups = "drop")

# Exclude COVID years from model fitting
training_data <- provincial_annual %>%
  filter(!(year %in% c(2020, 2021)))

# Fit models
models <- training_data %>%
  group_by(profession) %>%
  nest() %>%
  mutate(
    model   = map(data, ~ lm(total_headcount ~ year, data = .x)),
    glance  = map(model, glance),
    tidy    = map(model, tidy)
  )

# --- Model diagnostics --------------------------------------------------------

cat("\n=== REGRESSION MODEL DIAGNOSTICS ===\n\n")

diagnostics <- models %>%
  select(profession, glance) %>%
  unnest(glance) %>%
  select(profession, r.squared, adj.r.squared, p.value, sigma) %>%
  mutate(across(c(r.squared, adj.r.squared), ~ round(., 4)),
         p.value = format.pval(p.value, digits = 3))

print(as.data.frame(diagnostics), row.names = FALSE)
cat("\n")

# Extract annual growth rates from coefficients
growth_rates <- models %>%
  select(profession, tidy) %>%
  unnest(tidy) %>%
  filter(term == "year") %>%
  select(profession, estimate, std.error, p.value) %>%
  mutate(
    annual_growth = round(estimate, 1),
    se            = round(std.error, 1),
    p.value       = format.pval(p.value, digits = 3)
  )

cat("Annual growth rates (headcount/year):\n")
print(as.data.frame(growth_rates %>% select(profession, annual_growth, se, p.value)),
      row.names = FALSE)
cat("\n")

# --- Forecasts ----------------------------------------------------------------

forecast_years <- tibble(year = 2025:2028)

forecasts <- models %>%
  select(profession, model) %>%
  mutate(
    predictions = map(model, ~ {
      pred <- predict(.x, newdata = forecast_years, interval = "prediction",
                       level = 0.95)
      bind_cols(forecast_years, as_tibble(pred))
    })
  ) %>%
  select(profession, predictions) %>%
  unnest(predictions) %>%
  rename(forecast = fit, lower_95 = lwr, upper_95 = upr) %>%
  mutate(across(c(forecast, lower_95, upper_95), round))

cat("Workforce forecasts (2025-2028):\n")
print(as.data.frame(forecasts), row.names = FALSE)

write_csv(forecasts, "output/forecast_summary.csv")
cat("\nSaved: output/forecast_summary.csv\n")


# --- Plot 6: Forecast panels by profession -----------------------------------

# Combine historical + forecast for plotting
historical_plot <- provincial_annual %>%
  mutate(type = "Historical")

forecast_plot <- forecasts %>%
  rename(total_headcount = forecast) %>%
  mutate(type = "Forecast")

combined <- bind_rows(
  historical_plot %>% select(year, profession, total_headcount, type),
  forecast_plot   %>% select(year, profession, total_headcount, type)
)

p6 <- ggplot() +
  # Historical line
  geom_line(data = filter(combined, type == "Historical"),
            aes(x = year, y = total_headcount),
            colour = "#2c7bb6", linewidth = 0.8) +
  geom_point(data = filter(combined, type == "Historical"),
             aes(x = year, y = total_headcount),
             colour = "#2c7bb6", size = 1.2) +
  # Forecast line + ribbon
  geom_ribbon(data = forecast_plot,
              aes(x = year, ymin = lower_95, ymax = upper_95),
              fill = "#fdae61", alpha = 0.35) +
  geom_line(data = filter(combined, type == "Forecast"),
            aes(x = year, y = total_headcount),
            colour = "#d7191c", linewidth = 0.8, linetype = "dashed") +
  geom_point(data = filter(combined, type == "Forecast"),
             aes(x = year, y = total_headcount),
             colour = "#d7191c", size = 1.2) +
  # COVID exclusion shading
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.25) +
  facet_wrap(~ profession, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Allied Health Workforce Forecasts (2025\u20132028)",
    subtitle = "Blue = historical | Red dashed = forecast | Shaded band = 95% prediction interval\nGrey zone = COVID years (excluded from model)",
    x = NULL, y = "Provincial Headcount"
  ) +
  theme(strip.text = element_text(face = "bold", size = 10))

ggsave("output/06_forecast_panels.png", p6, width = 12, height = 10, dpi = 300)
cat("Saved: output/06_forecast_panels.png\n")


# =============================================================================
# 5. STATISTICAL TESTING — PRE-COVID vs POST-COVID VACANCY RATES
# =============================================================================
# Welch's t-test comparing mean vacancy rates before (2015-2019) vs
# after (2022-2024) the pandemic, by profession.

pre_covid  <- df_clean %>% filter(year >= 2015, year <= 2019)
post_covid <- df_clean %>% filter(year >= 2022, year <= 2024)

t_test_results <- tibble(profession = unique(df_clean$profession)) %>%
  mutate(
    test = map(profession, ~ {
      pre  <- pre_covid  %>% filter(profession == .x) %>% pull(vacancy_rate)
      post <- post_covid %>% filter(profession == .x) %>% pull(vacancy_rate)
      t.test(post, pre, alternative = "greater")
    }),
    tidied = map(test, tidy)
  ) %>%
  unnest(tidied) %>%
  select(profession, estimate1, estimate2, statistic, p.value, conf.low) %>%
  rename(
    mean_post_covid = estimate1,
    mean_pre_covid  = estimate2,
    t_statistic     = statistic
  ) %>%
  mutate(
    difference = round(mean_post_covid - mean_pre_covid, 4),
    significant = if_else(p.value < 0.05, "Yes", "No"),
    across(c(mean_post_covid, mean_pre_covid), ~ round(., 4)),
    p.value = round(p.value, 4)
  )

cat("\n=== WELCH'S T-TEST: POST-COVID vs PRE-COVID VACANCY RATES ===\n")
cat("H1: Post-COVID vacancy rates are higher than pre-COVID\n\n")
print(as.data.frame(t_test_results %>%
  select(profession, mean_pre_covid, mean_post_covid, difference,
         t_statistic, p.value, significant)),
  row.names = FALSE)

write_csv(t_test_results, "output/t_test_results.csv")
cat("\nSaved: output/t_test_results.csv\n")


# --- Plot 7: Pre vs post-COVID vacancy comparison ----------------------------

vacancy_comparison <- bind_rows(
  pre_covid  %>% mutate(period = "Pre-COVID (2015\u20132019)"),
  post_covid %>% mutate(period = "Post-COVID (2022\u20132024)")
) %>%
  group_by(profession, period) %>%
  summarise(mean_vacancy = mean(vacancy_rate), .groups = "drop")

p7 <- ggplot(vacancy_comparison, aes(x = profession, y = mean_vacancy,
                                      fill = period)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Pre-COVID (2015\u20132019)" = "#abd9e9",
                                "Post-COVID (2022\u20132024)" = "#d7191c")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Vacancy Rates: Pre-COVID vs Post-COVID",
    subtitle = "Mean vacancy rate by profession (Welch\u2019s t-test, \u03b1 = 0.05)",
    x = NULL, y = "Mean Vacancy Rate", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
        legend.position = "bottom")

ggsave("output/07_vacancy_comparison.png", p7, width = 10, height = 6, dpi = 300)
cat("Saved: output/07_vacancy_comparison.png\n")


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== PIPELINE COMPLETE ===\n")
cat("Outputs:\n")
cat("  Plots:  output/01_provincial_trends.png\n")
cat("          output/02_growth_comparison.png\n")
cat("          output/03_supply_by_ha.png\n")
cat("          output/04_vacancy_heatmap.png\n")
cat("          output/05_unfilled_positions.png\n")
cat("          output/06_forecast_panels.png\n")
cat("          output/07_vacancy_comparison.png\n")
cat("  Tables: output/forecast_summary.csv\n")
cat("          output/t_test_results.csv\n")
