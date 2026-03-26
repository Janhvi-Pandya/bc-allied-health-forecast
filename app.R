# =============================================================================
# app.R — BC Allied Health Workforce Interactive Dashboard
# =============================================================================
# Deploy to shinyapps.io:
#   library(rsconnect)
#   deployApp(appDir = ".", appName = "bc-allied-health-forecast")
# =============================================================================

library(shiny)
library(tidyverse)
library(broom)
library(scales)
library(bslib)

# --- Data & Pre-computation --------------------------------------------------

df <- read_csv("data/bc_allied_health_workforce.csv", show_col_types = FALSE)

# Impute missing values (same logic as analysis script)
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

all_professions <- sort(unique(df_clean$profession))
all_has          <- sort(unique(df_clean$health_authority))

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

# Pre-compute provincial totals
provincial <- df_clean %>%
  group_by(year, profession) %>%
  summarise(total_headcount = sum(headcount), .groups = "drop")

# Pre-compute forecasts
training <- provincial %>% filter(!(year %in% c(2020, 2021)))
models_list <- training %>%
  group_by(profession) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(total_headcount ~ year, data = .x)))

forecast_years <- tibble(year = 2025:2028)
forecasts_all <- models_list %>%
  mutate(
    pred = map(model, ~ {
      p <- predict(.x, newdata = forecast_years, interval = "prediction", level = 0.95)
      bind_cols(forecast_years, as_tibble(p))
    })
  ) %>%
  select(profession, pred) %>%
  unnest(pred) %>%
  rename(forecast = fit, lower_95 = lwr, upper_95 = upr) %>%
  mutate(across(c(forecast, lower_95, upper_95), round))


# --- UI ----------------------------------------------------------------------

ui <- page_navbar(
  title = "BC Allied Health Workforce",
  theme = bs_theme(bootswatch = "flatly", primary = "#2c7bb6"),
  nav_spacer(),
  nav_item(tags$a("Report", href = "https://janhvi-pandya.github.io/bc-allied-health-forecast/",
                   target = "_blank", class = "nav-link")),

  # Tab 1: Trends
  nav_panel("Trends",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        checkboxGroupInput("trend_profs", "Professions",
                           choices = all_professions,
                           selected = all_professions),
        actionButton("trend_all", "Select All", class = "btn-sm"),
        actionButton("trend_none", "Clear All", class = "btn-sm")
      ),
      card(
        card_header("Provincial Headcount Trends (2010–2024)"),
        plotOutput("trend_plot", height = "500px")
      ),
      card(
        card_header("Growth: 2010 vs 2024"),
        plotOutput("growth_plot", height = "400px")
      )
    )
  ),

  # Tab 2: Regional
  nav_panel("Regional",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        selectInput("region_year", "Year", choices = rev(2010:2024), selected = 2024),
        checkboxGroupInput("region_has", "Health Authorities",
                           choices = all_has, selected = all_has)
      ),
      card(
        card_header("Per-Capita Supply by Health Authority"),
        plotOutput("supply_plot", height = "500px")
      ),
      card(
        card_header("Vacancy Rate Heatmap"),
        plotOutput("heatmap_plot", height = "450px")
      )
    )
  ),

  # Tab 3: Vacancies
  nav_panel("Vacancies",
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        selectInput("vacancy_year", "Year", choices = rev(2010:2024), selected = 2024),
        checkboxGroupInput("vacancy_has", "Health Authorities",
                           choices = all_has, selected = all_has)
      ),
      card(
        card_header("Estimated Unfilled Positions"),
        plotOutput("unfilled_plot", height = "450px")
      ),
      card(
        card_header("Pre-COVID vs Post-COVID Vacancy Rates"),
        plotOutput("covid_plot", height = "400px")
      )
    )
  ),

  # Tab 4: Forecasts
  nav_panel("Forecasts",
    layout_sidebar(
      sidebar = sidebar(
        title = "Select Profession",
        selectInput("forecast_prof", "Profession",
                    choices = all_professions, selected = "Physiotherapist")
      ),
      card(
        card_header("Workforce Forecast (2025–2028)"),
        plotOutput("forecast_plot", height = "450px")
      ),
      card(
        card_header("Forecast Table"),
        tableOutput("forecast_table")
      )
    )
  ),

  # Tab 5: About
  nav_panel("About",
    card(
      card_header("About This Dashboard"),
      card_body(
        tags$p("This interactive dashboard accompanies the",
               tags$a("BC Allied Health Workforce Forecasting Analysis",
                      href = "https://janhvi-pandya.github.io/bc-allied-health-forecast/"),
               "report."),
        tags$h5("Data"),
        tags$p("Synthetic dataset modeled after CIHI and BC Ministry of Health workforce
                statistics. Covers 8 professions, 5 Health Authorities, 15 years (2010–2024)."),
        tags$h5("Methods"),
        tags$ul(
          tags$li("Group-median imputation for missing values (~3%)"),
          tags$li("OLS linear regression per profession (COVID years excluded)"),
          tags$li("95% prediction intervals for 2025–2028 forecasts"),
          tags$li("Welch's t-test for pre/post-COVID vacancy comparison")
        ),
        tags$h5("Author"),
        tags$p("Keriu Pandya"),
        tags$h5("Source Code"),
        tags$p(tags$a("GitHub Repository",
                      href = "https://github.com/Janhvi-Pandya/bc-allied-health-forecast",
                      target = "_blank"))
      )
    )
  )
)


# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # Trend tab: select all / clear all buttons
  observeEvent(input$trend_all, {
    updateCheckboxGroupInput(session, "trend_profs", selected = all_professions)
  })
  observeEvent(input$trend_none, {
    updateCheckboxGroupInput(session, "trend_profs", selected = character(0))
  })

  # --- Trends tab ---
  output$trend_plot <- renderPlot({
    req(length(input$trend_profs) > 0)
    provincial %>%
      filter(profession %in% input$trend_profs) %>%
      ggplot(aes(x = year, y = total_headcount, colour = profession)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_colour_manual(values = prof_colours) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Total Headcount", colour = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2))
  })

  output$growth_plot <- renderPlot({
    req(length(input$trend_profs) > 0)
    provincial %>%
      filter(profession %in% input$trend_profs, year %in% c(2010, 2024)) %>%
      pivot_wider(names_from = year, values_from = total_headcount, names_prefix = "y") %>%
      mutate(growth_pct = round((y2024 - y2010) / y2010 * 100, 1),
             profession = fct_reorder(profession, growth_pct)) %>%
      ggplot(aes(x = growth_pct, y = profession)) +
      geom_col(fill = "#2c7bb6", width = 0.7) +
      geom_text(aes(label = paste0(growth_pct, "%")), hjust = -0.1, size = 4) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Growth (%)", y = NULL) +
      theme_minimal(base_size = 14)
  })

  # --- Regional tab ---
  output$supply_plot <- renderPlot({
    req(length(input$region_has) > 0)
    df_clean %>%
      filter(year == as.integer(input$region_year),
             health_authority %in% input$region_has) %>%
      ggplot(aes(x = profession, y = supply_per_100k, fill = health_authority)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = ha_colours) +
      labs(x = NULL, y = "Per 100k Population", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "bottom")
  })

  output$heatmap_plot <- renderPlot({
    req(length(input$region_has) > 0)
    df_clean %>%
      filter(year == as.integer(input$region_year),
             health_authority %in% input$region_has) %>%
      ggplot(aes(x = health_authority, y = profession, fill = vacancy_rate)) +
      geom_tile(colour = "white", linewidth = 0.8) +
      geom_text(aes(label = percent(vacancy_rate, accuracy = 0.1)), size = 4) +
      scale_fill_gradient(low = "#ffffcc", high = "#d7191c", labels = percent_format()) +
      labs(x = NULL, y = NULL, fill = "Vacancy\nRate") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  # --- Vacancies tab ---
  output$unfilled_plot <- renderPlot({
    req(length(input$vacancy_has) > 0)
    df_clean %>%
      filter(year == as.integer(input$vacancy_year),
             health_authority %in% input$vacancy_has) %>%
      mutate(estimated_demand = round(headcount / (1 - vacancy_rate)),
             unfilled = estimated_demand - headcount) %>%
      group_by(profession) %>%
      summarise(total_unfilled = sum(unfilled), .groups = "drop") %>%
      mutate(profession = fct_reorder(profession, total_unfilled)) %>%
      ggplot(aes(x = total_unfilled, y = profession)) +
      geom_col(fill = "#d7191c", width = 0.7) +
      geom_text(aes(label = comma(total_unfilled)), hjust = -0.1, size = 4) +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Estimated Unfilled Positions", y = NULL) +
      theme_minimal(base_size = 14)
  })

  output$covid_plot <- renderPlot({
    pre  <- df_clean %>% filter(year >= 2015, year <= 2019) %>%
      mutate(period = "Pre-COVID (2015\u20132019)")
    post <- df_clean %>% filter(year >= 2022, year <= 2024) %>%
      mutate(period = "Post-COVID (2022\u20132024)")

    bind_rows(pre, post) %>%
      group_by(profession, period) %>%
      summarise(mean_vacancy = mean(vacancy_rate), .groups = "drop") %>%
      ggplot(aes(x = profession, y = mean_vacancy, fill = period)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Pre-COVID (2015\u20132019)" = "#abd9e9",
                                    "Post-COVID (2022\u20132024)" = "#d7191c")) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Mean Vacancy Rate", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "bottom")
  })

  # --- Forecasts tab ---
  output$forecast_plot <- renderPlot({
    prof <- input$forecast_prof

    hist_data <- provincial %>% filter(profession == prof)
    fc_data   <- forecasts_all %>% filter(profession == prof)

    ggplot() +
      annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
               fill = "grey80", alpha = 0.3) +
      geom_line(data = hist_data, aes(x = year, y = total_headcount),
                colour = "#2c7bb6", linewidth = 1.2) +
      geom_point(data = hist_data, aes(x = year, y = total_headcount),
                 colour = "#2c7bb6", size = 2.5) +
      geom_ribbon(data = fc_data, aes(x = year, ymin = lower_95, ymax = upper_95),
                  fill = "#fdae61", alpha = 0.4) +
      geom_line(data = fc_data, aes(x = year, y = forecast),
                colour = "#d7191c", linewidth = 1.2, linetype = "dashed") +
      geom_point(data = fc_data, aes(x = year, y = forecast),
                 colour = "#d7191c", size = 2.5) +
      scale_y_continuous(labels = comma) +
      labs(title = prof,
           subtitle = "Blue = historical | Red = forecast | Orange = 95% PI | Grey = COVID years (excluded)",
           x = NULL, y = "Provincial Headcount") +
      theme_minimal(base_size = 14)
  })

  output$forecast_table <- renderTable({
    forecasts_all %>%
      filter(profession == input$forecast_prof) %>%
      mutate(across(c(forecast, lower_95, upper_95), ~ format(., big.mark = ","))) %>%
      rename(Year = year, Forecast = forecast,
             `Lower 95%` = lower_95, `Upper 95%` = upper_95) %>%
      select(-profession)
  })
}

shinyApp(ui, server)
