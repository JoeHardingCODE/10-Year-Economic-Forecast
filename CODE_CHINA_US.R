library(WDI)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# 2. Fetch and tidy GDP data (current USD)
us_data <- WDI(
  country   = "US",
  indicator = "NY.GDP.MKTP.CD",
  start     = 1960,
  end       = 2024
) %>%
  arrange(year) %>%
  transmute(
    Year    = year,
    GDP     = NY.GDP.MKTP.CD,
    Country = "United States"
  )

china_data <- WDI(
  country   = "CN",
  indicator = "NY.GDP.MKTP.CD",
  start     = 1960,
  end       = 2024
) %>%
  arrange(year) %>%
  transmute(
    Year    = year,
    GDP     = NY.GDP.MKTP.CD,
    Country = "China"
  )


# 3. Convert to trillions USD

prepare_gdp <- function(df) {
  df %>% mutate(GDP_trillion = GDP / 1e12)
}

us_data    <- prepare_gdp(us_data)
china_data <- prepare_gdp(china_data)



# ──────────────────────────────────────────────────────────────────────────────
# 4. Fit automatic ARIMA models
# ──────────────────────────────────────────────────────────────────────────────
us_ts    <- ts(us_data$GDP_trillion,    start = 1960, frequency = 1)
china_ts <- ts(china_data$GDP_trillion, start = 1960, frequency = 1)

us_model    <- auto.arima(us_ts,    seasonal = FALSE, ic = "aicc",
                          stepwise = FALSE, approximation = FALSE)

china_model <- auto.arima(china_ts, seasonal = FALSE, ic = "aicc",
                          stepwise = FALSE, approximation = FALSE)


# 5. Generate 11‐year forecast with 90% intervals

forecast_years <- 2025:2035
horizon        <- length(forecast_years)

us_fc <- forecast(us_model,    h = horizon, level = 90) %>%
  as.data.frame() %>%
  transmute(
    Year         = forecast_years,
    GDP_trillion = `Point Forecast`,
    Lower        = `Lo 90`,
    Upper        = `Hi 90`,
    Country      = "United States"
  )

china_fc <- forecast(china_model, h = horizon, level = 90) %>%
  as.data.frame() %>%
  transmute(
    Year         = forecast_years,
    GDP_trillion = `Point Forecast`,
    Lower        = `Lo 90`,
    Upper        = `Hi 90`,
    Country      = "China"
  )


# 6. Combine historical and forecast data
combine_data <- function(hist, fc) {
  hist %>%
    transmute(
      Year         = Year,
      GDP_trillion = GDP_trillion,
      Country      = Country,
      Type         = "Historical",
      Lower        = GDP_trillion,
      Upper        = GDP_trillion
    ) %>%
    bind_rows(
      fc %>%
        mutate(Type = "Forecast")
    )
}

us_combined    <- combine_data(us_data,    us_fc)
china_combined <- combine_data(china_data, china_fc)

all_data <- bind_rows(us_combined, china_combined)


# 7. Plot (2000–2035): historical & forecast with 95% bands
ggplot(
  all_data %>% filter(Year >= 2000),
  aes(x = Year, y = GDP_trillion, color = Country, fill = Country)
) +
  # 90% forecast bands (only for Forecast)
  geom_ribbon(
    data = . %>% filter(Type == "Forecast"),
    aes(x = Year, ymin = Lower, ymax = Upper),
    alpha = 0.15
  ) +
  # Lines: solid = historical, dashed = forecast
  geom_line(aes(linetype = Type), linewidth = 1.2) +
  # Highlight last observed point (2024)
  geom_point(
    data = . %>% filter(Year == 2024),
    aes(x = Year, y = GDP_trillion),
    size  = 3,
    shape = 21,
    fill  = "white",
    stroke = 1.5
  ) +
  # Scales & labels
  scale_color_manual(values = c("China" = "red", "United States" = "blue")) +
  scale_fill_manual(values  = c("China" = "red", "United States" = "blue")) +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed")) +
  scale_y_continuous(
    "GDP (Trillions USD)",
    labels = label_dollar(prefix = "$", suffix = "T"),
    breaks = breaks_extended(8),
    limits = c(0, NA)
  ) +
  scale_x_continuous(breaks = seq(2000, 2035, by = 5)) +
  labs(
    title    = "GDP Forecasts 2025–2035: China vs United States",
    subtitle = "Automatic ARIMA models with 90% Confidence intervals",
    caption  = "Source: World Bank WDI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey"),
    plot.title        = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle     = element_text(color = "gray", margin = margin(b = 20), hjust = 0.5),
    legend.title      = element_blank(),
    legend.box        = "horizontal",
    legend.margin     = margin(t = 10),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    plot.caption      = element_text(color = "gray", margin = margin(t = 15))
  ) +
  guides(
    color    = guide_legend(override.aes = list(fill = NA, size = 3)),
    linetype = guide_legend(override.aes = list(fill = NA))
  )

# Print ARIMA model summaries and coefficients
cat("United States ARIMA model:\n")
print(summary(us_model))
cat("\nModel coefficients (US):\n")
print(us_model$coef)

cat("\n\nChina ARIMA model:\n")
print(summary(china_model))
cat("\nModel coefficients (China):\n")
print(china_model$coef)
