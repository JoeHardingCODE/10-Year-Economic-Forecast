#Load required libraries
library(WDI)
library(dplyr)
library(tidyr)
library(BVAR)
library(ggplot2)
library(scales)

#Retrieve real GDP (constant 2015 USD) data for China and US
gdp_raw <- WDI(
  country = c("CN", "US"),
  indicator = "NY.GDP.MKTP.KD",
  start = 2000, end = 2024
)

#Clean data
gdp_wide <- gdp_raw %>%
  select(iso2c, year, NY.GDP.MKTP.KD) %>%
  pivot_wider(names_from = iso2c, values_from = NY.GDP.MKTP.KD) %>%
  rename(China = CN, US = US) %>%
  arrange(year) %>%
  filter(!is.na(China) & !is.na(US))

#Log-transform for stationality 
log_data <- log(gdp_wide[, c("China", "US")])

#Create differenced log GDP
diff_log <- diff(as.matrix(log_data))  # stationary input for BVAR

#Fit BVAR model
set.seed(123)
model <- bvar(diff_log, lags = 2, n_draw = 5000, n_burn = 1000, verbose = FALSE)

#Forecast 10 years ahead
forecast_horizon <- 11
fcast <- predict(model, horizon = forecast_horizon)

#Extract mean of forecasted growth (log differences)
log_diff_forecast <- apply(fcast$fcast, c(2, 3), mean)  # [horizon x variables]
log_diff_forecast <- as.matrix(log_diff_forecast)
colnames(log_diff_forecast) <- c("China", "US")

#Reconstruct GDP levels
last_log <- as.numeric(tail(log_data, 1))
log_forecast_levels <- matrix(NA, nrow = forecast_horizon, ncol = 2)
log_forecast_levels[1, ] <- last_log + log_diff_forecast[1, ]

for (i in 2:forecast_horizon) {
  log_forecast_levels[i, ] <- log_forecast_levels[i - 1, ] + log_diff_forecast[i, ]
}

#back-transform log-GDP to level GDP
gdp_forecast_levels <- exp(log_forecast_levels)
forecast_years <- (max(gdp_wide$year) + 1):(max(gdp_wide$year) + forecast_horizon)

forecast_df <- data.frame(
  Year = rep(forecast_years, 2),
  Country = rep(c("China", "US"), each = forecast_horizon),
  GDP = c(gdp_forecast_levels[, 1], gdp_forecast_levels[, 2])
)

#Merge with historical data
actual_df <- data.frame(
  Year = rep(gdp_wide$year, 2),
  Country = rep(c("China", "US"), each = nrow(gdp_wide)),
  GDP = c(gdp_wide$China, gdp_wide$US)
)

combined_df <- bind_rows(actual_df, forecast_df)

#Plot GDP levels
ggplot(combined_df, aes(x = Year, y = GDP, color = Country)) +
  geom_line(size = 1.2) +
  labs(
    title = "Real GDP (constant 2015 USD) – China vs US (2000–2035)",
    subtitle = "Bayesian VAR Forecast from 2025 onwards",
    x = "Year", y = "GDP (constant 2015 USD)",
    color = "Country"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme(
    text = element_text(size = 14),
    legend.position = "bottom"
  )
