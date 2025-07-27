#required libraries
library(dplyr)
library(tidyr)
library(BVAR)
library(ggplot2)
library(scales)

#Manually inserted data set from GDPgrowth spreadsheet
years <- 1961:2024
China_values <- c(-27.27, -5.58, 10.3, 18.18, 16.95, 10.65, -5.77, -4.1, 16.94, 19.3, 7.06, 3.81, 7.76, 2.31, 8.72, 
                  -1.57, 7.57, 11.326, 7.59132, 7.83415, 5.11276, 9.05928, 10.7274, 15.2326, 13.4355, 8.93506, 11.6282, 
                  11.2226, 4.20633, 3.94163, 9.3617, 14.2996, 13.9291, 13.0727, 11.0328, 9.9762, 9.28765, 7.92079, 7.737, 
                  8.58643, 8.31264, 9.24339, 10.1182, 10.1314, 11.4582, 12.6747, 14.15, 9.67025, 9.4027, 10.5963, 9.46134, 
                  7.85617, 7.77872, 7.4619, 6.97878, 6.77556, 6.89127, 6.75672, 6.0685, 2.34019, 8.57009, 3.13419, 5.41484, 4.97736)
US_values <- c(2.56534, 6.12964, 4.35729, 5.76275, 6.49845, 6.59534, 2.74267, 4.91451, 3.12248, 0.184619, 3.29272, 
               5.2555, 5.64568, -0.54055, -0.205619, 5.38803, 4.62419, 5.53521, 3.16599, -0.256776, 2.5377, -1.80301, 
               4.58379, 7.23645, 4.16958, 3.46266, 3.45463, 4.17698, 3.67224, 1.88597, -0.108313, 3.5225, 2.7518, 4.02902, 
               2.68443, 3.77277, 4.44713, 4.48313, 4.78843, 4.07759, 0.955538, 1.70045, 2.79561, 3.84777, 3.48355, 2.78454, 
               2.00386, 0.113587, -2.5765, 2.69519, 1.56441, 2.28911, 2.11783, 2.52382, 2.94555, 1.81945, 2.45762, 2.96651, 
               2.58383, -2.16303, 6.05505, 2.51238, 2.88756, 2.79619)

gdp_wide <- data.frame(
  year = years,
  China = China_values,
  US = US_values
) %>% 
  arrange(year) %>%
  filter(!is.na(China) & !is.na(US))

#create differenced GDP growth data 
diff_data <- data.frame(
  China = diff(gdp_wide$China),
  US    = diff(gdp_wide$US)
)

#Fit Bayesian VAR model on the full-sample diff_data
set.seed(123)
model <- bvar(
  data    = diff_data,
  lags    = 1,
  n_draw  = 5000,
  n_burn  = 1000,
  verbose = FALSE
)

# 5. Forecast 11 years ahead (2025–2035)
fcast <- predict(model, horizon = 11)

# 6. Extract forecast means of differenced growth
means_diff <- apply(fcast$fcast, c(2, 3), mean)
means_diff <- as.matrix(means_diff)
colnames(means_diff) <- c("China", "US")

# 7. Reconstruct growth levels from last observed data
last_China <- tail(gdp_wide$China, 1)
last_US    <- tail(gdp_wide$US, 1)

China_pred <- last_China + cumsum(means_diff[, "China"])
US_pred    <- last_US    + cumsum(means_diff[, "US"])

forecast_years <- (max(gdp_wide$year) + 1):(max(gdp_wide$year) + 11)

forecast_df <- data.frame(
  Year    = rep(forecast_years, 2),
  Country = rep(c("China", "US"), each = 11),
  Growth  = c(China_pred, US_pred)
)

# 8. Prepare actual data for plotting
actual_df <- data.frame(
  Year    = rep(gdp_wide$year, 2),
  Country = rep(c("China", "US"), each = nrow(gdp_wide)),
  Growth  = c(gdp_wide$China, gdp_wide$US)
)

# 9. Plot actual vs. forecasted growth from 2000 onward
ggplot() +
  geom_line(
    data = actual_df %>% filter(Year >= 2000),
    aes(x = Year, y = Growth, color = Country),
    size = 1.2
  ) +
  geom_line(
    data = forecast_df %>% filter(Year >= 2000),
    aes(x = Year, y = Growth, color = Country),
    linetype = "dashed", size = 1.2
  ) +
  labs(
    title    = "GDP Growth (Annual %) – China vs US (2000–2035)",
    subtitle = "Bayesian VAR Forecast (Dashed: 2025–2035)",
    x        = "Year", y = "GDP Growth (%)",
    color    = "Country"
  ) +
  theme_minimal() +
  theme(
    text            = element_text(size = 14),
    legend.position = "bottom"
  )

# 10. In‑Sample One‑Step‑Ahead Error Metrics (ME, RMSE, MAE, MAPE, MASE)
# --------------------------------------------------------------------------------
resid_mat <- residuals(model)
actual_mat <- tail(as.matrix(diff_data), nrow(resid_mat))
fitted_mat <- actual_mat - as.matrix(resid_mat)

compute_metrics <- function(actual, fitted) {
  e       <- fitted - actual
  ME      <- mean(e)
  RMSE    <- sqrt(mean(e^2))
  MAE     <- mean(abs(e))
  MAPE    <- mean(abs(e / actual) * 100)
  den     <- mean(abs(diff(actual)), na.rm = TRUE)
  MASE    <- ifelse(den > 0, MAE / den, NA)
  c(ME = ME, RMSE = RMSE, MAE = MAE, MAPE = MAPE, MASE = MASE)
}

countries    <- colnames(diff_data)
metrics_list <- lapply(countries, function(ctry) {
  act <- actual_mat[, ctry]
  fit <- fitted_mat[, ctry]
  m   <- compute_metrics(act, fit)
  data.frame(Country = ctry, t(m), row.names = NULL)
})
in_sample_accuracy <- do.call(rbind, metrics_list)

cat("\n=== In‑Sample One‑Step‑Ahead Metrics (Growth) ===\n")
print(in_sample_accuracy)