#Load required libraries
library(dplyr)
library(tidyr)
library(BVAR)
library(forecast)
library(ggplot2)
library(scales)

#Create GDP growth data frame, manually inserted
years <- 1961:2024
China_values <- c(
  -27.27, -5.58, 10.3, 18.18, 16.95, 10.65, -5.77, -4.1, 16.94, 19.3, 7.06, 3.81,
  7.76,  2.31, 8.72, -1.57, 7.57, 11.326,  7.59132, 7.83415, 5.11276, 9.05928,
  10.7274,15.2326,13.4355, 8.93506,11.6282,11.2226, 4.20633, 3.94163, 9.3617,
  14.2996,13.9291,13.0727,11.0328, 9.9762, 9.28765, 7.92079, 7.737,  8.58643,
  8.31264, 9.24339,10.1182,10.1314,11.4582,12.6747,14.15,   9.67025, 9.4027,
  10.5963, 9.46134, 7.85617, 7.77872, 7.4619,  6.97878, 6.77556, 6.89127, 6.75672,
  6.0685,  2.34019, 8.57009, 3.13419, 5.41484, 4.97736
)
US_values <- c(
  2.56534, 6.12964, 4.35729, 5.76275, 6.49845, 6.59534, 2.74267, 4.91451, 3.12248,
  0.184619,3.29272, 5.2555,  5.64568, -0.54055, -0.205619,5.38803, 4.62419, 5.53521,
  3.16599, -0.256776,2.5377, -1.80301, 4.58379, 7.23645, 4.16958, 3.46266, 3.45463,
  4.17698, 3.67224, 1.88597, -0.108313,3.5225,  2.7518,  4.02902, 2.68443, 3.77277,
  4.44713, 4.48313, 4.78843, 4.07759, 0.955538,1.70045, 2.79561, 3.84777, 3.48355,
  2.78454, 2.00386, 0.113587,-2.5765, 2.69519, 1.56441, 2.28911, 2.11783, 2.52382,
  2.94555, 1.81945, 2.45762, 2.96651, 2.58383,-2.16303,6.05505, 2.51238, 2.88756,
  2.79619
)

gdp_wide <- data.frame(
  Year  = years,
  China = China_values,
  US    = US_values
)

#China via BVAR on differenced series
#same method as the other program

diff_data <- data.frame(
  China = diff(gdp_wide$China),
  US    = diff(gdp_wide$US)
)
set.seed(123)
bvar_model <- bvar(
  data    = diff_data,
  lags    = 1,
  n_draw  = 5000,
  n_burn  = 1000,
  verbose = FALSE
)

bvar_fcast  <- predict(bvar_model, horizon = 11)
mean_diffs  <- apply(bvar_fcast$fcast, c(2,3), mean)
last_china      <- tail(gdp_wide$China, 1)
china_pred_diff <- mean_diffs[, 1]           
china_pred      <- last_china + cumsum(china_pred_diff)
forecast_years  <- (max(gdp_wide$Year) + 1):(max(gdp_wide$Year) + 11)

china_forecast_df <- data.frame(
  Year    = forecast_years,
  Growth  = china_pred,
  Country = "China",
  Type    = "Forecast"
)

#Fit the Arima forecast for the US 
#same method as the other program
us_ts   <- ts(gdp_wide$US, start = min(gdp_wide$Year), frequency = 1)
us_fit  <- auto.arima(
  us_ts,
  seasonal      = FALSE,
  stepwise      = FALSE,
  approximation = FALSE,
  trace         = FALSE
)

us_fcast   <- forecast(us_fit, h = length(forecast_years))$mean
us_forecast_df <- data.frame(
  Year    = forecast_years,
  Growth  = as.numeric(us_fcast),
  Country = "US",
  Type    = "Forecast"
)

#Prepare data
actual_df <- gdp_wide %>%
  filter(Year >= 2000) %>%
  pivot_longer(cols = c(China, US), names_to = "Country", values_to = "Growth") %>%
  mutate(Type = "Actual")

#Combine actual data + forecasted data
plot_df <- bind_rows(
  actual_df,
  china_forecast_df  %>% filter(Year >= 2000),
  us_forecast_df     %>% filter(Year >= 2000)
)

#Plot two curves 
ggplot(plot_df, aes(x = Year, y = Growth, color = Country, linetype = Type)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(China = "red", US = "blue")) +
  scale_linetype_manual(values = c(Actual = "solid", Forecast = "dashed")) +
  labs(
    title    = "GDP Growth (Annual %)",
    subtitle = paste0(
      "China: BVAR, ",
      "US: ARIMA(", paste0(arimaorder(us_fit), collapse = ","), ")"
    ),
    x        = "Year",
    y        = "GDP Growth (%)",
    color    = "Country",
    linetype = "Data Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
