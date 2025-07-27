library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)

#Parameters
start_model_year <- 1961
start_plot_year <- 2000
last_data_year <- 2024
end_forecast <- 2035
target_countries <- c("China", "United States")

#Create GDP growth data
years <- 1961:2024

#These are the manually extracted values from spreadsheet (GDPgrowth)
country_data <- list(
  China = c(
    -27.27, -5.58, 10.3, 18.18, 16.95, 10.65, -5.77, -4.1, 16.94, 19.3, 7.06, 3.81, 7.76, 2.31, 8.72, 
    -1.57, 7.57, 11.326, 7.59132, 7.83415, 5.11276, 9.05928, 10.7274, 15.2326, 13.4355, 8.93506, 11.6282, 
    11.2226, 4.20633, 3.94163, 9.3617, 14.2996, 13.9291, 13.0727, 11.0328, 9.9762, 9.28765, 7.92079, 7.737, 
    8.58643, 8.31264, 9.24339, 10.1182, 10.1314, 11.4582, 12.6747, 14.15, 9.67025, 9.4027, 10.5963, 9.46134, 
    7.85617, 7.77872, 7.4619, 6.97878, 6.77556, 6.89127, 6.75672, 6.0685, 2.34019, 8.57009, 3.13419, 5.41484, 4.97736
  ),
  "United States" = c(
    2.56534, 6.12964, 4.35729, 5.76275, 6.49845, 6.59534, 2.74267, 4.91451, 3.12248, 0.184619, 3.29272, 
    5.2555, 5.64568, -0.54055, -0.205619, 5.38803, 4.62419, 5.53521, 3.16599, -0.256776, 2.5377, -1.80301, 
    4.58379, 7.23645, 4.16958, 3.46266, 3.45463, 4.17698, 3.67224, 1.88597, -0.108313, 3.5225, 2.7518, 4.02902, 
    2.68443, 3.77277, 4.44713, 4.48313, 4.78843, 4.07759, 0.955538, 1.70045, 2.79561, 3.84777, 3.48355, 2.78454, 
    2.00386, 0.113587, -2.5765, 2.69519, 1.56441, 2.28911, 2.11783, 2.52382, 2.94555, 1.81945, 2.45762, 2.96651, 
    2.58383, -2.16303, 6.05505, 2.51238, 2.88756, 2.79619
  )
)

#Create matrix for storage
all_data <- list()
model_info <- list()  # matrix for model specifications
accuracy_metrics <- list()  # matrix for accuracy metrics

for(country in target_countries) {
  full_df <- data.frame(
    Year = years,
    GDP_Growth = country_data[[country]]
  ) %>%
    dplyr::filter(Year >= start_model_year, 
                  Year <= last_data_year,
                  !is.na(GDP_Growth)) %>%
    dplyr::arrange(Year)
  
  #create time series
  ts_data <- ts(full_df$GDP_Growth, start = start_model_year, frequency = 1)
  
  #Use auto.arima to find optimal model (maximizes accuracy)
  fit <- forecast::auto.arima(
    ts_data, 
    seasonal = FALSE,
    stepwise = FALSE,       
    approximation = FALSE,
    trace = TRUE            
  )     #iterativley checks each ARIMA set up to minimise MASE
  
  # Store model information
  model_order <- forecast::arimaorder(fit)
  model_info[[country]] <- paste0(
    "ARIMA(", model_order["p"], ",", model_order["d"], ",", model_order["q"], ")"
  )
  
  # Forecast next years
  forecast_horizon <- end_forecast - last_data_year
  suppressWarnings({
    gdp_forecast <- forecast::forecast(fit, h = forecast_horizon)
  })
  
  #Compute accuracy metrics using fitted values
  fitted_values <- as.numeric(fitted(fit))
  actual_values <- as.numeric(ts_data)
  
  #Align prediction values with actual values
  start_index <- length(actual_values) - length(fitted_values) + 1
  aligned_actual <- actual_values[start_index:length(actual_values)]
  
  #Compute error statistics
  e <- fitted_values - aligned_actual
  ME <- mean(e)
  RMSE <- sqrt(mean(e^2))
  MAE <- mean(abs(e))
  MAPE <- mean(abs(e / aligned_actual)) * 100
  
  #Compute MASE
  naive_errors <- diff(actual_values)
  denom <- mean(abs(naive_errors))
  MASE <- MAE / denom
  
  #Store accuracy statistics
  accuracy_metrics[[country]] <- data.frame(
    Country = country,
    ME = ME,
    RMSE = RMSE,
    MAE = MAE,
    MAPE = MAPE,
    MASE = MASE
  )
  
  #Prepare data frames for plotting
  forecast_years <- (last_data_year + 1):end_forecast
  forecast_df <- data.frame(
    Year = forecast_years,
    GDP_Growth = as.numeric(gdp_forecast$mean),
    lower = gdp_forecast$lower[,2],
    upper = gdp_forecast$upper[,2],
    Type = "Forecast"
  )
  
  #Filter data for plotting
  plot_df <- full_df %>%
    dplyr::filter(Year >= start_plot_year) %>%
    dplyr::mutate(lower = NA, upper = NA, Type = "Actual") %>%
    dplyr::select(Year, GDP_Growth, lower, upper, Type)
  
  #Combine forecast data with known values
  country_df <- dplyr::bind_rows(plot_df, forecast_df) %>%
    dplyr::mutate(Country = country)
  
  all_data[[country]] <- country_df
}

#Combine all countries into one data frame
combined_df <- dplyr::bind_rows(all_data)

#Create subtitle with model information
model_subtitle <- paste(
  "China:", model_info[["China"]], "|",
  "US:", model_info[["United States"]],
)

#Plot (2000-2024, 2025-2035)
ggplot(combined_df, aes(x = Year, y = GDP_Growth, color = Country, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("China" = "red", "United States" = "blue")) +
  scale_fill_manual(values = c("China" = "red", "United States" = "blue")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Forecast" = "dashed")) +
  labs(title = "Real GDP Growth Forecast: 2000–2035 (Optimal ARIMA Models)",
       subtitle = model_subtitle,
       x = "Year", 
       y = "GDP Growth (%)", 
       color = "Country",
       linetype = "Data Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray30")) +
  scale_x_continuous(breaks = seq(2000, 2035, 5)) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))

#Print selected models
cat("\n=== Optimal ARIMA Models Selected ===\n")
cat("China:", model_info[["China"]], "\n")
cat("United States:", model_info[["United States"]], "\n")

#Output accuracy metrics
accuracy_table <- dplyr::bind_rows(accuracy_metrics)
cat("\n=== Forecast Accuracy Metrics (In-Sample) ===\n")
print(accuracy_table)