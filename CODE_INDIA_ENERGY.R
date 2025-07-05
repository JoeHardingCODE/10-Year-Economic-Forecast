#Input historical capacity data (2014–2024)
library(forecast); library(ggplot2); library(dplyr); library(tidyr)

years <- 2014:2024
capacity_values <- c(
  35849.59, 39950.09, 47091.39, 58558.20, 70651.44,
  79412.00, 88255.31, 95803.42, 109885.39, 125159.82,
  143644.50
)
history_df <- tibble(Year = years, Capacity = capacity_values)

#ETS forecast (2025–2030) --> converts the vector above into a time series object beggining in 2014
#ETS forecasting = Exponential Smoothing predictions 
#subject to 80 and 95 percent prediction intervals 
capacity_ts <- ts(capacity_values, start = 2014, frequency = 1)
ets_fc <- forecast(ets(capacity_ts), h = 6)
ets_df <- tibble(
  Year    = 2025:2030,
  Forecast = as.numeric(ets_fc$mean),
  Lower95  = as.numeric(ets_fc$lower[, "95%"]),
  Upper95  = as.numeric(ets_fc$upper[, "95%"]),
  Type     = "ETS Forecast"
)

#Exponential‐trend forecast --> regress log(capacity) on by year which assumes
#year-on-year percentage growth is constant between years 
#predict(.....) gives the log-predicted values and their SE 
#transmute function gives the 95% confidence interval assuming normal Z distribution of Wattage
exp_fit   <- lm(log(Capacity) ~ Year, data = history_df)
future_df <- tibble(Year = 2025:2030)
exp_pred  <- predict(exp_fit, newdata = future_df, se.fit = TRUE)
exp_df    <- future_df %>%
  transmute(
    Year     = Year,
    Forecast = exp(exp_pred$fit),
    Lower95  = exp(exp_pred$fit - 1.96 * exp_pred$se.fit),
    Upper95  = exp(exp_pred$fit + 1.96 * exp_pred$se.fit),
    Type     = "Exp. Trend"
  )

#Historical “series” as pseudo‐forecast (no ribbon)
hist_df <- history_df %>%
  transmute(
    Year     = Year,
    Forecast = Capacity,
    Lower95  = NA_real_,
    Upper95  = NA_real_,
    Type     = "Actual"
  )
plot_df <- bind_rows(hist_df, ets_df, exp_df)

#solving discontinutity in the graph - without this graph ends at start of 2024 and 
#starts at the start of 2025 leaving a uear long discontinuity
last_hist <- history_df %>%
  filter(Year == max(Year)) %>%
  transmute(
    Year     = Year,
    Forecast = Capacity,
    Lower95  = NA_real_,
    Upper95  = NA_real_,
    Type     = "ETS Forecast"
  )
ets_df <- bind_rows(
  last_hist,
  ets_df
)
last_hist_exp <- last_hist %>% mutate(Type = "Exp. Trend")
exp_df <- bind_rows(
  last_hist_exp,
  exp_df
)
#put three series together
plot_df <- bind_rows(
  history_df %>%
    transmute(
      Year     = Year,
      Forecast = Capacity,
      Lower95  = NA_real_,
      Upper95  = NA_real_,
      Type     = "Actual"
    ),
  ets_df,
  exp_df
)

#plot
ggplot() +
  geom_ribbon(
    data = filter(plot_df, Type != "Actual"),
    aes(x = Year, ymin = Lower95, ymax = Upper95, fill = Type),
    alpha = 0.3, colour = NA
  ) +
  geom_line(
    data = plot_df,
    aes(x = Year, y = Forecast, colour = Type, linetype = Type, group = Type),
    size = 1
  ) +
  scale_colour_manual(
    name   = "Series",
    values = c(
      "Actual"        = "black",
      "ETS Forecast"  = "blue",
      "Exp. Trend"    = "red"
    )
  ) +
  scale_fill_manual(
    values = c(
      "ETS Forecast" = "purple",
      "Exp. Trend"   = "orange"
    ),
    guide = FALSE
  ) +
  scale_linetype_manual(
    name   = "Series",
    values = c(
      "Actual"        = "solid",
      "ETS Forecast"  = "solid",
      "Exp. Trend"    = "dashed"
    )
  ) +
  labs(
    title    = "Forecast of India’s Non-Hydro Renewable Capacity (MW)",
    subtitle = "2014–2024 actual and 2025–2030 forecasts (ETS vs. exponential trend)",
    x        = "Year",
    y        = "Capacity (MW)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))
