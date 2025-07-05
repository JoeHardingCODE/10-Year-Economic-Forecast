#── Load libraries ────────────────────────────────────────────────────────────────
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)     # for grid.arrange()

#── 1. Historical GDP data (2014–2024) ────────────────────────────────────────────
years <- 2014:2024
gdp_values <- c(
  2039.13, 2103.59, 2294.80, 2651.47, 2702.93,
  2835.61, 2674.85, 3167.27, 3353.47, 3567.55,
  3550.00
)
history_df <- tibble(Year = years, GDP = gdp_values) %>%
  arrange(Year) %>%
  mutate(
    Growth = 100 * (GDP / lag(GDP) - 1)
  )

#── 2. ETS forecast (2025–2030) ───────────────────────────────────────────────────
gdp_ts <- ts(gdp_values, start = 2014, frequency = 1)
ets_fc <- forecast(ets(gdp_ts), h = 6)
ets_df <- tibble(
  Year    = 2025:2030,
  GDP     = as.numeric(ets_fc$mean),
  Lower95 = as.numeric(ets_fc$lower[, "95%"]),
  Upper95 = as.numeric(ets_fc$upper[, "95%"]),
  Type    = "ETS Forecast"
)

#── 3. Exponential‐trend forecast ────────────────────────────────────────────────
exp_fit  <- lm(log(GDP) ~ Year, data = history_df)
future   <- tibble(Year = 2025:2030)
exp_pred <- predict(exp_fit, newdata = future, se.fit = TRUE)
exp_df   <- future %>%
  transmute(
    Year    = Year,
    GDP     = exp(exp_pred$fit),
    Lower95 = exp(exp_pred$fit - 1.96 * exp_pred$se.fit),
    Upper95 = exp(exp_pred$fit + 1.96 * exp_pred$se.fit),
    Type    = "Exp. Trend"
  )

#── 4. Fix continuity by prepending 2024 into each forecast series ────────────────
last_obs <- history_df %>%
  slice(n()) %>%
  transmute(
    Year    = Year,
    GDP     = GDP,
    Lower95 = NA_real_,
    Upper95 = NA_real_
  )

ets_df <- bind_rows(
  last_obs %>% mutate(Type = "ETS Forecast"),
  ets_df
)

exp_df <- bind_rows(
  last_obs %>% mutate(Type = "Exp. Trend"),
  exp_df
)

#── 5. Compute forecast growth rates ─────────────────────────────────────────────
growth_fc <- bind_rows(ets_df, exp_df) %>%
  group_by(Type) %>%
  arrange(Year) %>%
  mutate(Growth = 100 * (GDP / lag(GDP) - 1)) %>%
  ungroup()

#── 6. Prepare plot data ─────────────────────────────────────────────────────────
cap_df <- bind_rows(
  history_df %>%
    transmute(Year, GDP, Lower95 = NA_real_, Upper95 = NA_real_, Type = "Actual"),
  ets_df,
  exp_df
)

growth_df <- bind_rows(
  history_df %>%
    transmute(Year, Growth, Type = "Actual"),
  growth_fc %>%
    transmute(Year, Growth, Type)
)

#── 7a. Plot GDP levels ──────────────────────────────────────────────────────────
p1 <- ggplot(cap_df) +
  geom_ribbon(
    data = filter(cap_df, Type != "Actual"),
    aes(x = Year, ymin = Lower95, ymax = Upper95, fill = Type),
    alpha = 0.3, colour = NA
  ) +
  geom_line(
    aes(x = Year, y = GDP, colour = Type, linetype = Type),
    size = 1
  ) +
  scale_colour_manual(
    name   = "Series",
    values = c(Actual = "black", "ETS Forecast" = "blue", "Exp. Trend" = "red")
  ) +
  scale_fill_manual(
    values = c("ETS Forecast" = "lightblue", "Exp. Trend" = "mistyrose"),
    guide  = FALSE
  ) +
  scale_linetype_manual(
    name   = "Series",
    values = c(Actual = "solid", "ETS Forecast" = "solid", "Exp. Trend" = "dashed")
  ) +
  labs(
    title = "India’s Nominal GDP & Forecasts",
    y     = "GDP (US$ billion)",
    x     = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# …everything up to preparing growth_df remains the same…

# 7b. Plot year‐on‐year growth without warnings by dropping NAs
p2 <- ggplot(
  data = filter(growth_df, !is.na(Growth)),   # drop the NA rows here
  aes(x = Year, y = Growth, colour = Type, linetype = Type)
) +
  geom_line(size = 1) +
  scale_colour_manual(
    name   = "Series",
    values = c(Actual = "black", "ETS Forecast" = "blue", "Exp. Trend" = "red")
  ) +
  scale_linetype_manual(
    name   = "Series",
    values = c(Actual = "solid", "ETS Forecast" = "solid", "Exp. Trend" = "dashed")
  ) +
  labs(
    y = "Growth rate (%)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#── 8. Stack the two plots vertically ────────────────────────────────────────────
grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
