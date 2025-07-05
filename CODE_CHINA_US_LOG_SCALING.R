library(WDI)        # to fetch data from the World Bank
library(forecast)   # ETS modeling & forecasting
library(ggplot2)    # plotting
library(dplyr)      # data wrangling
library(tidyr)      # reshaping

#── 1. Fetch nominal GDP (current US$) for US & China, 2014–2024 ─────────────────
gdp_raw <- WDI(
  country    = c("US","CN"),
  indicator  = "NY.GDP.MKTP.CD",
  start      = 2014,
  end        = 2024,
  extra      = FALSE,    # no extra metadata
  cache      = NULL
)

# Clean up and reshape
gdp_df <- gdp_raw %>%
  rename(GDP = NY.GDP.MKTP.CD) %>%
  select(iso2c, year, GDP) %>%
  pivot_wider(names_from = iso2c, values_from = GDP) %>%
  arrange(year) %>%
  rename(
    Year    = year,
    USA     = US,
    China   = CN
  )

#── 2. Build time‐series objects ────────────────────────────────────────────────
# Note: start=2014, frequency=1 for annual data
ts_USA   <- ts(gdp_df$USA,   start = 2014, frequency = 1)
ts_China <- ts(gdp_df$China, start = 2014, frequency = 1)

#── 3. Fit ETS models ───────────────────────────────────────────────────────────
ets_USA   <- ets(ts_USA)
ets_China <- ets(ts_China)

#── 4. Forecast 11 years ahead (2025–2035) ──────────────────────────────────────
horizon <- 11
fc_USA   <- forecast(ets_USA,   h = horizon, level = 95)
fc_China <- forecast(ets_China, h = horizon, level = 95)

#── 5. Prepare a combined data frame for plotting ───────────────────────────────
# Historical
hist_plot <- gdp_df %>%
  select(Year, USA, China) %>%
  pivot_longer(-Year, names_to = "Country", values_to = "GDP") %>%
  mutate(Type = "Actual")

# Forecast
years_fc <- 2025:(2024 + horizon)
fc_plot <- tibble(
  Year       = rep(years_fc, 2),
  Country    = rep(c("USA","China"), each = horizon),
  GDP        = c(as.numeric(fc_USA$mean),   as.numeric(fc_China$mean)),
  Lower95    = c(as.numeric(fc_USA$lower),  as.numeric(fc_China$lower)),
  Upper95    = c(as.numeric(fc_USA$upper),  as.numeric(fc_China$upper)),
  Type       = "Forecast"
)

# Combine and mark intervals only for Forecast
plot_df <- bind_rows(
  hist_plot %>% mutate(Lower95 = NA, Upper95 = NA),
  fc_plot
)

# … everything up through building plot_df stays the same …

# 6. Plot both series with ETS forecasts & 95% bands, on a log10 scale
library(scales)  # for pretty comma labels, optional

ggplot(plot_df, aes(x = Year, y = GDP, colour = Country, fill = Country)) +
  geom_ribbon(
    data = filter(plot_df, Type == "Forecast"),
    aes(ymin = Lower95, ymax = Upper95),
    alpha = 0.2, colour = NA
  ) +
  geom_line(aes(linetype = Type), size = 1) +
  scale_colour_manual(values = c(USA = "steelblue", China = "firebrick")) +
  scale_fill_manual(values = c(USA = "steelblue", China = "firebrick"), guide = FALSE) +
  scale_linetype_manual(values = c(Actual = "solid", Forecast = "dashed"), name = "") +
  scale_y_log10(
    labels = scales::comma_format(),
    breaks = c(1e12, 2e12, 4e12, 8e12)   # removed the 1.6e13 break
  ) +
  labs(
    title    = "ETS Forecast of Nominal GDP: US vs. China (Log Scale)",
    subtitle = "Historical (2014–2024) and Forecasted (2025–2035) GDP",
    x        = "Year",
    y        = "GDP (current US$, log10 scale)",
    colour   = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
