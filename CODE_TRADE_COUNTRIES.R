library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

set.seed(123)

#Country-Specific Parameter
country_params <- tibble(
  country = c("Germany", "India", "China", "Egypt"),
  merch_lo = c(0.45, 0.20, 0.70, 0.75),
  merch_hi = c(0.60, 0.40, 0.85, 0.95),
  resilience_lo = c(0.25, 0.15, 0.55, 0.65),
  resilience_hi = c(0.45, 0.35, 0.75, 0.85),
  base_elasticity = c(0.8, 1.2, 1.5, 1.8),
  shock_base_prob = c(0.18, 0.28, 0.38, 0.48),
  shock_inc = c(0.12, 0.12, 0.15, 0.18),
  # Economic vulnerability labels
  vulnerability = c("Manufacturing Services Hub", 
                    "Digital Services Exporter",
                    "Global Manufacturing Base",
                    "Commodity Import Dependent")
)

#Simulation 
n_sims <- 5000
years <- 2025:2035
results <- expand.grid(sim = 1:n_sims, year = years, country = country_params$country) %>%
  arrange(country, sim, year) %>%
  left_join(country_params, by = "country") %>%
  mutate(
    volume = NA_real_,
    shock_flag = FALSE,
    share_merch = NA_real_,
    service_resil = NA_real_,
    shock_mult = NA_real_
  )

#Simulation Loop
for (c in unique(results$country)) {
  params <- filter(country_params, country == c)
  
  for (i in 1:n_sims) {
    vol <- 100.0
    current_prob <- params$shock_base_prob
    share_merch <- runif(1, params$merch_lo, params$merch_hi)
    serv_resil <- runif(1, params$resilience_lo, params$resilience_hi)
    shock_mult <- params$base_elasticity * (share_merch + serv_resil * (1 - share_merch))
    
    for (y in seq_along(years)) {
      idx <- which(results$country == c & results$sim == i & results$year == years[y])
      
      is_shock <- runif(1) < current_prob
      if (is_shock) {
        growth <- rnorm(1, mean = -0.07 * shock_mult, sd = shock_mult * 0.015)
        current_prob <- min(current_prob + params$shock_inc, 1.0)
      } else {
        growth <- rnorm(1, mean = 0.03, sd = 0.01)
        current_prob <- params$shock_base_prob
      }
      
      vol <- vol * (1 + growth)
      results$volume[idx] <- vol
      results$shock_flag[idx] <- is_shock
      results$share_merch[idx] <- share_merch
      results$service_resil[idx] <- serv_resil
      results$shock_mult[idx] <- shock_mult
    }
  }
}

#Time Series Plot
summary_ts <- results %>%
  group_by(country, year, vulnerability) %>%
  summarize(
    median_vol = median(volume),
    p25 = quantile(volume, 0.25),
    p75 = quantile(volume, 0.75),
    .groups = "drop"
  )

ggplot(summary_ts, aes(x = year, y = median_vol, color = vulnerability)) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = vulnerability), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  facet_wrap(~ country, nrow = 2) +
  scale_y_continuous(limits = c(70, 120), breaks = seq(80, 120, 10)) +
  labs(
    title = "Trade Volume Projections Under Rising Protectionism (2025-2035)",
    subtitle = "Shaded regions: 25th-75th percentile ranges | Dashed line: 2025 baseline",
    x = "Year", 
    y = "Trade Volume Index (2025 = 100)",
    caption = "Sources: WTO structural profiles, Global Trade Alert database, UNCTAD forecasts"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

