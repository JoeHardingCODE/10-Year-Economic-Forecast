library(dplyr)
library(ggplot2)

set.seed(123)

#Simulation Parameters
n_sims  <- 1000
years   <- 2025:2035
n_years <- length(years)

baseline_mean <- 0.03
baseline_sd   <- 0.025

#Define Country Type Parameters (with export/resilience bounds)
type_params <- tibble(
  type               = c("SE–ME", "SE–MI", "SI–ME", "SI–MI"),
  
  #Elasticity for each group
  base_elasticity    = c(0.35, 0.50, 0.70, 1.00),
  
  # Service resilience bounds (lower = more resilient)
  resilience_lo      = c(0.2, 0.3, 0.5, 0.7),
  resilience_hi      = c(0.4, 0.5, 0.7, 0.9),
  
  #Shock probabilities
  shock_base_prob    = c(0.15, 0.25, 0.35, 0.45),
  shock_inc          = rep(0.15, 4),
  
  #Merchandise share bounds
  merch_lo           = c(0.35, 0.25, 0.65, 0.75),
  merch_hi           = c(0.50, 0.35, 0.75, 0.90)
)

#Create  data frame
results <- expand.grid(
  sim   = 1:n_sims,
  year  = years,
  type  = type_params$type
) %>% arrange(type, sim, year)

results$volume              <- NA_real_
results$shock_flag          <- FALSE
results$share_merchandise   <- NA_real_
results$share_services      <- NA_real_
results$service_resilience  <- NA_real_

#Run simulation
for (tp in seq_len(nrow(type_params))) {
  params <- type_params[tp, ]
  
  for (i in 1:n_sims) {
    vol <- 100.0
    current_prob <- params$shock_base_prob
    
    #Randomize export composition & service resilience for this simulation
    share_merchandise   <- runif(1, params$merch_lo, params$merch_hi)
    share_services      <- 1 - share_merchandise
    service_resilience  <- runif(1, params$resilience_lo, params$resilience_hi)
    
    for (y in seq_len(n_years)) {
      is_shock <- runif(1) < current_prob
      
      if (is_shock) {
        shock_multiplier <- params$base_elasticity * (
          share_merchandise + service_resilience * share_services
        )
        shock_impact <- rnorm(1, mean = -shock_multiplier * 0.05,
                              sd   = shock_multiplier * 0.05)
        growth <- shock_impact
        current_prob <- min(current_prob + params$shock_inc, 1.0)
      } else {
        growth <- rnorm(1, mean = baseline_mean, sd = baseline_sd)
        current_prob <- params$shock_base_prob
      }
      
      vol <- vol * (1 + growth)
      
      idx <- with(results, sim == i & year == years[y] & type == params$type)
      results$volume[idx]             <- vol
      results$shock_flag[idx]         <- is_shock
      results$share_merchandise[idx]  <- share_merchandise
      results$share_services[idx]     <- share_services
      results$service_resilience[idx] <- service_resilience
    }
  }
}

#Summarize by year and type
summary_df <- results %>%
  group_by(type, year) %>%
  summarize(
    median = median(volume),
    p10    = quantile(volume, 0.10),
    p90    = quantile(volume, 0.90),
    .groups = "drop"
  )

#Plot median + bands
ggplot(summary_df, aes(x = year, y = median, fill = type, color = type)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.15) +
  geom_line(size = 1) +
  facet_wrap(~ type, ncol = 2) +
  labs(
    title = "Trade Volume Index by Country Profile (2025–2035)",
    subtitle = "Randomized Export Composition and Service Resilience",
    x = "Year", y = "Volume Index (base = 100)"
  ) +
  theme_minimal()

#Final-year trade volumes
final_vol <- results %>% filter(year == max(years))

ggplot(final_vol, aes(x = volume, fill = type)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ type, ncol = 2) +
  labs(
    title = "Distribution of Trade Volume Index in 2035 by Country Profile",
    x = "Trade Volume Index", y = "Density"
  ) +
  theme_light()

#Final summary stats (including average mix and resilience)
stats_final <- final_vol %>%
  group_by(type) %>%
  summarize(
    mean_volume           = mean(volume),
    median_volume         = median(volume),
    sd_volume             = sd(volume),
    p5_volume             = quantile(volume, 0.05),
    p95_volume            = quantile(volume, 0.95),
    prob_below100         = mean(volume < 100),
    avg_merch_share       = mean(share_merchandise),
    avg_service_share     = mean(share_services),
    avg_service_resilience= mean(service_resilience),
    .groups = "drop"
  )

print(stats_final)