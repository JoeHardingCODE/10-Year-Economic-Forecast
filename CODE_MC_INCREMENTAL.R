#Monte Carlo Simulation of global trade volume growth (2025–2035)
#with incremental increase in shock probability (+5%) for each successive protectionist year

set.seed(123)

# 1. Simulation parameters
n_sims  <- 5000
years   <- 2025:2035
n_years <- length(years)

#Growth assumptions (from WTO long run estimates)
baseline_mean <- 0.03   # 3% baseline growth
baseline_sd   <- 0.025  # 2.5% baseline sd

#Shock assumptions (incremental path-dependency)
shock_base_prob      <- 0.25  # initial 10% chance of shock
shock_increment      <- 0.15  # increase shock probability by 15ppt after each shock year
shock_mean           <- -0.05 # -5% mean growth in shock year
shock_sd             <- 0.05  # 5% sd in shock year

#Prepare results
results <- expand.grid(sim = 1:n_sims, year = years)
results$volume <- NA   #cumulative volume index (base=100)
results$shock_flag  <- NA   #track whether year was a shock

#Run simulations with incremental shock probability
for (i in 1:n_sims) {
  vol             <- 100.0           # starting index
  current_prob    <- shock_base_prob # start with base probability
  
  for (y in seq_len(n_years)) {
    #Determine if a shock occurs this year
    is_shock <- (runif(1) < current_prob)
    
    #Draw protectionist probability based on current year status
    if (is_shock) {
      growth <- rnorm(1, mean = shock_mean, sd = shock_sd)
      # increase probability for next year, capped at 1.0
      current_prob <- min(current_prob + shock_increment, 1.0)
    } else {
      growth <- rnorm(1, mean = baseline_mean, sd = baseline_sd)
      # reset probability to base after a non-shock year
      current_prob <- shock_base_prob
    }
    
    # Update and record volume and shock probability
    vol <- vol * (1 + growth)
    results$volume[results$sim == i & results$year == years[y]] <- vol
    results$shock_flag[results$sim == i & results$year == years[y]] <- is_shock
  }
}

#Summarize and plot results
library(dplyr)
library(ggplot2)

#Median and percentile bands by year
summary_df <- results %>%
  group_by(year) %>%
  summarize(
    median = median(volume),
    p10    = quantile(volume, 0.10),
    p90    = quantile(volume, 0.90)
  )

#Time-series plot: median with 10th/90th percentile bands
ggplot(summary_df, aes(x = year)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = median), color = "blue", size = 1) +
  labs(
    title = "Global Trade Volume (Index, base=100)\nIncremental Protectionism Shock Probability",
    y = "Volume Index", x = "Year"
  ) +
  theme_minimal()

# Density of final-year volume (2035)
final_vol <- results %>% filter(year == max(years))
ggplot(final_vol, aes(x = volume)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(
    title = sprintf("Distribution of Merchandise Trade Volume Index in %d", max(years)),
    x = sprintf("Volume Index (%d)", max(years)), y = "Density"
  ) +
  theme_light()

# Summary statistics for final-year volume
stats_final <- final_vol %>% summarize(
  mean          = mean(volume),
  median        = median(volume),
  sd            = sd(volume),
  p5           = quantile(volume, 0.05),
  p95           = quantile(volume, 0.95),
  prob_below100 = mean(volume < 100)
)
print(stats_final) 
