# Load necessary libraries
library(tidyverse)
library(fixest)       
library(modelsummary) 
library(ggplot2)
library(MASS)         

#Load the DATASET
file_path <- "C:/Users/joeha/Documents/Bridgewater/Gravity_V202211.rds"
gravity_data <- readRDS(file_path)

#Filter and log-transform data
gravity_filtered <- gravity_data %>%
  filter(!is.na(tradeflow_baci), tradeflow_baci > 0,
         !is.na(gdp_ppp_pwt_o), !is.na(gdp_ppp_pwt_d),
         !is.na(pop_pwt_o), !is.na(pop_pwt_d),
         !is.na(dist)) %>%
  mutate(
    log_trade   = log(tradeflow_baci),
    log_gdp_o   = log(gdp_ppp_pwt_o),
    log_gdp_d   = log(gdp_ppp_pwt_d),
    log_pop_o   = log(pop_pwt_o),
    log_pop_d   = log(pop_pwt_d),
    log_dist    = log(dist)
  )

#Estimate beta values using a PPML method
gravity_model <- feglm(
  tradeflow_baci ~ log_gdp_o + log_gdp_d + log_pop_o + log_pop_d + log_dist |
    iso3_o + iso3_d,
  data   = gravity_filtered,
  family = poisson(link = "log"),
  cluster = ~iso3_o + iso3_d
)

#Create a summary table
gravity_summary <- modelsummary(gravity_model, output = "data.frame")
print(gravity_summary)

#Extract estimates and covariance
coefs <- coef(gravity_model)
vcov_mat <- vcov(gravity_model)

#Compute means of logged datapoints
mean_log_gdp_o <- mean(gravity_filtered$log_gdp_o)
mean_log_gdp_d <- mean(gravity_filtered$log_gdp_d)
mean_log_pop_o <- mean(gravity_filtered$log_pop_o)
mean_log_pop_d <- mean(gravity_filtered$log_pop_d)
mean_log_dist  <- mean(gravity_filtered$log_dist)

#Baseline index = log-trade identities
baseline_log_trade <- 
  coefs["log_gdp_o"] * mean_log_gdp_o +
  coefs["log_gdp_d"] * mean_log_gdp_d +
  coefs["log_pop_o"] * mean_log_pop_o +
  coefs["log_pop_d"] * mean_log_pop_d +
  coefs["log_dist"]  * mean_log_dist

#Baseline expected trade in levels
baseline_trade_level <- exp(baseline_log_trade)


# Monte Carlo simulation of world trade index over next 10 years

#Establish running parameters  
set.seed(123)  
n_years <- 10
n_sim   <- 10000

#Establishing shock/growth parameters
shock_prob   <- 0.4
growth_prob  <- 1 - shock_prob
shock_mean   <- -0.06
shock_sd     <- 0.05
growth_mean  <-  0.03
growth_sd    <- 0.02

#Prepare matrix
sim_matrix <- matrix(NA, nrow = n_sim, ncol = n_years)

#Defining parameter uncertainty (as part of the stochastic method)
for (sim in 1:n_sim) {
beta_sim <- mvrnorm(1, mu = coefs, Sigma = vcov_mat)
baseline_sim_log <- sum(beta_sim * c(mean_log_gdp_o, mean_log_gdp_d,
                                     mean_log_pop_o, mean_log_pop_d,
                                     mean_log_dist))
baseline_trade <- exp(baseline_sim_log)
  
#PPML‐calibrated baseline level
trade_index <- baseline_trade_level
  
#Apply shocks/growths (simple multiplication) --> taken from normal distributions 
for (year in 1:n_years) {
    event_type <- sample(c("shock", "growth"), 1, prob = c(shock_prob, growth_prob))
    impact <- if (event_type == "shock") {
      rnorm(1, mean = shock_mean, sd = shock_sd)
    } else {
      rnorm(1, mean = growth_mean, sd = growth_sd)
    }
    trade_index <- trade_index * exp(impact)
    sim_matrix[sim, year] <- trade_index
  }
}

#Convert to index form
sim_df <- as.data.frame(sim_matrix) %>%
  mutate(sim_id = 1:n_sim) %>%
  pivot_longer(cols = starts_with("V"), names_to = "year", values_to = "trade_level") %>%
  mutate(
    year        = as.integer(str_remove(year, "V")),
    trade_index = trade_level / baseline_trade_level * 100
  )

#Calculate summary statistics
summary_df <- sim_df %>%
  group_by(year) %>%
  summarise(
    mean_index   = mean(trade_index),
    median_index = median(trade_index),
    lower_95     = quantile(trade_index, 0.025),
    upper_95     = quantile(trade_index, 0.975)
  )

#Density plot 
final_year_data <- sim_df %>% filter(year == n_years)

ggplot(final_year_data, aes(x = trade_index)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(trade_index)), linetype = "dashed", size = 1, color = "red") +
  labs(
    title    = paste("Simulated Distribution of World Trade Index by 2035"),
    subtitle = "Baseline Calibrated to PPML Gravity Estimates",
    x        = "Trade Index (Base Year = 100)",
    y        = "Density"
  ) +
  theme_minimal()
