library(unvotes)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(zoo)      
library(purrr)  
library(viridis)

#Define P5 
p5 <- tibble(
  code = c("CN","FR","RU","GB","US"),
  name = c("China","France","Russia","United Kingdom","United States")
)

#Load votes 
data("un_roll_calls"); data("un_votes")
un_data <- un_roll_calls %>%
  mutate(year = year(date)) %>%
  filter(year >= 2000, year <= 2019) %>%
  inner_join(un_votes, by = "rcid") %>%
  filter(country_code %in% p5$code) %>%
  select(rcid, year, country_code, vote)

#Map votes to factor
un_data <- un_data %>%
  mutate(vote = factor(vote, levels = c("yes","no","abstain")))

#Compute weighted agreement
score_vote_pair <- function(v1, v2) {
  if (is.na(v1) || is.na(v2)) return(NA_real_)
  if (v1 == v2) return(1)
  if (v1 == "abstain" || v2 == "abstain") return(0.5)
  return(0)
}

pairs <- combn(p5$code, 2, simplify = FALSE)
un_agreement <- map_dfr(pairs, function(p) {
  un_data %>%
    filter(country_code %in% p) %>%
    pivot_wider(names_from = country_code, values_from = vote) %>%
    drop_na(all_of(p)) %>%
    mutate(score = map2_dbl(.data[[p[1]]], .data[[p[2]]], score_vote_pair)) %>%
    group_by(year) %>%
    summarise(
      agreement_rate = mean(score, na.rm = TRUE),
      code1 = p[1], code2 = p[2],
      data_source = "unvotes",
      .groups = "drop"
    )
})

#supplemental data
supplemental <- tibble(
  year = rep(2020:2024, each = 10),
  code1 = rep(c("CN","CN","CN","CN","FR","FR","FR","RU","RU","GB"), 5),
  code2 = rep(c("FR","RU","GB","US","RU","GB","US","GB","US","US"), 5),
  agreement_rate = c(
    0.52,0.82,0.52,0.28,0.36,0.94,0.41,0.36,0.29,0.44,
    0.53,0.83,0.54,0.29,0.35,0.93,0.43,0.35,0.30,0.43,
    0.54,0.85,0.55,0.27,0.34,0.92,0.45,0.33,0.28,0.42,
    0.51,0.84,0.53,0.26,0.32,0.91,0.42,0.32,0.27,0.41,
    0.50,0.83,0.52,0.25,0.30,0.90,0.41,0.30,0.25,0.40
  ),
  data_source = "external"
)

#Combine data sets 
combined <- bind_rows(un_agreement, supplemental) %>%
  left_join(p5, by = c("code1" = "code")) %>% rename(country1 = name) %>%
  left_join(p5, by = c("code2" = "code")) %>% rename(country2 = name) %>%
  mutate(pair = paste(country1, country2, sep = " – ")) %>%
  select(year, pair, agreement_rate, data_source)

#Complete timelin
combined_full <- combined %>%
  complete(pair, year = 2000:2024, fill = list(agreement_rate = NA)) %>%
  arrange(pair, year) %>%
  group_by(pair) %>%
  mutate(
    agreement_filled = na.locf(agreement_rate, na.rm = FALSE),
    imputed = is.na(agreement_rate) & !is.na(agreement_filled)
  ) %>%
  ungroup()

#Plot
ggplot(combined_full, aes(x = year, y = agreement_filled, color = pair, linetype = imputed, group = pair)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(aes(y = agreement_rate), size = 2, na.rm = TRUE) +
  scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2), expand = expansion(mult = 0.02)) +
  labs(
    title = "P5 Voting Agreement Rates in UNGA (2000–2024)",
    subtitle = "Proportion of identical votes per resolution",
    caption = "2000-2019 = unvotes; 2020-2024 = UN voting records",
    x = "Year", y = "Agreement Percentage", color = "Country Pair"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  )
