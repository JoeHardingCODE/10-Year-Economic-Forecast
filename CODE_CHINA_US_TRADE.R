
library(imf.data)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

DOT <- load_datasets("DOT")

pull_dots_q <- function(reporter, partner) {
  df <- DOT$get_series(
    freq = "Q",
    ref_area = reporter,
    indicator = c("TXG_FOB_USD", "TMG_CIF_USD"),
    counterpart_area = partner,
    start_period = "2016-01-01",
    end_period = "2025-03-31"
  )
  cols <- names(df)
  exp_col <- cols[grepl("TXG_FOB_USD", cols)]
  imp_col <- cols[grepl("TMG_CIF_USD", cols)]
  df2 <- df %>%
    select(TIME_PERIOD, all_of(exp_col), all_of(imp_col)) %>%
    rename(export = all_of(exp_col), import = all_of(imp_col)) %>%
    mutate(
      export = as.numeric(export),
      import = as.numeric(import),
      total_trade = export + import,
      date = ymd(paste0(TIME_PERIOD, "-01")),
      reporter = reporter,
      partner = partner
    ) %>%
    select(date, reporter, partner, total_trade)
  df2
}

#Retrieve data
us_cn <- pull_dots_q("US", "CN")
cn_us <- pull_dots_q("CN", "US")
us_wld <- pull_dots_q("US", "W00")
cn_wld <- pull_dots_q("CN", "W00")

#Compute trade series
bilateral <- bind_rows(us_cn, cn_us) %>%
  group_by(date) %>%
  summarize(bilateral_trade = mean(total_trade, na.rm = TRUE), .groups = "drop")

trade_others <- bind_rows(us_wld, cn_wld) %>%
  left_join(
    bind_rows(us_cn, cn_us) %>%
      group_by(date, reporter) %>%
      summarize(bilateral = mean(total_trade), .groups = "drop"),
    by = c("date","reporter")
  ) %>%
  mutate(
    trade_others = total_trade - bilateral,
    series = if_else(reporter == "US", "U.S. trade w/ other economies", "China trade w/ other economies")
  ) %>%
  select(date, series, value = trade_others)

final_data <- bind_rows(
  bilateral %>% mutate(series = "US–China trade", value = bilateral_trade),
  trade_others
)

base_vals <- final_data %>%
  filter(year(date) == 2018) %>%
  group_by(series) %>%
  summarize(base = mean(value, na.rm = TRUE), .groups = "drop")

plot_data <- final_data %>%
  left_join(base_vals, by = "series") %>%
  mutate(index = 100 * value / base)

events <- tibble(
  start = ymd(c("2018-07-01","2020-02-01","2022-02-24")),
  end   = ymd(c("2018-10-01","2020-05-01","2022-06-01")),
  label = c("Trade tensions","COVID19","Ukraine war")
)

#plot data and events
ggplot(plot_data, aes(x = date, y = index, color = series)) +
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey",
    alpha = 0.3
  ) +
  geom_point(size = 0.9, alpha = 0.6) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_text(
    data = events,
    aes(x = start, y = 180, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3
  ) +
  scale_color_manual(values = c(
    "US–China trade"         = "purple",
    "U.S. trade w/ other economies"   = "blue",
    "China trade w/ other economies"  = "red"
  )) +
  scale_y_continuous(limits = c(50, 180), breaks = seq(50, 180, 25)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Polynomial-Smoothed Indexed Quarterly Trade: China & U.S.",
    subtitle = "Bilateral and with Others (2016–2024, 2018 Average = 100)",
    x        = NULL, 
    y        = "Trade Index (2018 avg = 100)",
    caption  = "Source: IMF via imf.data"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "bottom",
    axis.text.x       = element_text(size = 8, hjust = 0.5),
    plot.title        = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle     = element_text(hjust = 0.5)
  )

