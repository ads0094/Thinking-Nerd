library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

vic_root <- file.path("..", "VIC-Electricity-Demand-Forecasting")
out_root <- "images"

demand <- read_csv(
  file.path(vic_root, "data", "processed", "vic_scheduled_demand_half_hourly.csv"),
  col_types = cols(interval_end_nem_time = col_character(), .default = col_guess()),
  show_col_types = FALSE
) |>
  mutate(
    interval_end = ymd_hms(interval_end_nem_time, tz = "Australia/Brisbane"),
    interval_start = interval_end - minutes(30),
    local_start = with_tz(interval_start, "Australia/Melbourne"),
    half_hour = hour(local_start) + minute(local_start) / 60,
    day_type = if_else(wday(local_start, week_start = 1) <= 5, "Weekday", "Weekend"),
    weather_hour = floor_date(interval_start, "hour")
  )

weather <- read_csv(
  file.path(vic_root, "data", "processed", "melbourne_weather_hourly.csv"),
  col_types = cols(time_nem = col_character(), .default = col_guess()),
  show_col_types = FALSE
) |>
  mutate(weather_hour = ymd_hm(time_nem, tz = "Australia/Brisbane")) |>
  select(weather_hour, temperature_2m)

demand <- left_join(demand, weather, by = "weather_hour")

base_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", colour = "#002244", size = 20, margin = margin(b = 14)),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(colour = "#263238"),
    plot.margin = margin(20, 25, 20, 20)
  )

profile <- demand |>
  group_by(day_type, half_hour) |>
  summarise(demand_mw = mean(scheduled_demand_mw), .groups = "drop")

p1 <- ggplot(profile, aes(half_hour, demand_mw, colour = day_type)) +
  geom_line(linewidth = 1.25) +
  scale_colour_manual(values = c(Weekday = "#1767a6", Weekend = "#df741b")) +
  scale_x_continuous(breaks = seq(0, 24, 3), labels = function(x) sprintf("%02d:00", x)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Weekday demand rises more sharply in the morning",
    x = "Time of day",
    y = "Average demand (MW)"
  ) +
  base_theme

ggsave(
  file.path(out_root, "vic-demand-weekday-profile.png"),
  p1,
  width = 10,
  height = 6.2,
  dpi = 150,
  bg = "white"
)

temp_profile <- demand |>
  filter(!is.na(temperature_2m)) |>
  mutate(temperature_band = floor(temperature_2m)) |>
  filter(temperature_band >= 0, temperature_band <= 40) |>
  group_by(temperature_band) |>
  summarise(demand_mw = mean(scheduled_demand_mw), observations = n(), .groups = "drop") |>
  filter(observations >= 48)

p2 <- ggplot(temp_profile, aes(temperature_band, demand_mw)) +
  geom_line(colour = "#1767a6", linewidth = 1.25) +
  geom_point(colour = "#df741b", size = 2) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Demand rises in both cold and hot conditions",
    x = "Central Melbourne temperature (°C)",
    y = "Average demand (MW)"
  ) +
  base_theme

ggsave(
  file.path(out_root, "vic-demand-temperature.png"),
  p2,
  width = 10,
  height = 6.2,
  dpi = 150,
  bg = "white"
)

models <- tibble::tribble(
  ~horizon, ~model, ~mae,
  "24 hours", "Previous day", 507,
  "24 hours", "Previous week", 603,
  "24 hours", "Random forest", 333,
  "7 days", "Previous day", 628,
  "7 days", "Previous week", 604,
  "7 days", "Random forest", 333
) |>
  mutate(model = factor(model, levels = c("Previous day", "Previous week", "Random forest")))

p3 <- ggplot(models, aes(model, mae, fill = model)) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(aes(label = paste0(mae, " MW")), vjust = -0.45, colour = "#263238", size = 4.5) +
  facet_wrap(~horizon) +
  scale_fill_manual(
    values = c("Previous day" = "#9aa6ad", "Previous week" = "#536f80", "Random forest" = "#1767a6")
  ) +
  scale_y_continuous(labels = label_comma(), limits = c(0, 700), expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = "Weather and calendar features improve the forecast",
    x = NULL,
    y = "MAE (MW)"
  ) +
  base_theme +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))

ggsave(
  file.path(out_root, "vic-demand-model-comparison.png"),
  p3,
  width = 10,
  height = 6.2,
  dpi = 150,
  bg = "white"
)
