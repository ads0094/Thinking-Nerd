suppressPackageStartupMessages({
  library(fpp3)
  library(purrr)
  library(scales)
})

output_dir <- "images"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

navy <- "#092c47"
teal <- "#487b87"
teal_dark <- "#285d69"
teal_light <- "#c7dadd"
orange <- "#c8892d"
ink <- "#24333d"
muted <- "#65737b"
paper <- "#f7f4ed"

article_theme <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "Segoe UI") +
    theme(
      plot.background = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      plot.title = element_text(
        family = "Georgia", colour = navy, size = 20,
        face = "bold", margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        colour = muted, size = 11.5, lineheight = 1.15,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        colour = muted, size = 9.5, hjust = 0,
        margin = margin(t = 13)
      ),
      axis.title = element_text(colour = ink, size = 11),
      axis.text = element_text(colour = muted, size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#dfe3df", linewidth = 0.45),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(colour = ink, size = 10),
      plot.margin = margin(24, 28, 22, 24)
    )
}

save_article_plot <- function(plot, filename, width = 10, height = 6.2) {
  ggsave(
    filename = file.path(output_dir, filename), plot = plot,
    width = width, height = height, dpi = 180, bg = paper
  )
}

distribution_quantile <- function(x, probability) {
  as.numeric(stats::quantile(x, p = probability))
}

# -----------------------------------------------------------------------------
# Main example: monthly Victorian food retailing turnover
# -----------------------------------------------------------------------------

retail <- aus_retail |>
  filter(State == "Victoria", Industry == "Food retailing") |>
  select(Month, Turnover)

stopifnot(
  nrow(retail) == 441L,
  nrow(scan_gaps(retail)) == 0L,
  !anyNA(retail$Turnover),
  min(retail$Month) == yearmonth("1982 Apr"),
  max(retail$Month) == yearmonth("2018 Dec")
)

# Origins run every two months from December 2013 through December 2017. This
# gives 25 complete rolling origins, each with twelve observable future months
# through 2018, and keeps the analysis practical to reproduce on a laptop.
origin_months <- retail |>
  filter(Month >= yearmonth("2013 Dec"), Month <= yearmonth("2017 Dec")) |>
  slice(seq(1, n(), by = 2)) |>
  pull(Month)

forecast_from_origin <- function(origin_position) {
  origin <- origin_months[origin_position]
  training <- retail |> filter(Month <= origin)

  fitted <- training |>
    model(
      `Seasonal naive` = SNAIVE(Turnover),
      ETS = ETS(log(Turnover)),
      ARIMA = ARIMA(
        log(Turnover) ~ pdq(0, 1, 1) + PDQ(0, 1, 1)
      )
    )

  fitted |>
    forecast(h = 12) |>
    as_tibble() |>
    transmute(
      origin = origin,
      Month,
      model = .model,
      point = .mean,
      lower80 = distribution_quantile(Turnover, 0.10),
      upper80 = distribution_quantile(Turnover, 0.90),
      lower95 = distribution_quantile(Turnover, 0.025),
      upper95 = distribution_quantile(Turnover, 0.975)
    )
}

retail_forecasts <- map_dfr(seq_along(origin_months), forecast_from_origin) |>
  mutate(
    horizon = (year(Month) - year(origin)) * 12L +
      (month(Month) - month(origin))
  ) |>
  left_join(as_tibble(retail), by = "Month") |>
  mutate(
    covered80 = Turnover >= lower80 & Turnover <= upper80,
    covered95 = Turnover >= lower95 & Turnover <= upper95,
    width80 = upper80 - lower80,
    width95 = upper95 - lower95
  )

stopifnot(
  nrow(retail_forecasts) == length(origin_months) * 12L * 3L,
  all(retail_forecasts$horizon %in% 1:12),
  !anyNA(retail_forecasts$Turnover),
  all(retail_forecasts$lower95 <= retail_forecasts$lower80),
  all(retail_forecasts$upper80 <= retail_forecasts$upper95)
)

coverage_summary <- retail_forecasts |>
  group_by(model) |>
  summarise(
    forecasts = n(),
    coverage80 = mean(covered80),
    coverage95 = mean(covered95),
    mean_width80 = mean(width80),
    mean_width95 = mean(width95),
    .groups = "drop"
  )

coverage_by_horizon <- retail_forecasts |>
  group_by(model, horizon) |>
  summarise(
    forecasts = n(),
    coverage80 = mean(covered80),
    coverage95 = mean(covered95),
    mean_width80 = mean(width80),
    mean_width95 = mean(width95),
    .groups = "drop"
  )

write.csv(
  coverage_summary,
  file.path("scripts", "forecast_uncertainty_coverage.csv"),
  row.names = FALSE
)

# One genuine forecast made at the end of 2017 and checked against 2018.
single_forecast <- retail_forecasts |>
  filter(origin == yearmonth("2017 Dec"), model == "ETS")

history <- retail |>
  filter(Month >= yearmonth("2015 Jan"), Month <= yearmonth("2017 Dec"))

single_plot <- ggplot() +
  geom_line(
    data = history,
    aes(Month, Turnover, colour = "Observed before forecast"),
    linewidth = 0.75
  ) +
  geom_ribbon(
    data = single_forecast,
    aes(Month, ymin = lower95, ymax = upper95, fill = "95% interval"),
    alpha = 0.45
  ) +
  geom_ribbon(
    data = single_forecast,
    aes(Month, ymin = lower80, ymax = upper80, fill = "80% interval"),
    alpha = 0.75
  ) +
  geom_line(
    data = single_forecast,
    aes(Month, point, colour = "Point forecast"),
    linewidth = 0.9
  ) +
  geom_line(
    data = single_forecast,
    aes(Month, Turnover, colour = "What happened"),
    linewidth = 0.95
  ) +
  geom_point(
    data = single_forecast,
    aes(Month, Turnover, colour = "What happened"),
    size = 1.8
  ) +
  geom_vline(
    xintercept = as.numeric(yearmonth("2017 Dec")),
    colour = muted, linetype = "dashed"
  ) +
  geom_text(
    data = tibble(
      Month = yearmonth("2017 Oct"),
      Turnover = max(single_forecast$upper95) * 1.015,
      label = "Forecast made here"
    ),
    aes(Month, Turnover, label = label),
    hjust = 1, colour = muted, size = 3.4
  ) +
  scale_colour_manual(values = c(
    "Observed before forecast" = "#7a858a",
    "Point forecast" = navy,
    "What happened" = orange
  )) +
  scale_fill_manual(values = c(
    "80% interval" = teal,
    "95% interval" = teal_light
  )) +
  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "One forecast is a range of possible futures",
    subtitle = paste0(
      "ETS forecast for monthly Victorian food retailing turnover, made in December 2017.\n",
      "The orange line was unknown when the forecast was produced."
    ),
    x = NULL,
    y = "Turnover ($ million)",
    caption = "Source: Australian Bureau of Statistics data supplied in tsibbledata::aus_retail."
  ) +
  guides(
    colour = guide_legend(order = 1),
    fill = guide_legend(order = 2, override.aes = list(alpha = c(0.75, 0.45)))
  ) +
  article_theme()

save_article_plot(single_plot, "forecast-uncertainty-single-forecast.png")

coverage_long <- coverage_summary |>
  select(model, coverage80, coverage95) |>
  pivot_longer(
    cols = starts_with("coverage"),
    names_to = "level",
    values_to = "coverage"
  ) |>
  mutate(
    level = recode(level, coverage80 = "80% interval", coverage95 = "95% interval"),
    target = if_else(level == "80% interval", 0.80, 0.95)
  )

coverage_plot <- ggplot(coverage_long, aes(model, coverage, fill = level)) +
  geom_col(position = position_dodge(width = 0.76), width = 0.66) +
  geom_text(
    aes(label = percent(coverage, accuracy = 1)),
    position = position_dodge(width = 0.76),
    vjust = -0.55, colour = ink, size = 3.7
  ) +
  geom_hline(yintercept = 0.80, colour = teal_dark, linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = 0.95, colour = orange, linetype = "dotted", linewidth = 0.7) +
  scale_fill_manual(values = c("80% interval" = teal, "95% interval" = orange)) +
  scale_y_continuous(
    labels = label_percent(), limits = c(0, 1.04),
    breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "The label on an interval is a claim that can be tested",
    subtitle = paste(
      paste0("Observed coverage across ", nrow(retail_forecasts) / 3, " rolling forecasts per model."),
      "Dotted lines show the advertised 80% and 95% rates."
    ),
    x = NULL,
    y = "Share of actual outcomes inside the interval",
    caption = paste(
      "Rolling origins: December 2013 to December 2017; forecast horizons: 1–12 months.",
      "Source: ABS data supplied in tsibbledata::aus_retail."
    )
  ) +
  article_theme()

save_article_plot(coverage_plot, "forecast-uncertainty-coverage.png")

width_long <- coverage_by_horizon |>
  filter(model == "ETS") |>
  select(horizon, mean_width80, mean_width95) |>
  pivot_longer(
    cols = starts_with("mean_width"),
    names_to = "level",
    values_to = "width"
  ) |>
  mutate(level = recode(
    level,
    mean_width80 = "80% interval",
    mean_width95 = "95% interval"
  ))

width_plot <- ggplot(width_long, aes(horizon, width, colour = level)) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.25) +
  scale_colour_manual(values = c("80% interval" = teal, "95% interval" = orange)) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Uncertainty grows as the forecast reaches further ahead",
    subtitle = paste0(
      "Average width of ETS prediction intervals across ",
      length(origin_months), " rolling forecast origins."
    ),
    x = "Forecast horizon (months ahead)",
    y = "Average interval width ($ million)",
    caption = "Source: Author's calculations using ABS data supplied in tsibbledata::aus_retail."
  ) +
  article_theme()

save_article_plot(width_plot, "forecast-uncertainty-horizon.png")

# -----------------------------------------------------------------------------
# Supporting example: the 1989 Ansett pilots' dispute
# -----------------------------------------------------------------------------

ansett_total <- ansett |>
  as_tibble() |>
  group_by(Week) |>
  summarise(Passengers = sum(Passengers), .groups = "drop") |>
  as_tsibble(index = Week)

stopifnot(
  nrow(scan_gaps(ansett_total)) == 0L,
  sum(ansett_total$Passengers == 0) == 7L
)

ansett_origin <- yearweek("1989 W30")
ansett_training <- ansett_total |> filter(Week <= ansett_origin)

ansett_forecast <- ansett_training |>
  model(ARIMA = ARIMA(log1p(Passengers))) |>
  forecast(h = 20) |>
  as_tibble() |>
  transmute(
    Week,
    point = .mean,
    lower95 = distribution_quantile(Passengers, 0.025),
    upper95 = distribution_quantile(Passengers, 0.975)
  ) |>
  left_join(as_tibble(ansett_total), by = "Week") |>
  mutate(covered95 = Passengers >= lower95 & Passengers <= upper95)

ansett_history <- ansett_total |>
  filter(Week >= yearweek("1988 W35"), Week <= ansett_origin)

zero_period <- ansett_total |> filter(Passengers == 0)

ansett_forecast_plot <- ansett_forecast |> mutate(Date = as.Date(Week))
ansett_history_plot <- ansett_history |> mutate(Date = as.Date(Week))
zero_period_plot <- zero_period |> mutate(Date = as.Date(Week))

ansett_plot <- ggplot() +
  annotate(
    "rect",
    xmin = min(zero_period_plot$Date),
    xmax = max(zero_period_plot$Date) + 6,
    ymin = -Inf, ymax = Inf, fill = "#f1d8ce", alpha = 0.6
  ) +
  geom_line(
    data = ansett_history_plot,
    aes(Date, Passengers, colour = "Observed before forecast"),
    linewidth = 0.75
  ) +
  geom_ribbon(
    data = ansett_forecast_plot,
    aes(Date, ymin = lower95, ymax = upper95, fill = "95% interval"),
    alpha = 0.65
  ) +
  geom_line(
    data = ansett_forecast_plot,
    aes(Date, point, colour = "Point forecast"),
    linewidth = 0.95
  ) +
  geom_line(
    data = ansett_forecast_plot,
    aes(Date, Passengers, colour = "What happened"),
    linewidth = 1
  ) +
  geom_point(
    data = ansett_forecast_plot,
    aes(Date, Passengers, colour = "What happened"),
    size = 1.8
  ) +
  geom_vline(
    xintercept = as.Date(ansett_origin),
    colour = muted, linetype = "dashed"
  ) +
  geom_text(
    data = tibble(
      Date = as.Date(yearweek("1989 W37")),
      Passengers = max(ansett_forecast$upper95) * 0.98,
      label = "Pilots' dispute"
    ),
    aes(Date, Passengers, label = label),
    colour = "#8d4937", fontface = "bold", size = 3.6
  ) +
  scale_colour_manual(values = c(
    "Observed before forecast" = "#7a858a",
    "Point forecast" = navy,
    "What happened" = orange
  )) +
  scale_fill_manual(values = c("95% interval" = teal_light)) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_number(big.mark = ","), limits = c(0, NA)) +
  labs(
    title = "A 95% interval is not a shield against structural shocks",
    subtitle = paste0(
      "Twenty-week ARIMA forecast of total Ansett passengers, made before the 1989 pilots' dispute.\n",
      "The model had no way to anticipate seven weeks with no passengers."
    ),
    x = NULL,
    y = "Passengers per week",
    caption = "Source: Ansett Airlines data supplied in tsibbledata::ansett."
  ) +
  article_theme()

save_article_plot(ansett_plot, "forecast-uncertainty-ansett-shock.png")

ansett_zero_misses <- ansett_forecast |>
  filter(Passengers == 0, !covered95) |>
  nrow()

cat("\nRetail data audit\n")
cat("Rows:", nrow(retail), "\n")
cat("Range:", format(min(retail$Month)), "to", format(max(retail$Month)), "\n")
cat("Missing months:", nrow(scan_gaps(retail)), "\n")
cat("Missing turnover:", sum(is.na(retail$Turnover)), "\n")
cat("\nRolling forecast coverage\n")
print(coverage_summary)
cat("\nAnsett dispute check\n")
cat("Zero-passenger weeks outside the 95% interval:", ansett_zero_misses, "of 7\n")
