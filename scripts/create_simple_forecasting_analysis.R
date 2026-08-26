suppressPackageStartupMessages({
  library(fpp3)
  library(purrr)
  library(scales)
})

output_dir <- "images"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

navy <- "#092c47"
teal <- "#487b87"
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

retail <- aus_retail |>
  filter(State == "Victoria", Industry == "Food retailing") |>
  select(Month, Turnover)

stopifnot(
  nrow(retail) == 441L,
  nrow(scan_gaps(retail)) == 0L,
  !anyNA(retail$Turnover)
)

origin_months <- retail |>
  filter(Month >= yearmonth("2013 Dec"), Month <= yearmonth("2017 Dec")) |>
  slice(seq(1, n(), by = 2)) |>
  pull(Month)

forecast_from_origin <- function(origin_position) {
  origin <- origin_months[origin_position]
  training <- retail |> filter(Month <= origin)

  training |>
    model(
      `Seasonal naive` = SNAIVE(Turnover),
      ETS = ETS(log(Turnover)),
      ARIMA = ARIMA(log(Turnover) ~ pdq(0, 1, 1) + PDQ(0, 1, 1))
    ) |>
    forecast(h = 12) |>
    as_tibble() |>
    transmute(origin, Month, model = .model, point = .mean)
}

forecasts <- map_dfr(seq_along(origin_months), forecast_from_origin) |>
  mutate(
    horizon = (year(Month) - year(origin)) * 12L +
      (month(Month) - month(origin))
  ) |>
  left_join(as_tibble(retail), by = "Month") |>
  mutate(
    error = Turnover - point,
    absolute_error = abs(error),
    squared_error = error^2,
    absolute_percentage_error = abs(error / Turnover)
  )

stopifnot(
  nrow(forecasts) == length(origin_months) * 12L * 3L,
  all(forecasts$horizon %in% 1:12),
  !anyNA(forecasts$Turnover)
)

overall_accuracy <- forecasts |>
  group_by(model) |>
  summarise(
    forecasts = n(),
    MAE = mean(absolute_error),
    RMSE = sqrt(mean(squared_error)),
    MAPE = mean(absolute_percentage_error),
    .groups = "drop"
  ) |>
  mutate(
    relative_MAE = MAE / MAE[model == "Seasonal naive"],
    relative_RMSE = RMSE / RMSE[model == "Seasonal naive"]
  )

accuracy_by_horizon <- forecasts |>
  group_by(model, horizon) |>
  summarise(
    forecasts = n(),
    MAE = mean(absolute_error),
    RMSE = sqrt(mean(squared_error)),
    .groups = "drop"
  ) |>
  group_by(horizon) |>
  mutate(relative_MAE = MAE / MAE[model == "Seasonal naive"]) |>
  ungroup()

winner_counts <- forecasts |>
  group_by(origin, Month, horizon) |>
  slice_min(absolute_error, n = 1, with_ties = FALSE) |>
  ungroup() |>
  count(model, name = "wins") |>
  mutate(share = wins / sum(wins))

write.csv(
  overall_accuracy,
  file.path("scripts", "simple_forecasting_accuracy.csv"),
  row.names = FALSE
)

write.csv(
  accuracy_by_horizon,
  file.path("scripts", "simple_forecasting_accuracy_by_horizon.csv"),
  row.names = FALSE
)

comparison_plot <- overall_accuracy |>
  mutate(
    model = factor(model, levels = c("Seasonal naive", "ETS", "ARIMA")),
    label = paste0("$", comma(round(MAE)), "m")
  ) |>
  ggplot(aes(model, MAE, fill = model)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = label), vjust = -0.55, colour = ink, size = 4) +
  scale_fill_manual(values = c(
    "Seasonal naive" = orange,
    "ETS" = teal,
    "ARIMA" = navy
  )) +
  scale_y_continuous(
    labels = label_number(prefix = "$", suffix = "m", big.mark = ","),
    limits = c(0, max(overall_accuracy$MAE) * 1.17),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "The benchmark was beaten",
    subtitle = paste0(
      "Mean absolute error across ", nrow(forecasts) / 3,
      " rolling forecast-outcome comparisons per method.\nLower is better."
    ),
    x = NULL,
    y = "Mean absolute error",
    caption = paste(
      "Rolling origins: December 2013 to December 2017; horizons: 1-12 months.",
      "Source: ABS data supplied in tsibbledata::aus_retail."
    )
  ) +
  guides(fill = "none") +
  article_theme()

save_article_plot(
  comparison_plot,
  "simple-forecasting-overall-accuracy.png",
  width = 9.4,
  height = 5.8
)

horizon_plot <- accuracy_by_horizon |>
  filter(model != "Seasonal naive") |>
  mutate(model = factor(model, levels = c("ETS", "ARIMA"))) |>
  ggplot(aes(horizon, relative_MAE * 100, colour = model)) +
  geom_hline(yintercept = 100, colour = orange, linewidth = 0.9, linetype = "dashed") +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.2) +
  annotate(
    "text", x = 12, y = 102.5, label = "Seasonal naive benchmark",
    hjust = 1, colour = orange, size = 3.4
  ) +
  scale_colour_manual(values = c("ETS" = teal, "ARIMA" = navy)) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(20, 110)) +
  labs(
    title = "The answer changes with the forecast horizon",
    subtitle = paste(
      "MAE relative to the seasonal naive benchmark.",
      "Values below 100% indicate an improvement."
    ),
    x = "Forecast horizon (months ahead)",
    y = "MAE relative to seasonal naive",
    caption = "Source: Author's calculations using ABS data supplied in tsibbledata::aus_retail."
  ) +
  article_theme()

save_article_plot(
  horizon_plot,
  "simple-forecasting-horizon-accuracy.png",
  width = 9.8,
  height = 6.1
)

winner_plot <- winner_counts |>
  mutate(
    model = factor(model, levels = c("Seasonal naive", "ETS", "ARIMA")),
    label = paste0(wins, " of ", sum(wins))
  ) |>
  ggplot(aes(model, share, fill = model)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = label), vjust = -0.55, colour = ink, size = 4) +
  scale_fill_manual(values = c(
    "Seasonal naive" = orange,
    "ETS" = teal,
    "ARIMA" = navy
  )) +
  scale_y_continuous(
    labels = label_percent(), limits = c(0, 0.43),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "A weaker average model can still win individual months",
    subtitle = paste(
      "Method with the smallest absolute error for each origin and forecast month.",
      "A model can lose overall without losing every time.",
      sep = "\n"
    ),
    x = NULL,
    y = "Share of forecast-outcome comparisons won",
    caption = "Source: Author's calculations using ABS data supplied in tsibbledata::aus_retail."
  ) +
  guides(fill = "none") +
  article_theme()

save_article_plot(
  winner_plot,
  "simple-forecasting-individual-wins.png",
  width = 9.4,
  height = 5.8
)

single_origin <- forecasts |>
  filter(origin == yearmonth("2017 Dec"))

single_history <- retail |>
  filter(Month >= yearmonth("2015 Jan"), Month <= yearmonth("2017 Dec"))

single_plot <- ggplot() +
  geom_line(
    data = single_history,
    aes(Month, Turnover, colour = "Observed before forecast"),
    linewidth = 0.75
  ) +
  geom_line(
    data = single_origin,
    aes(Month, point, colour = model),
    linewidth = 0.95
  ) +
  geom_line(
    data = single_origin |> distinct(Month, Turnover),
    aes(Month, Turnover, colour = "What happened"),
    linewidth = 1
  ) +
  geom_point(
    data = single_origin |> distinct(Month, Turnover),
    aes(Month, Turnover, colour = "What happened"),
    size = 1.8
  ) +
  geom_vline(
    xintercept = as.numeric(yearmonth("2017 Dec")),
    colour = muted, linetype = "dashed"
  ) +
  scale_colour_manual(values = c(
    "Observed before forecast" = "#7a858a",
    "Seasonal naive" = orange,
    "ETS" = teal,
    "ARIMA" = navy,
    "What happened" = "#9b493d"
  )) +
  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Three methods, one unknown year",
    subtitle = paste(
      "Forecasts made in December 2017 for monthly Victorian food retailing turnover.",
      "The red line shows the values subsequently observed.",
      sep = "\n"
    ),
    x = NULL,
    y = "Turnover ($ million)",
    caption = "Source: Australian Bureau of Statistics data supplied in tsibbledata::aus_retail."
  ) +
  article_theme()

save_article_plot(
  single_plot,
  "simple-forecasting-one-year.png",
  width = 10,
  height = 6.2
)

cat("\nOverall point forecast accuracy\n")
print(overall_accuracy)
cat("\nBest method counts\n")
print(winner_counts)
cat("\nAccuracy by horizon\n")
print(accuracy_by_horizon, n = Inf)
