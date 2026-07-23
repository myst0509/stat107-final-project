# ---------------------------------------------------------------------------
# plots.R
#
# Figure-building functions. Every function returns a ggplot object so the
# Quarto report only has to call the function and print the result.
# ---------------------------------------------------------------------------

# Shared colour scheme so the two pollutants are identifiable across figures.
pollutant_colours <- c(
  "Nitrogen dioxide (NO2)" = "#B2182B",
  "Ozone (O3)" = "#2166AC"
)

# House theme applied to every figure in the report.
project_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey30"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}


# Main figure: adjusted day-of-week profile for both pollutants.
#
# Values are model-adjusted percentage differences relative to the midweek
# reference day, so seasonal, annual, and location differences are already
# controlled for. Days are placed on a numeric axis so that the weekend can
# be shaded as a background band.
#
# Input : effects       - tibble from summarize_weekly_effects()
#         reference_day - day used as the regression reference level
# Output: ggplot object
plot_weekly_profile <- function(effects, reference_day = "Wed") {
  stopifnot(is.data.frame(effects), "pollutant" %in% names(effects))

  day_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  weekend_start <- match("Sat", day_levels) - 0.5
  weekend_end <- length(day_levels) + 0.5

  plot_data <- effects |>
    add_reference_row(reference_day = reference_day) |>
    mutate(day_position = match(.data$term, day_levels)) |>
    arrange(.data$pollutant, .data$day_position)

  ggplot(plot_data, aes(x = .data$day_position, y = .data$pct_change,
                        colour = .data$pollutant)) +
    annotate(
      "rect",
      xmin = weekend_start, xmax = weekend_end,
      ymin = -Inf, ymax = Inf,
      fill = "grey85", alpha = 0.55
    ) +
    annotate(
      "text",
      x = mean(c(weekend_start, weekend_end)),
      y = Inf, label = "Weekend",
      vjust = 1.6, colour = "grey35", size = 3.4
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
    geom_line(linewidth = 0.9) +
    geom_errorbar(
      aes(ymin = .data$pct_low, ymax = .data$pct_high),
      width = 0.14, linewidth = 0.6
    ) +
    geom_point(size = 2.6) +
    scale_x_continuous(breaks = seq_along(day_levels), labels = day_levels) +
    scale_colour_manual(values = pollutant_colours) +
    labs(
      title = "Ozone rises on weekends even as traffic pollution falls",
      subtitle = paste0(
        "Model-adjusted percentage difference from ", reference_day,
        ", with 95% confidence intervals"
      ),
      x = "Day of the week",
      y = "Difference in concentration (%)",
      colour = "Pollutant"
    ) +
    project_theme()
}


# Add a zero row for the regression reference day, which has no coefficient.
#
# Input : effects       - tibble from summarize_weekly_effects()
#         reference_day - name of the omitted reference level
# Output: tibble with one extra row per pollutant
add_reference_row <- function(effects, reference_day) {
  stopifnot(is.data.frame(effects))

  reference_rows <- effects |>
    distinct(.data$pollutant) |>
    mutate(
      term = reference_day,
      pct_change = 0,
      pct_low = 0,
      pct_high = 0
    )

  bind_rows(effects, reference_rows)
}


# Preliminary figure: distribution of an outcome before and after logging.
#
# Input : data    - analysis tibble
#         outcome - character name of the outcome column
#         label   - axis label for the raw variable
# Output: ggplot object
plot_log_justification <- function(data, outcome, label) {
  stopifnot(is.data.frame(data), outcome %in% names(data))

  data |>
    select(raw_value = all_of(outcome)) |>
    mutate(`Log scale` = log(.data$raw_value)) |>
    rename(`Original scale` = "raw_value") |>
    pivot_longer(everything(), names_to = "scale", values_to = "value") |>
    ggplot(aes(x = .data$value)) +
    geom_histogram(bins = 60, fill = "#4D4D4D") +
    facet_wrap(~ .data$scale, scales = "free") +
    labs(
      title = paste("Logging removes the right skew in", label),
      x = "Daily mean concentration",
      y = "Number of monitor-days"
    ) +
    project_theme()
}


# Preliminary figure: annual average concentration over the study period.
#
# Input : yearly_means - tibble with year, pollutant, and mean_value columns
# Output: ggplot object
plot_annual_trend <- function(yearly_means) {
  stopifnot(is.data.frame(yearly_means), "pollutant" %in% names(yearly_means))

  ggplot(yearly_means, aes(x = .data$year, y = .data$mean_value,
                           colour = .data$pollutant)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    facet_wrap(~ .data$pollutant, scales = "free_y") +
    scale_colour_manual(values = pollutant_colours, guide = "none") +
    labs(
      title = "Long-run pollutant trends across the study period",
      subtitle = "Annual means across the selected monitoring locations",
      x = "Year",
      y = "Mean daily concentration (ppb)"
    ) +
    project_theme()
}


# Diagnostic figure: residuals against fitted values.
#
# Input : diagnostics - tibble from sample_model_diagnostics()
#         label       - pollutant name for the title
# Output: ggplot object
plot_residual_fitted <- function(diagnostics, label) {
  stopifnot(is.data.frame(diagnostics), "fitted" %in% names(diagnostics))

  ggplot(diagnostics, aes(x = .data$fitted, y = .data$residual)) +
    geom_point(alpha = 0.15, size = 0.7, colour = "#333333") +
    geom_hline(yintercept = 0, colour = "#B2182B", linewidth = 0.7) +
    labs(
      title = paste("Residuals versus fitted values:", label),
      x = "Fitted value (log scale)",
      y = "Residual (log scale)"
    ) +
    project_theme()
}


# Diagnostic figure: normal quantile-quantile plot of residuals.
#
# Input : diagnostics - tibble from sample_model_diagnostics()
#         label       - pollutant name for the title
# Output: ggplot object
plot_residual_qq <- function(diagnostics, label) {
  stopifnot(is.data.frame(diagnostics), "std_residual" %in% names(diagnostics))

  ggplot(diagnostics, aes(sample = .data$std_residual)) +
    stat_qq(alpha = 0.2, size = 0.7, colour = "#333333") +
    stat_qq_line(colour = "#B2182B", linewidth = 0.7) +
    labs(
      title = paste("Normal Q-Q plot of residuals:", label),
      x = "Theoretical normal quantile",
      y = "Standardised residual"
    ) +
    project_theme()
}
