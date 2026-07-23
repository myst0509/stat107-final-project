# ---------------------------------------------------------------------------
# summaries.R
#
# Functions that build the descriptive tables shown in the report: the data
# dictionary, the outcome summary statistics, and the formatted results
# table for the weekend effect.
# ---------------------------------------------------------------------------


# Build the data dictionary for the variables retained in the analysis.
#
# Input : none
# Output: tibble with one row per analysis variable
build_code_book <- function() {
  tribble(
    ~Variable, ~Type, ~Description,
    "date", "Date",
    "Calendar date of the monitor reading (2000-01-01 to 2023-09-30).",
    "state", "character",
    "US state containing the monitoring site.",
    "city", "character",
    "City containing the monitoring site.",
    "location", "factor",
    paste(
      "State and city combined. Used as a fixed effect because three city",
      "names appear in more than one state."
    ),
    "no2_mean", "numeric",
    paste(
      "Primary outcome. Mean nitrogen dioxide concentration for the day,",
      "in parts per billion. Used as a marker of vehicle exhaust."
    ),
    "o3_mean", "numeric",
    paste(
      "Second outcome. Mean ground-level ozone concentration for the day,",
      "in parts per million. Ozone is formed by photochemistry, not",
      "emitted directly."
    ),
    "co_mean", "numeric",
    "Mean carbon monoxide concentration, retained for the appendix check.",
    "so2_mean", "numeric",
    "Mean sulphur dioxide concentration, retained for the appendix check.",
    "day_of_week", "factor",
    "Day of the week, with Wednesday as the regression reference level.",
    "is_weekend", "factor",
    "Weekday or Weekend. The primary predictor of interest.",
    "month", "factor",
    "Calendar month, included to absorb strong seasonal cycles.",
    "year", "factor",
    "Calendar year, included to absorb long-run emission trends."
  )
}


# Explain which raw columns were dropped and why.
#
# Input : none
# Output: tibble with one row per omitted variable group
build_omitted_variables <- function() {
  tribble(
    ~Variable, ~Reason,
    "Address",
    paste(
      "Street address of the monitor. Superseded by the coarser location",
      "factor, which gives more observations per level."
    ),
    "County",
    "Redundant once state and city identify the monitoring site.",
    "* 1st Max Value, * 1st Max Hour",
    paste(
      "Daily maxima and their timing describe short peaks rather than the",
      "daily burden this analysis models."
    ),
    "* AQI",
    paste(
      "AQI is a piecewise, non-linear rescaling of the same concentration",
      "readings, so modelling it would distort the percentage comparisons."
    )
  )
}


# Summary statistics for the outcome variables.
#
# Input : data     - analysis tibble
#         outcomes - named character vector of outcome columns
# Output: tibble with one row per outcome
summarize_outcomes <- function(data, outcomes) {
  stopifnot(is.data.frame(data), is.character(outcomes))

  outcomes |>
    imap(\(column, label) {
      values <- data[[column]]
      tibble(
        Pollutant = label,
        N = length(values),
        Mean = mean(values),
        SD = sd(values),
        Median = median(values),
        IQR = IQR(values)
      )
    }) |>
    list_rbind()
}


# Format the weekend effect estimates for presentation.
#
# Input : effects - tibble from summarize_weekly_effects()
# Output: tibble with presentation-ready columns
format_effect_table <- function(effects) {
  stopifnot(is.data.frame(effects), "pct_change" %in% names(effects))

  effects |>
    transmute(
      Pollutant = .data$pollutant,
      `Coefficient (log scale)` = round(.data$estimate, 4),
      `Std. error` = round(.data$std_error, 4),
      `t statistic` = round(.data$statistic, 1),
      `p value` = format.pval(.data$p_value, digits = 3, eps = 1e-300),
      `Change (%)` = round(.data$pct_change, 2),
      `95% CI (%)` = paste0(
        "[", round(.data$pct_low, 2), ", ", round(.data$pct_high, 2), "]"
      )
    )
}
