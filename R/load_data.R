# ---------------------------------------------------------------------------
# load_data.R
#
# Functions for locating, reading, validating, and cleaning the US air
# quality data (EPA, 2000-2023) used in this analysis.
#
# The entry point is build_analysis_bundle(), which reads the raw file once
# and returns both the analysis-ready table and an audit of dropped rows.
# ---------------------------------------------------------------------------

# Raw column names this analysis depends on. Used to validate the input file
# before any downstream work is attempted.
required_columns <- c(
  "Date", "State", "City",
  "NO2 Mean", "O3 Mean", "CO Mean", "SO2 Mean"
)

# Days treated as "weekend". lubridate::wday() returns 1 for Sunday and 7
# for Saturday under its default (US) week start.
weekend_day_numbers <- c(1, 7)

# Midweek reference level for the day-of-week regressions.
reference_weekday <- "Wed"


# Locate the dataset, accepting either the compressed or plain CSV.
#
# The repository ships the compressed file to stay well within GitHub's file
# size limits; readr reads both transparently.
#
# Input : folder    - directory to search
#         base_name - file name without extension
# Output: length-one character path to the file that exists
resolve_data_path <- function(folder = "data",
                              base_name = "pollution_2000_2023") {
  stopifnot(is.character(folder), is.character(base_name))

  candidates <- file.path(folder, paste0(base_name, c(".csv.gz", ".csv")))
  found <- candidates[file.exists(candidates)]

  if (length(found) == 0) {
    stop(
      "Could not find the dataset. Looked for:\n  ",
      paste(candidates, collapse = "\n  "),
      call. = FALSE
    )
  }

  found[1]
}


# Read the raw pollution file and confirm it has the columns we need.
#
# Input : path - length-one character path to the CSV or CSV.GZ file
# Output: tibble of raw data, unmodified apart from parsing
read_pollution_data <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)

  if (!file.exists(path)) {
    stop("Data file not found at '", path, "'.", call. = FALSE)
  }

  raw_data <- read_csv(path, show_col_types = FALSE, progress = FALSE)

  missing_columns <- setdiff(required_columns, names(raw_data))
  if (length(missing_columns) > 0) {
    stop(
      "Input file is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  raw_data
}


# Keep only the columns used in this analysis and give them snake_case names.
#
# Input : raw_data - tibble returned by read_pollution_data()
# Output: tibble with seven renamed columns
select_analysis_columns <- function(raw_data) {
  stopifnot(is.data.frame(raw_data))

  raw_data |>
    select(
      date = "Date",
      state = "State",
      city = "City",
      no2_mean = "NO2 Mean",
      o3_mean = "O3 Mean",
      co_mean = "CO Mean",
      so2_mean = "SO2 Mean"
    )
}


# Drop observations that cannot be used by a log-linear model.
#
# Monitors occasionally report zero or slightly negative concentrations when
# true levels sit near the instrument detection limit. These are physically
# impossible and undefined on the log scale, so they are removed. Rows with
# the placeholder city label "Not in a city" are also removed because they
# cannot be attributed to a specific urban area.
#
# Input : data        - tibble from select_analysis_columns()
#         placeholder - city label marking unlocated monitors
# Output: tibble of strictly positive, locatable observations
drop_unusable_rows <- function(data, placeholder = "Not in a city") {
  stopifnot(is.data.frame(data), is.character(placeholder))

  data |>
    filter(
      .data$no2_mean > 0,
      .data$o3_mean > 0,
      .data$city != placeholder
    )
}


# Add the calendar and location variables used as model terms.
#
# location combines state and city because three city names (for example
# Portland) appear in more than one state and would otherwise be merged.
# day_of_week is releveled to a midweek reference so that regression
# coefficients describe deviations from a typical working day.
#
# Input : data - tibble from drop_unusable_rows()
# Output: tibble with location, day_of_week, is_weekend, month, year added
add_model_variables <- function(data) {
  stopifnot(is.data.frame(data), inherits(data$date, "Date"))

  weekday_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  data |>
    mutate(
      location = factor(paste(.data$state, .data$city, sep = " - ")),
      day_of_week = relevel(
        factor(
          wday(.data$date, label = TRUE, abbr = TRUE),
          levels = weekday_labels,
          ordered = FALSE
        ),
        ref = reference_weekday
      ),
      is_weekend = factor(
        if_else(wday(.data$date) %in% weekend_day_numbers,
                "Weekend", "Weekday"),
        levels = c("Weekday", "Weekend")
      ),
      month = factor(month(.data$date, label = TRUE, abbr = TRUE),
                     ordered = FALSE),
      year = factor(year(.data$date))
    )
}


# Restrict the data to the locations with the most complete records.
#
# Input : data        - tibble from add_model_variables()
#         n_locations - positive integer, how many locations to keep
# Output: tibble filtered to the best-covered monitoring sites
select_top_locations <- function(data, n_locations) {
  stopifnot(
    is.data.frame(data),
    is.numeric(n_locations),
    length(n_locations) == 1L,
    n_locations >= 1
  )

  available <- nlevels(droplevels(data$location))
  if (n_locations > available) {
    stop(
      "Requested ", n_locations, " locations but only ", available,
      " are present in the data.",
      call. = FALSE
    )
  }

  kept <- data |>
    count(.data$location, sort = TRUE) |>
    slice_head(n = n_locations) |>
    pull(.data$location)

  data |>
    filter(.data$location %in% kept) |>
    mutate(location = droplevels(.data$location))
}


# Count how many rows are lost at each cleaning stage, so that the report can
# justify every dropped observation.
#
# Input : column_data - tibble from select_analysis_columns()
# Output: tibble with one row per cleaning stage
audit_cleaning_steps <- function(column_data) {
  stopifnot(is.data.frame(column_data))

  positive_only <- column_data |>
    filter(.data$no2_mean > 0, .data$o3_mean > 0)

  tibble(
    stage = c(
      "Raw observations",
      "Remove non-positive NO2 or O3 readings",
      "Remove monitors with no city assigned"
    ),
    rows_remaining = c(
      nrow(column_data),
      nrow(positive_only),
      nrow(drop_unusable_rows(column_data))
    )
  ) |>
    mutate(rows_dropped = lag(.data$rows_remaining) - .data$rows_remaining)
}


# Read the raw file once and return everything the report needs.
#
# Input : path        - path to the raw CSV or CSV.GZ
#         n_locations - number of monitoring locations to retain
# Output: named list with elements `data` and `audit`
build_analysis_bundle <- function(path, n_locations) {
  column_data <- path |>
    read_pollution_data() |>
    select_analysis_columns()

  analysis_data <- column_data |>
    drop_unusable_rows() |>
    add_model_variables() |>
    select_top_locations(n_locations = n_locations)

  list(
    data = analysis_data,
    audit = audit_cleaning_steps(column_data)
  )
}


# Summarise annual means for the preliminary trend figure.
#
# Input : data     - analysis tibble
#         outcomes - named character vector of outcome columns
# Output: long tibble with year, pollutant, and mean_value columns
summarize_annual_means <- function(data, outcomes) {
  stopifnot(is.data.frame(data), is.character(outcomes))

  data |>
    select(date, all_of(unname(outcomes))) |>
    mutate(year = year(.data$date)) |>
    summarize(across(all_of(unname(outcomes)), mean), .by = "year") |>
    pivot_longer(
      cols = all_of(unname(outcomes)),
      names_to = "column",
      values_to = "mean_value"
    ) |>
    mutate(
      pollutant = names(outcomes)[match(.data$column, outcomes)]
    )
}
