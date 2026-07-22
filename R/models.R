# ---------------------------------------------------------------------------
# models.R
#
# Model fitting and coefficient extraction for the weekday/weekend analysis.
#
# Both outcomes are modelled on the natural log scale, so a coefficient b
# corresponds to an approximate percentage change of 100 * (exp(b) - 1).
# ---------------------------------------------------------------------------


# Build the model formula for a given outcome and weekly-cycle term.
#
# Input : outcome      - character name of the outcome column
#         weekly_term  - character name of the weekly predictor
#         controls     - character vector of control terms
# Output: a formula object
build_model_formula <- function(outcome, weekly_term,
                                controls = c("month", "year", "location")) {
  stopifnot(
    is.character(outcome), length(outcome) == 1L,
    is.character(weekly_term), length(weekly_term) == 1L,
    is.character(controls), length(controls) >= 1L
  )

  right_side <- paste(c(weekly_term, controls), collapse = " + ")
  as.formula(paste0("log(", outcome, ") ~ ", right_side))
}


# Fit one log-linear regression.
#
# Input : data        - analysis tibble
#         outcome     - character name of the outcome column
#         weekly_term - character name of the weekly predictor
# Output: an lm object
fit_pollutant_model <- function(data, outcome, weekly_term) {
  stopifnot(is.data.frame(data), outcome %in% names(data))

  if (any(data[[outcome]] <= 0, na.rm = TRUE)) {
    stop(
      "Outcome '", outcome, "' contains non-positive values, which cannot ",
      "be log-transformed. Run drop_unusable_rows() first.",
      call. = FALSE
    )
  }

  model_formula <- build_model_formula(outcome, weekly_term)
  lm(model_formula, data = data)
}


# Fit the same specification to several outcomes at once.
#
# Input : data        - analysis tibble
#         outcomes    - named character vector of outcome columns
#         weekly_term - character name of the weekly predictor
# Output: named list of lm objects
fit_all_models <- function(data, outcomes, weekly_term) {
  stopifnot(is.character(outcomes), !is.null(names(outcomes)))

  map(outcomes, \(outcome) {
    fit_pollutant_model(data, outcome = outcome, weekly_term = weekly_term)
  })
}


# Extract estimates for the model terms matching a prefix, converting the
# log-scale coefficients into percentage changes with confidence intervals.
#
# Input : model      - an lm object
#         prefix     - character prefix identifying terms of interest
#         conf_level - confidence level for the interval
# Output: tibble with one row per matching term
extract_term_estimates <- function(model, prefix, conf_level = 0.95) {
  stopifnot(inherits(model, "lm"), is.character(prefix))

  coefficient_table <- summary(model)$coefficients
  matched <- grep(paste0("^", prefix), rownames(coefficient_table),
                  value = TRUE)

  if (length(matched) == 0) {
    stop("No model terms start with '", prefix, "'.", call. = FALSE)
  }

  intervals <- confint(model, parm = matched, level = conf_level)

  tibble(
    term = sub(paste0("^", prefix), "", matched),
    estimate = coefficient_table[matched, "Estimate"],
    std_error = coefficient_table[matched, "Std. Error"],
    statistic = coefficient_table[matched, "t value"],
    p_value = coefficient_table[matched, "Pr(>|t|)"],
    conf_low = intervals[, 1],
    conf_high = intervals[, 2]
  ) |>
    mutate(
      pct_change = 100 * (exp(.data$estimate) - 1),
      pct_low = 100 * (exp(.data$conf_low) - 1),
      pct_high = 100 * (exp(.data$conf_high) - 1)
    )
}


# Collect the weekly-cycle estimates from several fitted models.
#
# Input : models - named list of lm objects
#         prefix - character prefix identifying terms of interest
# Output: tibble with one row per model term, labelled by pollutant
summarize_weekly_effects <- function(models, prefix) {
  stopifnot(is.list(models), !is.null(names(models)))

  models |>
    imap(\(model, label) {
      extract_term_estimates(model, prefix = prefix) |>
        mutate(pollutant = label, .before = everything())
    }) |>
    list_rbind()
}


# Report standard goodness-of-fit measures for each fitted model.
#
# Input : models - named list of lm objects
# Output: tibble with one row per model
summarize_model_fit <- function(models) {
  stopifnot(is.list(models), !is.null(names(models)))

  models |>
    imap(\(model, label) {
      model_summary <- summary(model)
      tibble(
        pollutant = label,
        observations = length(model$residuals),
        parameters = length(coef(model)),
        adj_r_squared = model_summary$adj.r.squared,
        residual_se = model_summary$sigma
      )
    }) |>
    list_rbind()
}


# Draw a reproducible sample of fitted values and residuals.
#
# Diagnostic plots of 200,000+ points are unreadable and slow to render, so
# the report inspects a random subset instead.
#
# Input : model       - an lm object
#         sample_size - number of points to retain
#         seed        - integer seed for reproducibility
# Output: tibble with fitted, residual, and standardised residual columns
sample_model_diagnostics <- function(model, sample_size = 5000, seed = 107) {
  stopifnot(inherits(model, "lm"), sample_size >= 1)

  residual_values <- residuals(model)
  n_available <- length(residual_values)
  n_keep <- min(sample_size, n_available)

  set.seed(seed)
  chosen <- sample.int(n_available, size = n_keep)

  tibble(
    fitted = fitted(model)[chosen],
    residual = residual_values[chosen]
  ) |>
    mutate(std_residual = .data$residual / sd(residual_values))
}
