#' Simulate Time-to-Event (Survival) Data
#'
#' Simulates survival times for a clinical trial population, with optional treatment effect
#' and administrative censoring. Returns survival time, censoring indicator, and other relevant fields.
#'
#' @param df Data frame with patient-level data. Must include a treatment column (`ARM` or `ARMCD`).
#' @param arm_col Name of the treatment variable in `df`. Default is "ARM".
#' @param hr Hazard ratio for treatment vs control (HR < 1 = beneficial effect).
#' @param baseline_median Median survival time (in days) for the control group.
#' @param max_followup Administrative censoring time (e.g., 365 days).
#' @param shape Weibull shape parameter (default = 1 for exponential).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame with added columns: `TIME`, `EVENT`, `CNSR`, `AVAL`, `AVALC`, `PARAM`.
#'
#' @export
simulate_survival <- function(df,
                              arm_col = "ARM",
                              hr = 0.7,
                              baseline_median = 365,
                              max_followup = 365,
                              shape = 1,
                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (!(arm_col %in% colnames(df))) stop(paste("Treatment column", arm_col, "not found in df"))

  n <- nrow(df)
  trt <- df[[arm_col]]
  trt_levels <- unique(trt)
  if (length(trt_levels) != 2) stop("Function only supports 2-arm trials.")

  # Assign λ based on median survival time
  lambda_control <- (log(2) / baseline_median)^(1 / shape)
  lambda_treated <- lambda_control * hr^(1 / shape)

  lambda <- ifelse(trt == trt_levels[1], lambda_control, lambda_treated)

  # Simulate survival times using Weibull
  time <- (-log(runif(n)) / lambda)^(1 / shape)

  # Censor administratively
  event <- ifelse(time <= max_followup, 1, 0)
  time_censored <- pmin(time, max_followup)

  df$TIME <- round(time_censored, 1)
  df$EVENT <- event
  df$CNSR <- 1 - event
  df$AVAL <- df$TIME
  df$AVALC <- ifelse(df$EVENT == 1, "Event", "Censored")
  df$PARAM <- "Overall Survival (days)"

  return(df)
}
