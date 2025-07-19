#' Simulate Patient Dropout
#'
#' Simulates dropout in a clinical trial population based on time, treatment group, and optional patient characteristics.
#'
#' @param df Data frame with patient-level data.
#' @param dropout_prob Overall probability of dropout (e.g., 0.2), or named vector by arm (e.g., c(Placebo = 0.3, Drug = 0.1)).
#' @param dropout_window Integer vector of possible dropout days (e.g., 10:365).
#' @param model Character; "uniform", "early", or "late" dropout distribution.
#' @param reason Logical; if TRUE, adds simulated reason (e.g., "Lost to follow-up", "Adverse event").
#' @param stratify_by Optional variable to stratify dropout behavior (e.g., "AGE", "SEX").
#' @param seed Optional integer for reproducibility.
#'
#' @return Data frame with added columns: `DROPOUT`, `DROPOUT_DAY`, `EOSDT`, `EOSSTT`, `DCSREAS`.
#'
#' @export
simulate_patient_dropout <- function(df,
                                     dropout_prob = 0.2,
                                     dropout_window = 10:365,
                                     model = c("uniform", "early", "late"),
                                     reason = TRUE,
                                     stratify_by = NULL,
                                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  model <- match.arg(model)

  n <- nrow(df)

  # Determine dropout probability by arm if named vector is passed
  arm <- if ("ARM" %in% names(df)) df$ARM else rep("All", n)
  probs <- if (is.numeric(dropout_prob) && length(dropout_prob) == 1) {
    rep(dropout_prob, n)
  } else if (is.named(dropout_prob)) {
    as.numeric(dropout_prob[arm])
  } else {
    stop("dropout_prob must be a single number or a named vector matching treatment arms.")
  }

  dropout_flag <- rbinom(n, 1, probs)

  # Distribution of dropout days
  if (model == "uniform") {
    days <- sample(dropout_window, n, replace = TRUE)
  } else if (model == "early") {
    days <- round(runif(n, min(dropout_window), quantile(dropout_window, 0.3)))
  } else if (model == "late") {
    days <- round(runif(n, quantile(dropout_window, 0.7), max(dropout_window)))
  }

  dropout_day <- ifelse(dropout_flag == 1, days, NA)

  # Apply dropout date
  trtsdt <- if ("TRTSDT" %in% names(df)) as.Date(df$TRTSDT) else as.Date("2024-01-01")
  eosdt <- ifelse(!is.na(dropout_day), trtsdt + dropout_day, NA)

  # Optional reason
  dcsreas <- if (reason) {
    reasons <- c("Lost to follow-up", "Adverse event", "Withdrawal of consent", "Protocol deviation", "Other")
    sample(reasons, n, replace = TRUE)
  } else {
    NA_character_
  }

  df$DROPOUT <- dropout_flag
  df$DROPOUT_DAY <- dropout_day
  df$EOSDT <- eosdt
  df$EOSSTT <- ifelse(dropout_flag == 1, "DISCONTINUED", "COMPLETED")
  df$DCSREAS <- ifelse(dropout_flag == 1, dcsreas, NA)

  return(df)
}
