#' Simulate Clinical Trial Outcomes
#'
#' Simulates patient-level outcomes (continuous, binary, or ordinal) based on treatment assignment
#' and specified treatment effect. Optionally includes covariate-based variation and generates
#' ADaM-like variables (`AVAL`, `AVALC`, `PARAM`, `PARAMCD`).
#'
#' @param df Data frame including treatment assignment (`ARM` or `ARMCD`).
#' @param arm_col Character; name of the treatment column (default = "ARM").
#' @param outcome_type Type of outcome: "continuous", "binary", or "ordinal".
#' @param effect_size Numeric; effect size (difference in means or odds ratio).
#' @param baseline_mean Mean outcome in control group (for continuous).
#' @param baseline_sd Standard deviation in control group (for continuous).
#' @param prob_baseline Probability of success in control group (for binary).
#' @param n_levels Integer; number of levels for ordinal outcome.
#' @param covariates Optional character vector of covariate names to include as predictors.
#' @param seed Optional integer for reproducibility.
#' @param param Character; label of the outcome (e.g. "Change in BP").
#'
#' @return Data frame with added columns: `AVAL`, `AVALC`, `PARAM`, `PARAMCD`.
#'
#' @export
simulate_outcomes <- function(df,
                              arm_col = "ARM",
                              outcome_type = c("continuous", "binary", "ordinal"),
                              effect_size = 0.5,
                              baseline_mean = 100,
                              baseline_sd = 15,
                              prob_baseline = 0.5,
                              n_levels = 3,
                              covariates = NULL,
                              seed = NULL,
                              param = "Simulated Endpoint") {

  if (!arm_col %in% names(df)) stop(paste("Column", arm_col, "not found."))
  if (!is.null(seed)) set.seed(seed)
  outcome_type <- match.arg(outcome_type)

  trt <- df[[arm_col]]
  arms <- unique(trt)
  if (length(arms) != 2) stop("Only two-arm simulations are currently supported.")

  treated <- trt == arms[2]  # assumes second is active

  n <- nrow(df)
  eta <- as.numeric(treated) * effect_size

  # Add covariate effects
  if (!is.null(covariates)) {
    for (var in covariates) {
      if (!var %in% names(df)) stop(paste("Covariate", var, "not in dataframe"))
      norm_var <- scale(as.numeric(df[[var]]))
      eta <- eta + 0.1 * norm_var  # small covariate effect
    }
  }

  if (outcome_type == "continuous") {
    df$AVAL <- rnorm(n, mean = baseline_mean + eta, sd = baseline_sd)
    df$AVALC <- NA
  }

  if (outcome_type == "binary") {
    logit_control <- log(prob_baseline / (1 - prob_baseline))
    p <- 1 / (1 + exp(-(logit_control + eta)))
    df$AVAL <- rbinom(n, 1, p)
    df$AVALC <- ifelse(df$AVAL == 1, "Responder", "Non-responder")
  }

  if (outcome_type == "ordinal") {
    cutpoints <- qlogis(seq(0.2, 0.8, length.out = n_levels - 1))
    latent_score <- rnorm(n, mean = eta)
    df$AVAL <- cut(latent_score,
                   breaks = c(-Inf, cutpoints, Inf),
                   labels = paste("Grade", 1:n_levels),
                   right = TRUE)
    df$AVALC <- as.character(df$AVAL)
    df$AVAL <- as.numeric(df$AVAL)
  }

  df$PARAM <- param
  df$PARAMCD <- gsub(" ", "_", toupper(param))

  return(df)
}
