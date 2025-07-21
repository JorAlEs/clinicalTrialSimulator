#' Simulate a single outcome per patient (continuous or binary)
#'
#' This function simulates an endpoint (e.g., a final outcome or response) for each patient, which could be continuous (e.g., a lab measurement) or binary (e.g., responder yes/no). It supports a treatment effect and optional cluster-level variability for continuous outcomes.
#'
#' @param data A data frame of patients that should include a treatment group column \code{Arm} (optional if only one group).
#' @param outcome_type Character, either "continuous" or "binary" (default = "continuous").
#' @param outcome_mean_control Numeric, mean of the continuous outcome for the control group (used if \code{outcome_type = "continuous"}, default = 0).
#' @param outcome_diff Numeric, treatment effect on the outcome. For continuous outcomes, this is added to the control mean for treatment group(s). For binary outcomes, this is ignored (use probabilities instead). Default = 0 (no treatment effect).
#' @param outcome_sd Numeric, standard deviation for the continuous outcome (default = 1). Ignored for binary outcomes.
#' @param outcome_prob_control Numeric, probability of outcome = "Yes" for control group (used if \code{outcome_type = "binary"}, default = 0.5).
#' @param outcome_prob_treatment Numeric, probability of outcome = "Yes" for treatment group (used if \code{outcome_type = "binary"} and exactly two arms, default = 0.5).
#' @param outcome_means Named numeric vector of outcome means by arm (optional alternative to using \code{outcome_mean_control} and \code{outcome_diff}).
#' @param outcome_probs Named numeric vector of outcome "Yes" probabilities by arm (optional alternative to using \code{outcome_prob_control} and \code{outcome_prob_treatment}).
#' @param cluster_var Character, name of cluster variable in \code{data} for random effect (only used for continuous outcomes; e.g., "Site" for site-level effect).
#' @param cluster_effect_sd Numeric, standard deviation of random effect for clusters (only for continuous outcomes, default = 0).
#'
#' @return The input data frame with a new column \code{Outcome}. For continuous outcomes, this is numeric. For binary outcomes, this is a factor with levels "No" and "Yes". Patients who dropped out or had an event before the outcome measurement are assigned \code{NA} for the outcome.
#'
#' @details For continuous outcomes, if multiple arms are present and \code{outcome_means} is provided, those values are used for each arm's mean. Otherwise, \code{outcome_mean_control} is used for the first arm (assumed control) and \code{outcome_diff} is added for other arms. A random normal error with SD \code{outcome_sd} is added for each patient. If \code{cluster_var} is given (and \code{cluster_effect_sd > 0}), a random intercept per cluster is added to the outcome to simulate cluster-level variability.
#'
#' For binary outcomes, if two arms are present, \code{outcome_prob_control} and \code{outcome_prob_treatment} define the success probabilities for control and treatment respectively (or use \code{outcome_probs} for multiple arms). The outcome is generated as "Yes"/"No" for each patient. If a patient dropped out or had a terminal event prior to the endpoint, their outcome is set to \code{NA} (assuming missing data due to dropout/death).
#'
#' @examples
#' df <- simulate_patients(5)
#' df <- simulate_allocation(df)
#' # Continuous outcome: mean 50 in control, treatment +5 difference
#' df <- simulate_outcomes(df, outcome_type = "continuous",
#'                         outcome_mean_control = 50, outcome_diff = 5, outcome_sd = 10)
#' # Binary outcome: 30% responders in control vs 60% in treatment
#' df <- simulate_outcomes(df, outcome_type = "binary",
#'                         outcome_prob_control = 0.3, outcome_prob_treatment = 0.6)
#' head(df$Outcome)
#'
#' @export
simulate_outcomes <- function(data,
                              outcome_type = c("continuous", "binary"),
                              outcome_mean_control = 0, outcome_diff = 0, outcome_sd = 1,
                              outcome_prob_control = 0.5, outcome_prob_treatment = 0.5,
                              outcome_means = NULL, outcome_probs = NULL,
                              cluster_var = NULL, cluster_effect_sd = 0) {
  outcome_type <- match.arg(outcome_type)

  # Continuous outcome simulation
  if (outcome_type == "continuous") {
    # Prepare cluster offsets if needed
    cluster_offset <- NULL
    if (!is.null(cluster_var) && cluster_effect_sd > 0) {
      if (!(cluster_var %in% names(data))) {
        stop("cluster_var not found in data for outcome simulation.")
      }
      clusters <- unique(data[[cluster_var]])
      # Generate random effect per cluster
      cluster_effects <- stats::rnorm(length(clusters), mean = 0, sd = cluster_effect_sd)
      cluster_offset <- setNames(cluster_effects, clusters)
    }
    # Determine outcome mean for each arm
    arm_levels <- if ("Arm" %in% names(data)) levels(data$Arm) else "All"
    means_by_arm <- list()
    if (!is.null(outcome_means)) {
      # use provided means, fill missing with control if needed
      for (arm in arm_levels) {
        means_by_arm[[arm]] <- if (!is.null(outcome_means[arm])) outcome_means[arm] else outcome_mean_control
      }
    } else {
      if (length(arm_levels) == 1) {
        # single group
        means_by_arm[[arm_levels[1]]] <- outcome_mean_control
      } else {
        # multiple arms: assume first is control
        means_by_arm[[arm_levels[1]]] <- outcome_mean_control
        for (arm in arm_levels[-1]) {
          means_by_arm[[arm]] <- outcome_mean_control + outcome_diff
        }
      }
    }
    # Simulate outcome for each patient
    outcomes <- numeric(nrow(data))
    for (i in seq_len(nrow(data))) {
      arm_i <- if ("Arm" %in% names(data)) as.character(data$Arm[i]) else arm_levels[1]
      mu <- if (!is.null(means_by_arm[[arm_i]])) means_by_arm[[arm_i]] else outcome_mean_control
      # Add cluster random effect if applicable
      if (!is.null(cluster_offset) && !is.na(data[i, cluster_var])) {
        cl <- as.character(data[i, cluster_var])
        if (cl %in% names(cluster_offset)) {
          mu <- mu + cluster_offset[[cl]]
        }
      }
      outcomes[i] <- stats::rnorm(1, mean = mu, sd = outcome_sd)
    }
    data$Outcome <- outcomes
  }

  # Binary outcome simulation
  if (outcome_type == "binary") {
    arm_levels <- if ("Arm" %in% names(data)) levels(data$Arm) else "All"
    probs_by_arm <- list()
    if (!is.null(outcome_probs)) {
      # use provided probabilities for each arm
      for (arm in arm_levels) {
        if (arm %in% names(outcome_probs)) {
          probs_by_arm[[arm]] <- outcome_probs[[arm]]
        } else {
          stop("Missing probability for arm: ", arm, " in outcome_probs.")
        }
      }
    } else {
      if (length(arm_levels) == 1) {
        probs_by_arm[[arm_levels[1]]] <- outcome_prob_control
      } else if (length(arm_levels) == 2) {
        # two arms scenario
        probs_by_arm[[arm_levels[1]]] <- outcome_prob_control
        probs_by_arm[[arm_levels[2]]] <- outcome_prob_treatment
      } else {
        stop("For multiple arms, provide outcome_probs for each arm for binary outcomes.")
      }
    }
    # Simulate binary outcome as Yes/No
    outcome_factor <- rep(NA_character_, nrow(data))
    for (i in seq_len(nrow(data))) {
      arm_i <- if ("Arm" %in% names(data)) as.character(data$Arm[i]) else arm_levels[1]
      p <- if (!is.null(probs_by_arm[[arm_i]])) probs_by_arm[[arm_i]] else outcome_prob_control
      outcome_factor[i] <- if (stats::runif(1) < p) "Yes" else "No"
    }
    data$Outcome <- factor(outcome_factor, levels = c("No","Yes"))
  }

  # If patient dropped out or had event, mark outcome as missing (NA)
  if ("Dropped" %in% names(data)) {
    data$Outcome[data$Dropped == TRUE] <- NA
  }
  if ("EventStatus" %in% names(data)) {
    data$Outcome[data$EventStatus == 1] <- NA
  }

  return(data)
}
