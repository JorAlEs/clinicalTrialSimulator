#' Randomly allocate patients to treatment arms
#'
#' This function assigns each patient to a treatment arm (e.g., control or treatment) according to specified allocation ratios.
#'
#' @param data A data frame of patients (e.g., output of \code{\link{simulate_patients}}).
#' @param arm_names Character vector of arm names. Default is c("Control","Treatment") for a two-arm trial.
#' @param ratio Numeric vector of allocation ratios for each arm (same length as \code{arm_names}). Default is c(1,1) for equal allocation.
#'
#' @return The input data frame with an added factor column \code{Arm} indicating the treatment group for each patient.
#' @details Allocation is done by simple random sampling. The \code{ratio} vector is normalized to probabilities. If an arm receives no patients by chance (especially when sample size is small), the \code{Arm} factor will still include that level (to preserve all arm levels).
#'
#' @examples
#' df <- simulate_patients(10)
#' # Allocate patients equally to "Placebo" vs "Drug" arms
#' df <- simulate_allocation(df, arm_names = c("Placebo","Drug"), ratio = c(1,1))
#' table(df$Arm)
#'
#' @export
simulate_allocation <- function(data, arm_names = c("Control", "Treatment"), ratio = c(1, 1)) {
  if (length(arm_names) != length(ratio)) {
    stop("Length of arm_names must equal length of ratio vector.")
  }
  if (any(ratio < 0)) stop("Allocation ratios must be non-negative.")
  total_ratio <- sum(ratio)
  if (total_ratio <= 0) stop("Sum of ratios must be positive.")

  # Normalize ratios to probabilities
  probs <- ratio / total_ratio

  # Randomly assign arms according to probabilities
  assignments <- sample(arm_names, size = nrow(data), replace = TRUE, prob = probs)
  data$Arm <- factor(assignments, levels = arm_names)

  return(data)
}
