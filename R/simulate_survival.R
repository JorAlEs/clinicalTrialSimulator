#' Simulate time-to-event (survival) outcomes
#'
#' This function simulates a survival time (e.g., time to event or censoring) for each patient, with an option to specify treatment effect on hazard. It assumes an exponential survival distribution for simplicity.
#'
#' @param data A data frame of patients that includes a treatment group column (typically \code{Arm} from \code{\link{simulate_allocation}}). If no \code{Arm} column is present or only one level, all patients are treated as a single group.
#' @param median_time_control Numeric, median event time for the control group (e.g., median survival in control arm).
#' @param hr Numeric or named numeric vector specifying hazard ratios for treatment arms relative to control. If a single numeric is given (default = 1), and exactly two arms exist, it is interpreted as the hazard ratio for the treatment arm vs control. If multiple arms or a named vector is provided, each arm's hazard is adjusted accordingly (the control arm should have hazard ratio 1).
#' @param study_duration Numeric, maximum follow-up time (e.g., study end in time units). If provided, events beyond this time are censored at \code{study_duration}. (Default = NULL, meaning no administrative censoring apart from any later dropout handling.)
#'
#' @return The input data frame with two new columns:
#' \itemize{
#'   \item \code{EventTime}: Simulated event time or censoring time.
#'   \item \code{EventStatus}: Event occurrence indicator (1 = event occurred, 0 = censored by end of follow-up).
#' }
#'
#' @details Exponential survival times are generated. The control arm event rate is derived from \code{median_time_control} (rate = log(2)/median). Hazard ratios < 1 indicate longer survival in treatment arms (reduced hazard). For multiple arms, provide hazard ratios as a named vector (names matching \code{Arm} levels; the control arm can be omitted or set to 1). If no \code{Arm} column is present in \code{data}, all patients are assumed to share the same baseline hazard.
#'
#' @examples
#' df <- simulate_patients(100)
#' df <- simulate_allocation(df)
#' # Simulate survival with median 12 months in control, and hazard ratio 0.75 for treatment
#' df <- simulate_survival(df, median_time_control = 12, hr = 0.75, study_duration = 24)
#' head(df[c("Arm","EventTime","EventStatus")])
#'
#' @importFrom stats rnorm runif rexp
#' @export
simulate_survival <- function(data, median_time_control, hr = 1, study_duration = NULL) {
  if (missing(median_time_control) || median_time_control <= 0) {
    stop("median_time_control must be provided and > 0")
  }
  # Base hazard rate for control arm
  base_hazard <- log(2) / median_time_control

  # Determine unique arms
  arm_levels <- if ("Arm" %in% names(data)) levels(data$Arm) else "All"
  n_arms <- length(arm_levels)

  # Interpret hazard ratio parameter
  # Ensure hr is in named vector form for multiple arms
  hr_list <- NULL
  if (length(hr) > 1) {
    # if named, use directly; if unnamed and length == n_arms, assume order corresponds to levels
    if (!is.null(names(hr))) {
      hr_list <- hr
    } else if (length(hr) == n_arms) {
      hr_list <- setNames(hr, arm_levels)
    } else if (length(hr) == n_arms - 1) {
      # hr provided for arms excluding control
      control_name <- arm_levels[1]
      hr_list <- c(setNames(hr, arm_levels[-1]), setNames(1, control_name))
    } else {
      stop("Length of hr vector does not match number of arms.")
    }
  } else {
    # Single hazard ratio provided
    if (n_arms == 1) {
      hr_list <- setNames(1, arm_levels)  # only one group
    } else if (n_arms == 2) {
      # assume hr applies to second arm vs first arm (control)
      hr_list <- setNames(c(1, hr), arm_levels)
    } else {
      # multiple arms but single hr given: apply to all non-control arms
      hr_list <- setNames(rep(1, n_arms), arm_levels)
      hr_list[arm_levels[arm_levels != arm_levels[1]]] <- hr
      hr_list[arm_levels[1]] <- 1
    }
  }

  # Simulate event times for each patient
  event_times <- numeric(nrow(data))
  for (i in seq_len(nrow(data))) {
    arm_i <- if ("Arm" %in% names(data)) as.character(data$Arm[i]) else arm_levels[1]
    hazard_i <- base_hazard
    if (!is.null(hr_list)) {
      # if arm has a specified hazard ratio
      if (arm_i %in% names(hr_list)) {
        hazard_i <- base_hazard * hr_list[[arm_i]]
      }
    }
    # Ensure hazard is positive
    if (hazard_i <= 0) {
      event_times[i] <- Inf  # no event if hazard is zero or negative
    } else {
      event_times[i] <- stats::rexp(1, rate = hazard_i)
    }
  }

  # Apply administrative censoring at study_duration if provided
  status <- rep(1, nrow(data))
  if (!is.null(study_duration)) {
    for (j in seq_len(nrow(data))) {
      if (event_times[j] > study_duration) {
        event_times[j] <- study_duration
        status[j] <- 0  # censored at end of study
      }
    }
  }

  data$EventTime <- event_times
  data$EventStatus <- status
  return(data)
}
