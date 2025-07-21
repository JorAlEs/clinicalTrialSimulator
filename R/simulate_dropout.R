#' Simulate dropouts (patient discontinuation) during the trial
#'
#' This function simulates dropout times for patients, i.e., when patients prematurely leave the study. It can incorporate an increased dropout hazard for patients experiencing an adverse event (AE), representing dropouts related to side effects or other issues.
#'
#' @param data A data frame of patients. Optionally includes columns for adverse events or others; not required.
#' @param dropout_rate Numeric between 0 and 1, the overall proportion of patients expected to drop out by the end of follow-up (study duration).
#' @param study_duration Numeric, the length of follow-up (time units) for the trial. Required for calibrating dropout times.
#' @param p_ae Numeric between 0 and 1, probability that a patient experiences a certain serious adverse event that can increase dropout risk (default = 0).
#' @param dropout_hr_ae Numeric, hazard ratio for dropout if the patient had an adverse event vs no event (default = 1, meaning no increase). For example, 2.0 means patients with the AE drop out at twice the hazard.
#'
#' @return The input data frame with additional columns:
#' \itemize{
#'   \item \code{SeriousAE}: Logical (TRUE/FALSE) indicating whether the patient had the simulated adverse event.
#'   \item \code{DropoutTime}: The time of dropout for those who dropped out, or \code{NA} if the patient did not drop out before study end.
#'   \item \code{Dropped}: Logical flag indicating if the patient dropped out (TRUE) or not (FALSE) by \code{study_duration}.
#' }
#'
#' @details Dropout times are modeled with an exponential distribution. The rate is set such that \code{dropout_rate} of patients (without the special AE) would drop out by \code{study_duration} (assuming an exponential survival for dropout). If \code{dropout_hr_ae > 1}, patients with the adverse event have a higher dropout hazard. The adverse event occurrence is simulated independently for each patient with probability \code{p_ae}. This allows simulating dropouts related to side effects (an original feature not present in many other packages). All patients with \code{dropout_rate = 1} (100\% dropout) are assigned a random dropout time before \code{study_duration}.
#'
#' Patients who experience an event (e.g., death) prior to dropping out should be considered as not dropping out (their participation ended due to the event). The function does not remove such patients, but subsequent analysis (e.g., in ADSL) will mark their event time and ignore the dropout.
#'
#' @examples
#' df <- simulate_patients(5)
#' df <- simulate_allocation(df)
#' # Simulate 20% dropout rate by 12 months, with 10% having serious AEs doubling dropout hazard
#' df <- simulate_dropout(df, dropout_rate = 0.2, study_duration = 12, p_ae = 0.1, dropout_hr_ae = 2)
#' head(df[c("DropoutTime","Dropped","SeriousAE")])
#'
#' @export
simulate_dropout <- function(data, dropout_rate, study_duration, p_ae = 0, dropout_hr_ae = 1) {
  if (missing(dropout_rate) || missing(study_duration)) {
    stop("dropout_rate and study_duration must be provided.")
  }
  if (dropout_rate < 0) stop("dropout_rate must be between 0 and 1.")
  if (p_ae < 0 || p_ae > 1) stop("p_ae must be between 0 and 1.")
  if (dropout_hr_ae <= 0) stop("dropout_hr_ae must be > 0.")
  n <- nrow(data)

  # Simulate adverse event occurrence for each patient
  ae_flag <- stats::runif(n) < p_ae
  data$SeriousAE <- ae_flag

  # Initialize dropout times
  dropout_time <- rep(NA_real_, n)

  # Handle extreme cases
  if (dropout_rate <= 0) {
    # No one drops out
    data$DropoutTime <- NA
    data$Dropped <- FALSE
  } else if (dropout_rate >= 1) {
    # Everyone drops out by end of study, assign random dropout times within [0, study_duration]
    dropout_time <- stats::runif(n, min = 0, max = study_duration)
    data$DropoutTime <- dropout_time
    data$Dropped <- rep(TRUE, n)
  } else {
    # Calculate base dropout hazard (lambda) such that P(dropout by T) = dropout_rate
    # P(dropout by T) = 1 - exp(-lambda * T) => lambda = -log(1 - dropout_rate) / T
    lambda_base <- -log(1 - dropout_rate) / study_duration

    # Determine hazard per patient (increase for those with AE)
    hazard_vec <- lambda_base * ifelse(ae_flag, dropout_hr_ae, 1)

    # Simulate dropout time from exponential distribution
    dropout_time <- stats::rexp(n, rate = hazard_vec)

    # Apply administrative censoring at study_duration for dropout as well
    dropped_flag <- dropout_time <= study_duration
    # Set times beyond study end to NA (no dropout within study)
    dropout_time[!dropped_flag] <- NA

    data$DropoutTime <- dropout_time
    data$Dropped <- dropped_flag
  }

  # If survival data is present, adjust event status/times for dropouts
  if ("EventTime" %in% names(data) && "EventStatus" %in% names(data)) {
    for (i in seq_len(n)) {
      if (!is.na(data$DropoutTime[i])) {
        # There was a dropout
        if (data$EventStatus[i] == 1) {
          # Patient had an event
          if (data$EventTime[i] <= data$DropoutTime[i]) {
            # Event happened before or at dropout -> treat as event occurred, no dropout
            data$DropoutTime[i] <- NA
            data$Dropped[i] <- FALSE
          } else {
            # Event would have happened after dropout -> censor event at dropout
            data$EventTime[i] <- data$DropoutTime[i]
            data$EventStatus[i] <- 0
            data$Dropped[i] <- TRUE
          }
        } else {
          # No event occurred by end of study originally
          if (!is.na(data$DropoutTime[i]) && data$DropoutTime[i] < ifelse(is.null(study_duration), Inf, study_duration)) {
            data$EventTime[i] <- data$DropoutTime[i]
            # EventStatus remains 0 (censored at dropout)
          }
        }
      }
    }
  }

  return(data)
}
