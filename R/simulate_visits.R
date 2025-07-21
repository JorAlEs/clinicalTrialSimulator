#' Generate scheduled visits for each patient
#'
#' This function creates a dataset of planned visit times for each patient, taking into account censoring due to dropouts or events. After a patient drops out or has an event (e.g., death), no further visits are generated.
#'
#' @param data A data frame of patients. If the data contains \code{DropoutTime}/\code{Dropped} and/or \code{EventTime}/\code{EventStatus} columns, these will be used to censor visits.
#' @param visit_times Numeric vector of planned visit times (in the same time units as other simulation outputs). For example, \code{c(0, 3, 6, 9, 12)} for monthly visits over a year (if time in months). Include 0 for baseline.
#'
#' @return A data frame with one row per patient per scheduled visit (up to any censoring point). Columns:
#' \itemize{
#'   \item \code{ID}: Patient ID (matching \code{data$ID}).
#'   \item \code{Time}: Scheduled visit time.
#' }
#'
#' @details This function expands the patient data by visit time, but omits any visits that occur after a patient has dropped out or experienced a terminal event. If neither dropout nor event information is present, all specified \code{visit_times} will be returned for each patient.
#'
#' @examples
#' df <- simulate_patients(3)
#' df$Dropped <- c(FALSE, TRUE, FALSE)
#' df$DropoutTime <- c(NA, 5, NA)
#' df$EventStatus <- c(0, 0, 1)
#' df$EventTime <- c(NA, NA, 5)
#' simulate_visits(df, visit_times = c(0, 5, 10))
#'
#' @export
simulate_visits <- function(data, visit_times) {
  if (missing(visit_times) || length(visit_times) < 1) {
    stop("visit_times must be a numeric vector of at least one time (include 0 for baseline).")
  }

  visit_times <- sort(visit_times)
  n <- nrow(data)
  visits_list <- vector("list", n)

  for (i in seq_len(n)) {
    pid <- data$ID[i]
    censor_time <- Inf

    # Dropout censoring
    if ("Dropped" %in% names(data) && isTRUE(data$Dropped[i]) && !is.na(data$DropoutTime[i])) {
      censor_time <- min(censor_time, data$DropoutTime[i])
    }

    # Event censoring
    if ("EventStatus" %in% names(data) && data$EventStatus[i] == 1 && !is.na(data$EventTime[i])) {
      censor_time <- min(censor_time, data$EventTime[i])
    }

    # Visits up to and excluding censor_time
    times_i <- visit_times[visit_times < censor_time]

    visits_list[[i]] <- data.frame(ID = pid, Time = times_i, stringsAsFactors = FALSE)
  }

  visits_df <- do.call(rbind, visits_list)
  rownames(visits_df) <- NULL
  return(visits_df)
}
