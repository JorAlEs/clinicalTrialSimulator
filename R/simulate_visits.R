#' Simulate Visit Dates for Clinical Trial Patients
#'
#' Generates scheduled visit dates for each patient based on treatment start date,
#' with optional dropout and visit date jittering.
#'
#' @param df Data frame of patients including `TRTSDT` column as treatment start date.
#' @param visit_schedule Numeric vector of planned visit days from treatment start (e.g., c(0, 14, 28)).
#' @param jitter_days Integer; maximum number of days to randomly shift each visit date (+/-).
#' @param dropout_rate Numeric [0,1]; proportion of patients who drop out randomly before a given visit.
#' @param seed Optional integer to ensure reproducibility.
#'
#' @return Data frame with new columns `VISIT1DT`, `VISIT2DT`, etc., one for each visit.
#'
#' @export


simulate_visits <- function(df, visit_schedule = c(0, 7, 14, 30, 60, 90),
                            jitter_days = 2, # permite pequeñas desviaciones
                            dropout_rate = 0.2,
                            seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(df)
  n_visits <- length(visit_schedule)

  # Simular abandono antes de una visita determinada
  dropout_visit <- sample(c(NA, visit_schedule[-1]), n, replace = TRUE,
                          prob = c(1 - dropout_rate, rep(dropout_rate / (n_visits - 1), n_visits - 1)))

  visit_dates <- lapply(visit_schedule, function(day) {
    jitter <- sample(-jitter_days:jitter_days, n, replace = TRUE)
    as.Date(df$TRTSDT) + day + jitter
  })

  names(visit_dates) <- paste0("VISIT", seq_along(visit_schedule), "DT")
  visit_df <- as.data.frame(visit_dates)

  # Aplicar abandono (NA en visitas posteriores al abandono)
  for (i in seq_len(n_visits)) {
    day <- visit_schedule[i]
    visit_df[[i]][!is.na(dropout_visit) & dropout_visit < day] <- NA
  }

  return(cbind(df, visit_df))
}
