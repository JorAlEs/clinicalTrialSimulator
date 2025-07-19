#' Simulate patient data
#'
#' This function creates a synthetic dataset of patients for use in a clinical trial simulation.
#'
#' @param n Number of patients to simulate.
#' @param strata Optional vector of stratum labels (e.g., c("EU", "US")).
#'
#' @return A data frame with patient IDs, age, sex, and optionally a stratum column.
#'
#' @examples
#' simulate_patients(100, strata = c("EU", "US"))
#'
#' @export
#'
#'

simulate_patients <- function(n,
                              strata = NULL,
                              study_id = "SIM001",
                              sites = 5,
                              countries = c("ES", "DE", "FR", "US", "IN"),
                              age_mean = 60,
                              age_sd = 10,
                              bmi_mean = 27,
                              bmi_sd = 4,
                              smoking_rate = 0.25,
                              comorbidities = TRUE,
                              visit_schedule = c(0, 30, 60, 90),
                              seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  df <- data.frame(
    STUDYID = study_id,
    USUBJID = paste0(study_id, "-", sprintf("%04d", 1:n)),
    SITEID = sample(paste0("SITE", sprintf("%02d", 1:sites)), n, replace = TRUE),
    COUNTRY = sample(countries, n, replace = TRUE),
    SEX = sample(c("M", "F"), n, replace = TRUE),
    AGE = round(rnorm(n, mean = age_mean, sd = age_sd)),
    BMI = round(rnorm(n, mean = bmi_mean, sd = bmi_sd), 1),
    RACE = sample(c("White", "Black", "Asian", "Other"), n, replace = TRUE,
                  prob = c(0.6, 0.15, 0.2, 0.05)),
    SMOKER = rbinom(n, 1, smoking_rate),
    ECOG = sample(0:2, n, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    ARMCD = NA_character_, # Placeholder for treatment assignment
    ARM = NA_character_
  )

  if (!is.null(strata)) {
    df$STRATUM <- sample(strata, n, replace = TRUE)
  }

  if (comorbidities) {
    df$HYPERTENSION <- rbinom(n, 1, 0.3)
    df$DIABETES <- rbinom(n, 1, 0.15)
    df$CVD <- rbinom(n, 1, 0.1)  # Cardiovascular disease
  }

  return(df)
}
