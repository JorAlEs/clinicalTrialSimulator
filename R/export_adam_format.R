#' Export ADSL Dataset in ADaM-Compatible Format
#'
#' Converts a simulated patient dataset into a structured ADaM-compliant ADSL (subject-level) dataset,
#' including identification, treatment, demography, analysis population flags, and key dates.
#'
#' @param df Data frame with simulated patient data.
#' @param study_id Character; study identifier (e.g., "SIM001").
#' @param add_flags Logical; if TRUE, includes analysis population flags (ITTFL, SAFFL, PPSFL).
#'
#' @return Data frame structured according to ADaM ADSL standards.
#'
#' @export
export_adam_format <- function(df,
                               study_id = "SIM001",
                               add_flags = TRUE) {
  if (!"USUBJID" %in% names(df)) {
    df$USUBJID <- paste0(study_id, "-", sprintf("%04d", 1:nrow(df)))
  }
  df$STUDYID <- study_id

  # Ensure key dates
  df$RANDDT <- if ("RANDOMDT" %in% names(df)) as.Date(df$RANDOMDT) else as.Date("2024-01-01")
  df$TRTSDT <- if ("TRTSDT" %in% names(df)) as.Date(df$TRTSDT) else df$RANDDT + sample(0:3, nrow(df), replace = TRUE)
  df$TRTEDT <- if ("TRTEDT" %in% names(df)) as.Date(df$TRTEDT) else df$TRTSDT + sample(60:180, nrow(df), replace = TRUE)

  df$TRTDURD <- as.integer(df$TRTEDT - df$TRTSDT + 1)

  # Standard treatment columns
  if (!"ARM" %in% names(df)) df$ARM <- sample(c("Placebo", "Drug"), nrow(df), replace = TRUE)
  df$ARMCD <- as.character(match(df$ARM, unique(df$ARM)))
  df$ACTARM <- df$ARM
  df$ACTARMCD <- df$ARMCD

  # Demographics
  df$SEX <- if ("SEX" %in% names(df)) df$SEX else sample(c("M", "F"), nrow(df), replace = TRUE)
  df$AGE <- if ("AGE" %in% names(df)) df$AGE else round(rnorm(nrow(df), 60, 10))
  df$RACE <- if ("RACE" %in% names(df)) df$RACE else sample(c("White", "Black", "Asian", "Other"), nrow(df), replace = TRUE)
  df$ETHNIC <- sample(c("Hispanic", "Not Hispanic"), nrow(df), replace = TRUE)
  df$AGEGR1 <- cut(df$AGE, breaks = c(0, 50, 65, 100), labels = c("<50", "50–65", ">65"))

  # Analysis flags
  if (add_flags) {
    df$ITTFL <- "Y"
    df$SAFFL <- ifelse(!is.na(df$TRTSDT), "Y", "N")
    df$PPSFL <- ifelse(df$TRTDURD >= 90, "Y", "N")
  }

  # Discontinuation
  df$EOSDT <- if ("EOSDT" %in% names(df)) as.Date(df$EOSDT) else df$TRTEDT
  df$EOSSTT <- if ("EOSSTT" %in% names(df)) df$EOSSTT else "COMPLETED"
  df$DCSREAS <- if ("DCSREAS" %in% names(df)) df$DCSREAS else NA_character_

  # Death info (optional)
  df$DTHFL <- ifelse(runif(nrow(df)) < 0.05, "Y", "N")
  df$DTHDT <- ifelse(df$DTHFL == "Y", df$EOSDT - sample(1:10, nrow(df), replace = TRUE), NA)

  # Arrange columns in ADSL-like order
  df <- df[, c(
    "STUDYID", "USUBJID", "SITEID", "COUNTRY", "ARM", "ARMCD", "ACTARM", "ACTARMCD",
    "TRTSDT", "TRTEDT", "TRTDURD",
    "RANDDT", "EOSDT", "EOSSTT", "DCSREAS",
    "AGE", "AGEGR1", "SEX", "RACE", "ETHNIC",
    "ITTFL", "SAFFL", "PPSFL", "DTHFL", "DTHDT"
  )[c(
    c %in% colnames(df)
  )]]

  return(df)
}
