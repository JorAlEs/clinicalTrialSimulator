library(testthat)
library(clinicalTrialSimulator)

test_that("export_adam_format compiles ADSL correctly", {
  # Simulate a small trial scenario with all components
  df <- simulate_patients(5, countries = 2, sites_per_country = 2)
  df <- simulate_allocation(df, arm_names = c("Control","Treatment"), ratio = c(1,1))
  df <- simulate_survival(df, median_time_control = 6, hr = 0.8, study_duration = 12)
  df <- simulate_dropout(df, dropout_rate = 0.3, study_duration = 12, p_ae = 0.2, dropout_hr_ae = 2)
  df <- simulate_outcomes(df, outcome_type = "continuous", outcome_mean_control = 100, outcome_diff = -10, outcome_sd = 5)
  adsl <- export_adam_format(df, study_id = "TESTSTUDY")
  # Check one row per subject
  expect_equal(nrow(adsl), length(unique(df$ID)))
  # Required columns in ADSL
  expect_true(all(c("STUDYID","USUBJID","ID","Arm","Age","Sex") %in% names(adsl)))
  # STUDYID is constant and matches input
  expect_equal(length(unique(adsl$STUDYID)), 1)
  expect_equal(unique(adsl$STUDYID), "TESTSTUDY")
  # USUBJID contains STUDYID and ID
  expect_true(all(startsWith(adsl$USUBJID, "TESTSTUDY-")))
  # If Outcome present in df, it should be in ADSL
  if ("Outcome" %in% names(df)) {
    expect_true("Outcome" %in% names(adsl))
  }
  # If survival present, EventTime and EventStatus included
  if ("EventTime" %in% names(df)) {
    expect_true("EventTime" %in% names(adsl))
    expect_true("EventStatus" %in% names(adsl))
  }
  # If dropout present, DropoutTime and Dropped included
  if ("DropoutTime" %in% names(df)) {
    expect_true("DropoutTime" %in% names(adsl))
    expect_true("Dropped" %in% names(adsl))
  }
})
