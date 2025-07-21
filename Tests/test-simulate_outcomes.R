library(testthat)
library(clinicalTrialSimulator)

test_that("simulate_outcomes (continuous) returns correct values with no randomness", {
  # 4 patients: 2 control, 2 treatment
  df <- simulate_patients(4, countries = 1, sites_per_country = 1)
  df$Arm <- factor(c("Control","Control","Treatment","Treatment"),
                   levels = c("Control","Treatment"))
  # Manually add scenario of dropouts and event:
  # Patient2 dropped, Patient3 had event, others fine.
  df$Dropped <- c(FALSE, TRUE, FALSE, FALSE)
  df$EventStatus <- c(0, 0, 1, 0)
  # Simulate continuous outcome: control mean=10, diff=5, sd=0 (no random noise)
  df_out <- simulate_outcomes(df, outcome_type = "continuous",
                              outcome_mean_control = 10, outcome_diff = 5, outcome_sd = 0)
  # Expected outcomes:
  # Patient1 (control, no drop/event) -> 10
  # Patient2 (control, dropped) -> NA
  # Patient3 (treatment, event occurred) -> NA
  # Patient4 (treatment, no drop/event) -> 15
  expect_equal(df_out$Outcome[ df_out$ID == 1 ], 10)
  expect_true(is.na(df_out$Outcome[ df_out$ID == 2 ]))
  expect_true(is.na(df_out$Outcome[ df_out$ID == 3 ]))
  expect_equal(df_out$Outcome[ df_out$ID == 4 ], 15)
})

test_that("simulate_outcomes (binary) yields correct extreme probabilities", {
  # Create a dataset with balanced arms
  df <- simulate_patients(10, countries = 1, sites_per_country = 1)
  # Assign exactly half to Control, half to Treatment
  df$Arm <- factor(rep(c("Control","Treatment"), each = 5),
                   levels = c("Control","Treatment"))
  # Simulate binary outcome: 0% control, 100% treatment
  df_bin <- simulate_outcomes(df, outcome_type = "binary",
                              outcome_prob_control = 0, outcome_prob_treatment = 1)
  # All control arm outcomes should be "No", all treatment "Yes"
  control_outcomes <- df_bin$Outcome[df_bin$Arm == "Control"]
  treat_outcomes <- df_bin$Outcome[df_bin$Arm == "Treatment"]
  expect_true(all(control_outcomes == "No"))
  expect_true(all(treat_outcomes == "Yes"))
  # Outcome factor levels should be "No","Yes"
  expect_equal(levels(df_bin$Outcome), c("No","Yes"))
})
