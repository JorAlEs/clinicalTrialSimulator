library(testthat)
library(clinicalTrialSimulator)

test_that("simulate_survival produces non-negative times and appropriate hazard effects", {
  set.seed(123)
  df <- simulate_patients(100)
  df <- simulate_allocation(df, arm_names = c("Control", "Treatment"), ratio = c(1,1))
  # Test hazard ratio < 1 (treatment longer survival)
  df1 <- simulate_survival(df, median_time_control = 10, hr = 0.5, study_duration = 20)
  # All times should be >= 0 and <= study_duration
  expect_true(all(df1$EventTime >= 0 & df1$EventTime <= 20))
  # EventStatus should be 0 or 1
  expect_true(all(df1$EventStatus %in% c(0, 1)))
  # On average, treatment arm should have longer times than control arm
  control_times <- df1$EventTime[df1$Arm == "Control"]
  treat_times <- df1$EventTime[df1$Arm == "Treatment"]
  expect_gt(mean(treat_times), mean(control_times))
  # Test hazard ratio > 1 (treatment shorter survival)
  set.seed(124)
  df2 <- simulate_survival(df, median_time_control = 10, hr = 5, study_duration = 20)
  control_times2 <- df2$EventTime[df2$Arm == "Control"]
  treat_times2 <- df2$EventTime[df2$Arm == "Treatment"]
  expect_lt(mean(treat_times2), mean(control_times2))
})

test_that("simulate_survival handles single-arm scenario", {
  df <- simulate_patients(10)
  # No Arm column in df
  df_surv <- simulate_survival(df, median_time_control = 5)
  expect_true("EventTime" %in% names(df_surv))
  expect_equal(nlevels(factor(df_surv$EventStatus)), 2)  # 0 and 1 present (some events likely)
})
