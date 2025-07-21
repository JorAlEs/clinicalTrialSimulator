library(testthat)
library(clinicalTrialSimulator)

test_that("simulate_allocation assigns arms correctly", {
  df <- simulate_patients(20)
  df_alloc <- simulate_allocation(df, arm_names = c("A", "B", "C"), ratio = c(2, 1, 1))
  expect_true("Arm" %in% names(df_alloc))
  expect_s3_class(df_alloc$Arm, "factor")
  expect_equal(levels(df_alloc$Arm), c("A", "B", "C"))
  # The number of arms equals the provided arm_names length
  expect_equal(nlevels(df_alloc$Arm), 3)
  # All patients assigned one of the arms
  expect_true(all(df_alloc$Arm %in% c("A", "B", "C")))
  # Distribution roughly follows ratio (not exact, but all arms should have >0 members typically)
  tab <- table(df_alloc$Arm)
  expect_true(all(tab > 0))
})
