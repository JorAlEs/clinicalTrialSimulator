library(testthat)
library(clinicalTrialSimulator)

test_that("simulate_patients generates correct number of patients and columns", {
  df <- simulate_patients(n = 10, countries = 2, sites_per_country = 3, mean_age = 40, sd_age = 5, prop_female = 0.7)
  expect_equal(nrow(df), 10)
  # Check required columns exist
  expect_true(all(c("ID", "Country", "Site", "Age", "Sex") %in% names(df)))
  # Check data types
  expect_type(df$ID, "integer")
  expect_s3_class(df$Country, "factor")
  expect_s3_class(df$Site, "factor")
  expect_type(df$Age, "integer")
  expect_s3_class(df$Sex, "factor")
  # Check factor levels counts
  expect_equal(nlevels(df$Country), 2)
  expect_equal(nlevels(df$Site), 2 * 3)
  expect_equal(nlevels(df$Sex), 2)
  # Age should be within [18,90]
  expect_true(all(df$Age >= 18 & df$Age <= 90))
  # IDs should be unique
  expect_equal(length(unique(df$ID)), 10)
})
