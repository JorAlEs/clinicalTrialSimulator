library(testthat)
library(clinicalTrialSimulator)

test_that("simulate_longitudinal_outcomes generates expected values for deterministic scenario", {
  # Two patients, one control, one treatment
  df <- simulate_patients(2, countries = 1, sites_per_country = 2)
  df$Arm <- factor(c("Control", "Treatment"), levels = c("Control", "Treatment"))
  # Use fixed visits
  visits <- simulate_visits(df, visit_times = c(0, 10))
  # No variability scenario: baseline_sd=0, error_sd=0, control_slope=0, treatment_slope=-5
  # baseline_mean=100, so baseline for both =100.
  # Control: stays 100 at time10; Treatment: declines to 100 + (-5*10) = 50 at time10.
  visit_data <- simulate_longitudinal_outcomes(visits, df,
                                               baseline_mean = 100, baseline_sd = 0,
                                               control_slope = 0, treatment_slope = -5,
                                               error_sd = 0,
                                               cluster_var = "Site", cluster_effect_sd = 0)
  # Extract outcomes for each patient at each time
  out_mat <- reshape(visit_data[, c("ID","Time","Outcome")], idvar = "ID", timevar = "Time", direction = "wide")
  # Outcomes should be:
  # Patient1 (Control): Outcome.0 = 100, Outcome.10 = 100
  # Patient2 (Treatment): Outcome.0 = 100, Outcome.10 = 50
  expect_equal(out_mat$Outcome.0[out_mat$ID == 1], 100)
  expect_equal(out_mat$Outcome.10[out_mat$ID == 1], 100)
  expect_equal(out_mat$Outcome.0[out_mat$ID == 2], 100)
  expect_equal(out_mat$Outcome.10[out_mat$ID == 2], 50)
  # Check that Arm and site columns are present in visit_data
  expect_true(all(c("Arm","Site","Country") %in% names(visit_data)))
  # Arm assignments should correspond to patients
  expect_equal(as.character(visit_data$Arm[visit_data$ID==1][1]), "Control")
  expect_equal(as.character(visit_data$Arm[visit_data$ID==2][1]), "Treatment")
})

test_that("simulate_longitudinal_outcomes handles cluster effects (random intercepts)", {
  # Create data with 2 clusters (sites), one patient per site
  df <- data.frame(
    ID = 1:2,
    Site = factor(c("Site1","Site2"), levels = c("Site1","Site2")),
    Country = factor(c("Country1","Country1"), levels = "Country1"),
    Arm = factor(c("Control","Control"), levels = c("Control","Treatment"))
  )
  visits <- simulate_visits(df, visit_times = c(0))
  set.seed(999)
  vd <- simulate_longitudinal_outcomes(visits, df, baseline_mean = 0, baseline_sd = 0,
                                       control_slope = 0, treatment_slope = 0,
                                       error_sd = 0,
                                       cluster_var = "Site", cluster_effect_sd = 5)
  # Since baseline_sd and error_sd are 0, Outcome at baseline equals the cluster offset.
  # We expect two different Outcome values (one per site) and their difference reflects the random site offset difference.
  outcomes <- vd$Outcome[order(vd$ID)]
  expect_true(length(unique(outcomes)) >= 1)
  # If cluster_effect_sd > 0, with 2 clusters, outcomes likely differ.
  expect_false(isTRUE(all.equal(outcomes[1], outcomes[2])))
})
