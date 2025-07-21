library(testthat)
library(clinicalTrialSimulator)

test_that("simulate_visits returns correct visits and respects censoring", {
  # Create a test patient dataset with known dropout/event scenarios
  df <- data.frame(
    ID = 1:3,
    Dropped = c(FALSE, TRUE, FALSE),
    DropoutTime = c(NA, 5, NA),
    EventStatus = c(0, 0, 1),
    EventTime = c(NA, NA, 5)
  )
  visits <- simulate_visits(df, visit_times = c(0, 5, 10))
  # Expected: Patient1 (no drop, no event) -> visits at 0,5,10
  # Patient2 (dropout at 5) -> visits at 0 (skip 5 and beyond)
  # Patient3 (event at 5) -> visits at 0 (skip 5 and beyond)
  expected <- data.frame(
    ID = c(1, 1, 1, 2, 3),
    Time = c(0, 5, 10, 0, 0)
  )
  # Ensure IDs and times match expected
  expect_equal(visits[order(visits$ID, visits$Time), ], expected[order(expected$ID, expected$Time), ])
  # Also check that each ID appears with correct number of visits
  counts <- table(visits$ID)
  expect_equal(as.numeric(counts["1"]), 3)
  expect_equal(as.numeric(counts["2"]), 1)
  expect_equal(as.numeric(counts["3"]), 1)
})
