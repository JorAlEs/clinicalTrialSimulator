source("R/simulate_patients.R")
source("R/simulate_allocation.R")

patients <- simulate_patients(100, strata = c("EU", "US"))
trial <- simulate_allocation(patients, arms = c("Placebo", "Drug"), ratio = c(1, 2))

head(trial)
