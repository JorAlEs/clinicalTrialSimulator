#' Simulate baseline patient demographics and site assignments
#'
#' This function simulates a baseline dataset of trial patients with demographic information
#' and site/country assignments. Each patient is assigned to a site (nested within country)
#' and given baseline characteristics such as age and sex.
#'
#' @param n Integer, number of patients to simulate.
#' @param countries Integer, number of countries in the trial (default = 1).
#' @param sites_per_country Integer, number of sites per country (default = 1). Total sites = countries * sites_per_country.
#' @param mean_age Numeric, mean age of patients (default = 50).
#' @param sd_age Numeric, standard deviation of age (default = 10).
#' @param prop_female Numeric, proportion of patients who are female (default = 0.5).
#'
#' @return A data frame with \code{n} rows, each representing a patient. Columns include:
#' \itemize{
#'   \item \code{ID}: Patient identifier (integer).
#'   \item \code{Country}: Factor indicating country (e.g., "Country1", "Country2", ...).
#'   \item \code{Site}: Factor indicating site (e.g., "Site1", "Site2", ...).
#'   \item \code{Age}: Patient age (years).
#'   \item \code{Sex}: Patient sex (factor with levels "Female","Male").
#' }
#'
#' @details The site assignments are distributed roughly equally among the sites. Age is drawn from a normal distribution and then bounded between 18 and 90. Sex is assigned according to the specified proportion of females. This function is typically the first step in the simulation workflow.
#'
#' @examples
#' # Simulate 5 patients in 2 countries with 2 sites each
#' df <- simulate_patients(n = 5, countries = 2, sites_per_country = 2)
#' head(df)
#'
#' @export
simulate_patients <- function(n, countries = 1, sites_per_country = 1,
                              mean_age = 50, sd_age = 10, prop_female = 0.5) {
  # Input validation
  if (n <= 0) stop("n must be a positive integer.")
  if (countries <= 0 || sites_per_country <= 0) stop("countries and sites_per_country must be positive.")
  if (prop_female < 0 || prop_female > 1) stop("prop_female must be between 0 and 1.")

  # Generate IDs
  ID <- 1:n

  # Total number of sites
  total_sites <- countries * sites_per_country

  # Assign site to each patient (approximately uniform across sites)
  site_ids <- sample(1:total_sites, size = n, replace = TRUE)
  # Determine country from site id
  country_ids <- ceiling(site_ids / sites_per_country)

  # Create factor levels for countries and sites
  Country <- factor(paste0("Country", country_ids),
                    levels = paste0("Country", 1:countries))
  Site <- factor(paste0("Site", site_ids),
                 levels = paste0("Site", 1:total_sites))

  # Simulate age
  Age <- stats::rnorm(n, mean = mean_age, sd = sd_age)
  Age <- round(Age)  # round to nearest year
  # Bound ages between 18 and 90 for realism
  Age[Age < 18] <- 18
  Age[Age > 90] <- 90
  Age <- as.integer(Age)

  # Simulate sex
  Sex_char <- sample(c("Female", "Male"), size = n, replace = TRUE,
                     prob = c(prop_female, 1 - prop_female))
  Sex <- factor(Sex_char, levels = c("Female", "Male"))

  # Combine into data frame
  data <- data.frame(ID = ID,
                     Country = Country,
                     Site = Site,
                     Age = Age,
                     Sex = Sex,
                     stringsAsFactors = FALSE)
  return(data)
}
