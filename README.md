A modular R package for simulating realistic clinical trial datasets, including patient generation, treatment allocation, outcomes (continuous, binary, ordinal, longitudinal), survival, dropout, adverse events, and ADaM-compatible export.

🚀 Features
📋 Patient simulation with demographic and stratified characteristics

⚖️ Flexible treatment allocation with customizable randomization ratios

📈 Outcome simulation: continuous, binary, ordinal, and longitudinal

💀 Survival endpoint generation with censoring

🚪 Dropout modeling, including AE-related dropout logic

❗ Adverse event simulation by treatment arm and severity

🌍 Multilevel support (e.g., site, region, or country-level simulation)

📤 Export to CDISC ADaM-like ADSL format

🧪 Analysis flags: ITT, SAF, PPS

📦 Designed for SAP prototyping, training, and simulation-based power analysis

📦 Installation
Install from GitHub using devtools:


# install.packages("devtools")
```r
devtools::install_github("JorAlEs/clinicalTrialSimulator")
```
🔧 Requirements
R version ≥ 4.1.0

Suggested packages:

tidyverse

survival

lubridate

data.table

glue

testthat (for unit testing)

roxygen2, devtools, pkgdown (for development)

Install dependencies with:

```r
install.packages(c("tidyverse", "survival", "lubridate", "data.table", "glue"))
```

🧰 Main Functions
Function	Purpose
simulate_patients()	Generate baseline demographic data (age, sex, stratification, site)
simulate_allocation()	Assign treatments with ratio and stratification
simulate_outcomes()	Simulate outcomes (continuous, binary, ordinal, longitudinal)
simulate_survival()	Generate survival times and censoring
simulate_dropout()	Assign dropout flags and dropout times
simulate_adverse_events()	Simulate adverse events per arm and severity
simulate_post_dropout_followup()	Generate visit windows post-dropout or censoring
export_adam_format()	Export dataset in ADaM-like ADSL format

📖 Vignette
See the full simulation workflow in the package vignette:

```r
vignette("clinicalTrialSimulator_workflow")
```
Includes:

Full pipeline from patient generation to ADSL export

Longitudinal outcome simulation

AE-triggered dropout scenarios

Site-level simulation

🧪 Testing
Unit tests are included using testthat. To run all tests:

```r
devtools::test()
```

🛠 Roadmap
Planned features for v1.1 and beyond:

Export to additional ADaM domains (ADAE, ADLB, ADVS)

Integration with Shiny for interactive simulation

Quarto reporting templates

AE-based discontinuation logic

GUI-powered parameter input for non-programmers

📜 License
This package is released under the MIT License.
Feel free to use, modify, and distribute with attribution.

🙋 Contributing & Feedback
I welcome suggestions, feature requests, and collaborators!

Open an issue or submit a pull request.

🔗 Citation (example)
Alcántara J. (2025). clinicalTrialSimulator: Modular Clinical Trial Data Simulation in R. Version 1.0. https://github.com/JorAlEs/clinicalTrialSimulator

Let me know if you’d like this README in RMarkdown, with badges (build status, license, test coverage).
