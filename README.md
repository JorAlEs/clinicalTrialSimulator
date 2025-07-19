# Clinical Trial Simulator in R

This repository provides a modular R-based simulator for clinical trials (Phase I–III). It allows generation of realistic synthetic datasets, simulating multiple aspects of trial design such as treatment allocation, patient response, dropout, and survival endpoints.

## 🚀 Features

- Randomized patient allocation (parallel arms, adjustable ratio)
- Continuous, binary, and time-to-event outcome simulation
- Dropout simulation and administrative censoring
- Stratification-ready patient generation
- Export to ADaM-like format
- Modular functions for reuse in different study designs

## 🧰 Requirements

- R (>= 4.1.0)
- Packages:
  - `tidyverse`
  - `survival`
  - `lubridate`
  - `data.table`

Install all at once:
```r
install.packages(c("tidyverse", "survival", "lubridate", "data.table"))
