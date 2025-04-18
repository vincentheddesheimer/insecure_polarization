# Insecure Polarization: Economic Insecurity and Affective Polarization

This repository contains code and data for analyzing the relationship between economic insecurity and affective polarization using the LISS panel survey data from the Netherlands.

## Project Overview

This project investigates how economic insecurity affects affective polarization using longitudinal data from the LISS panel survey. The analysis focuses on three main treatments:
1. Job loss
2. Personal income drops
3. Household income drops

And examines their effects on:
1. Affective polarization (measured through distance and spread metrics)
2. Ingroup affinity and outgroup aversion
3. Heterophily in close social ties
4. Generalized trust

## Data

The LISS panel survey consists of 5,000 households (approximately 7,500 individuals) in the Netherlands, based on a true probability sample from the population register. Panel members complete monthly online questionnaires and are compensated for participation. The core longitudinal study is repeated yearly, with the first wave administered in October 2007 and the 15th wave in March 2023.

### Key Variables

#### Treatments
- **Job loss**: Coded as 1 if respondent reports being a job seeker following job loss or exempted from job seeking
- **Personal income drop**: Measured using a 12-point scale for net monthly income, with a "sizable drop" defined as a relative decrease of 25% or greater
- **Household income drop**: Defined as a decrease of 10% or more in net household income

#### Outcomes
- **Affective polarization**: Measured using:
  - Distance metric: Mean distance between most favored party and all other parties
  - Spread metric: Spread of weighted like-dislike scores
- **Ingroup affinity and outgroup aversion**: Based on maximum and minimum party thermometer scores
- **Heterophily**: Novel measure of demographic distance between respondents and their close social ties
- **Generalized trust**: Standard measures from the panel study

## Code Structure

The code is organized in the following structure:

```
code/
├── build/
│   └── liss/
│       └── 01_Liss_data_compilation.R  # Data processing and variable construction
└── analyze/
    ├── 01_descriptives.R               # Descriptive statistics and data exploration
    ├── 02_unemployment_all.R           # Main unemployment analysis
    ├── 02b_unemployment_gender.R       # Gender-specific unemployment effects
    ├── 02c_unemployment.R              # Additional unemployment analyses
    ├── 03_employment.R                 # Employment status analysis
    ├── 04_hh_income_loss.R             # Household income loss analysis
    └── 05_income_loss.R                # Personal income loss analysis
```

### Data Processing

The main data processing script (`01_Liss_data_compilation.R`) performs the following operations:

1. Loads and processes the LISS panel survey data
2. Constructs treatment variables:
   - Job loss indicators
   - Income drop measures
   - Household income changes
3. Creates outcome variables:
   - Affective polarization measures
   - Heterophily measures
   - Trust measures
4. Calculates various distance metrics for social ties
5. Handles missing data and creates analysis-ready datasets

### Analysis

The analysis scripts perform the following analyses:

1. **Descriptive Statistics** (`01_descriptives.R`):
   - Summary statistics for all key variables
   - Missing data analysis
   - Basic data exploration

2. **Unemployment Analysis** (`02_unemployment_all.R`, `02b_unemployment_gender.R`, `02c_unemployment.R`):
   - Event study analysis of unemployment effects
   - Gender-specific effects of unemployment
   - Heterogeneity analysis by demographic characteristics
   - Robustness checks and alternative specifications

3. **Employment Analysis** (`03_employment.R`):
   - Analysis of employment status changes
   - Reemployment effects
   - Employment stability analysis

4. **Income Loss Analysis** (`04_hh_income_loss.R`, `05_income_loss.R`):
   - Household income loss effects
   - Personal income loss effects
   - Income volatility analysis
   - Comparison of different income loss thresholds

## Requirements

The analysis requires the following R packages:
- tidyverse
- data.table
- scales
- dlookr
- did2s
- haschaR
- modelsummary
- kableExtra

## Usage

1. Place the LISS data file (`liss_combined.csv`) in the appropriate directory
2. Run the data compilation script to process the data
3. Run the analysis scripts in sequence to reproduce the results
4. The processed data will be saved as `liss.csv` in the data directory

## Notes

- The LISS data is not included in this repository due to data access restrictions
- Users need to obtain the data directly from the LISS panel survey
- The code assumes specific file paths that may need to be adjusted based on local setup
- Analysis scripts should be run in sequence as they build upon each other

## References

- LISS Panel Survey: https://www.lissdata.nl/
- Dunbar, R. I. M. (2018). The anatomy of friendship. Trends in Cognitive Sciences, 22(1), 32-51.
- Wagner, M. (2021). Affective polarization in multiparty systems. Electoral Studies, 69, 102199.