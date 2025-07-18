# Economic Insecurity Increases Affective Polarization and Outgroup-Aversion

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

## System Requirements

### Software Dependencies
- **R**: Version 4.2.0 or higher
- **RStudio**: Version 2022.07.0 or higher (recommended)
- **Operating Systems**: macOS 10.15+, Windows 10+, Ubuntu 18.04+

### R Packages
The following R packages are required with specific versions:
- `tidyverse` (>= 1.3.0)
- `data.table` (>= 1.14.0)
- `scales` (>= 1.2.0)
- `dlookr` (>= 0.7.0)
- `did2s` (>= 0.1.0)
- `haschaR` (>= 0.1.0)
- `modelsummary` (>= 0.10.0)
- `kableExtra` (>= 1.3.0)
- `pacman` (>= 0.5.0)
- `janitor` (>= 2.1.0)

### Hardware Requirements
- **Minimum**: 8GB RAM, 2GB free disk space
- **Recommended**: 16GB RAM, 5GB free disk space
- **Processor**: Any modern multi-core processor (Intel i5/AMD Ryzen 5 or better)

## Installation Guide

### Step 1: Install R and RStudio
1. Download and install R from [https://cran.r-project.org/](https://cran.r-project.org/)
2. Download and install RStudio from [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

### Step 2: Install Required R Packages
Open R or RStudio and run the following commands:

```r
# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Install haschaR from GitHub
if (!require("remotes")) install.packages("remotes")
devtools::install_github("hhilbig/haschaR")

# Install all required packages
pacman::p_install(
  tidyverse, data.table, scales, dlookr, did2s,
  modelsummary, kableExtra, janitor
)

```

### Step 3: Clone or Download Repository
```bash
git clone https://github.com/vincentheddesheimer/insecure_polarization.git
cd insecure_polarization
```

### Step 4: Verify Installation
Test that everything is working by running a simple analysis:
```r
# Load packages
library(tidyverse)
library(data.table)

# Test with the included dataset
df <- fread("data/liss.csv")
print(paste("Dataset loaded successfully with", nrow(df), "observations"))
```

**Typical Install Time**: 15-30 minutes on a standard desktop computer (depending on internet speed and system performance).

## Instructions for Use

### Step 1: Prepare Your Data
1. Obtain the LISS panel survey data from [https://www.lissdata.nl/](https://www.lissdata.nl/)
2. Run the files in `code/build/liss/clean/' to clean the raw data.
2. Place the raw data file (`liss_combined.csv`) in the `data/` directory
3. Ensure the data file contains all required variables (see Data section below)

### Step 2: Data Processing
```r
# Set working directory to repository root
setwd("path/to/insecure_polarization")

# Run data compilation
source("code/build/liss/01_Liss_data_compilation.R")
```

### Step 3: Run Analysis
Execute the analysis scripts in sequence:

```r
# 1. Descriptive statistics
source("code/analyze/01_descriptives.R")

# 2. Main unemployment analysis
source("code/analyze/02_unemployment_all.R")

# 3. Gender-specific unemployment effects
source("code/analyze/02b_unemployment_gender.R")

# 4. Additional unemployment analyses
source("code/analyze/02c_unemployment.R")

# 5. Employment analysis
source("code/analyze/03_employment.R")

# 6. Household income loss analysis
source("code/analyze/04_hh_income_loss.R")

# 7. Personal income loss analysis
source("code/analyze/05_income_loss.R")
```

### Step 4: Access Results
Results will be saved in:
- `results/tables/` - Statistical tables and summary statistics
- `results/figures/` - Plots and visualizations
- `results/models/` - Model objects and coefficients

## Data

The LISS panel survey consists of 5,000 households (approximately 7,500 individuals) in the Netherlands, based on a true probability sample from the population register. Panel members complete monthly online questionnaires and are compensated for participation. The core longitudinal study is repeated yearly, with the first wave administered in October 2007 and the 15th wave in March 2023.

### Required Variables
Your data file must contain the following variables:
- `nomem_encr`: Unique respondent identifier
- `wave`: Survey wave number
- `age`: Respondent age
- `gender`: Respondent gender
- `education_cat`: Education category
- `origin`: Ethnic origin
- `unemployed`: Unemployment status
- `employed`: Employment status
- `net_monthly_income_cat`: Income category
- `household_income`: Household income
- `generalized_trust`: Trust measure
- Party preference variables (ending with `_politics`)
- Close contact variables (ending with `_age`, `_gender`, `_education`, `_origin`, `_politics`)

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
│       ├── 01_Liss_data_compilation.R  # Main data processing and variable construction
│       └── clean/                       # Individual data transformation scripts
│           ├── 01_LISS_politics_transformation.R      # Political variables processing
│           ├── 02_LISS_assets_transformation.R        # Assets and wealth variables
│           ├── 03_LISS_housing_transformation.R       # Housing variables
│           ├── 04_LISS_income_transformation.R        # Income variables
│           ├── 05_LISS_background_transformation.R    # Background demographics
│           ├── 06_LISS_personality_transformationR.R  # Personality variables
│           ├── 07_LISS_socialintegration_transformationR.R  # Social integration
│           └── 08_LISS_combine.R                      # Combine all transformed data
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

The data processing consists of two main components:

**Individual Transformation Scripts** (`code/build/liss/clean/`):
1. **Political Variables** (`01_LISS_politics_transformation.R`): Processes party preferences and political attitudes
2. **Assets and Wealth** (`02_LISS_assets_transformation.R`): Handles financial assets and wealth measures
3. **Housing Variables** (`03_LISS_housing_transformation.R`): Processes housing-related variables
4. **Income Variables** (`04_LISS_income_transformation.R`): Constructs income measures and changes
5. **Background Demographics** (`05_LISS_background_transformation.R`): Processes demographic variables
6. **Personality Variables** (`06_LISS_personality_transformationR.R`): Handles personality measures
7. **Social Integration** (`07_LISS_socialintegration_transformationR.R`): Processes social network variables
8. **Data Combination** (`08_LISS_combine.R`): Combines all transformed datasets

**Main Processing Script** (`01_Liss_data_compilation.R`):
1. Loads the combined LISS panel survey data
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

## Reproduction Instructions

To reproduce all quantitative results from the manuscript:

1. **Data Access**: Obtain the full LISS panel survey data from [https://www.lissdata.nl/](https://www.lissdata.nl/)
2. **Data Processing**: Run `code/build/liss/01_Liss_data_compilation.R`
3. **Main Analysis**: Execute all scripts in `code/analyze/` in numerical order
4. **Results Compilation**: The scripts will automatically generate all tables and figures referenced in the manuscript

**Expected Reproduction Time**: 45-60 minutes on a standard desktop computer with 16GB RAM.

## Troubleshooting

### Common Issues

1. **Package Installation Errors**:
   - Update R to the latest version
   - Install packages individually if batch installation fails
   - On Windows, ensure Rtools is installed for packages requiring compilation

2. **Memory Issues**:
   - Close other applications to free up RAM
   - Process data in smaller chunks if needed
   - Consider using a machine with more RAM for large datasets

3. **File Path Errors**:
   - Ensure working directory is set to repository root
   - Check that data files are in the correct directories
   - Use absolute paths if relative paths fail

4. **Data Format Issues**:
   - Verify that your data file matches the required variable structure
   - Check for missing or incorrectly named variables
   - Ensure proper data types (numeric, character, etc.)

## Notes

- The raw LISS data is not included in this repository due to data access restrictions
- Users need to obtain the data directly from the LISS panel survey
- The code assumes specific file paths that may need to be adjusted based on local setup
- Analysis scripts should be run in sequence as they build upon each other

## References

- LISS Panel Survey: https://www.lissdata.nl/
- Dunbar, R. I. M. (2018). The anatomy of friendship. Trends in Cognitive Sciences, 22(1), 32-51.
- Wagner, M. (2021). Affective polarization in multiparty systems. Electoral Studies, 69, 102199.

## License

This code is licensed under the MIT License. See the [LICENSE](./LICENSE) file for details.
