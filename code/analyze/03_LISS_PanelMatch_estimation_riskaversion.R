### # Heddesheimer, Bryson - Replication code - Month XX, 2023
# vincent.heddesheimer@princeton.edu
#
# 
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, PanelMatch, haschaR)

# Load LISS data
# df <- fread("data/liss.csv")
df <- fread("polarization/orig/liss.csv")

# Remove duplicates -------------------------------------------------------

# create ID & time
df <- df |>
  mutate(id = paste0(nomem_encr,wave),
         t = as.integer(wave - 7))

dupl <- df |>
  count(id) |>
  filter(n>1)

dupldf <- df |>
  filter(id %in% dupl$id)

df <- df |>
  filter(!(id %in% dupl$id) & !is.na(wave)) |>
  # create identifier
  arrange(nomem_encr, wave) |>
  group_by(nomem_encr) |>
  mutate(id = cur_group_id()) |>
  ungroup() |>
  data.frame()

rm(dupl, dupldf)


# Apply Panelmatch function -----------------------------------------------

## Create empty dataframes for results -------------------------------------

# Create an empty dataframe to store the overall results
overall_results_df <- data.frame(
  treatment = character(),
  outcome = character(),
  estimate = numeric(),
  conf.low = numeric(),
  conf.high = numeric(),
  conf.low90 = numeric(),
  conf.high90 = numeric(),
  stringsAsFactors = FALSE
)

# Create an empty dataframe to store the event study results
results_df <- data.frame(
  treatment = character(),
  outcome = character(),
  t = numeric(),
  estimate = numeric(),
  conf.low = numeric(),
  conf.high = numeric(),
  conf.low90 = numeric(),
  conf.high90 = numeric(),
  stringsAsFactors = FALSE
)

# Create an empty dataframe to store covariate balances
covariate_balance_df <- data.frame(
  treatment = character(),
  outcome = character(),
  t = numeric(),
  covariate = character(),
  covbal = numeric(),
  model = character(),
  stringsAsFactors = FALSE
)



# Define treatments, outcomes, covariates ---------------------------------


# Define treatments, outcomes, and covariates
treatments <-
  c(
    "unemployed",
    "income_cat_decrease",
    "income_cat_2p_decrease",
    "income_hh_25p_decrease",
    "employed"
  )

outcomes <- df |> 
  select(
    distance_age_abs_mean:distance_education_rel_mean,
    red_distance_age_abs_mean:red_distance_education_rel_mean,
    weighted_distance_age_abs_mean:weighted_distance_education_rel_mean,
    red_weighted_distance_age_abs_mean:red_weighted_overall_mean_distance
  ) |>
  colnames() |> 
  dput()

covariates <-
  c(
    "age_cat",
    "male",
    "education_cat",
    "partner",
    "no_children_hh",
    "l1_net_monthly_income_cat",
    "house_owner"
  )

## Specify paths -----------------------------------------------------------

figure_path <- "polarization/output/riskaversion/figures/"
dataframes_path <- "polarization/output/riskaversion/datasets/"

# Create folders if they don't exist
dir.create(figure_path, showWarnings = FALSE)
dir.create(dataframes_path, showWarnings = FALSE)

## Load already existing results dataframes --------------------------------

# Check if panelmatch_results.csv exists
if (file.exists(paste0(dataframes_path, "panelmatch_results.csv"))) {
  results_df <- fread(paste0(dataframes_path, "panelmatch_results.csv"))
  message("Loaded existing panelmatch_results.csv.")
} else {
  message("panelmatch_results.csv does not exist. Initializing as NULL.")
}

# Check if panelmatch_covariate_balance_df.csv exists
if (file.exists(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))) {
  covariate_balance_df <- fread(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))
  message("Loaded existing panelmatch_covariate_balance_df.csv.")
} else {
  message("panelmatch_covariate_balance_df.csv does not exist. Initializing as NULL.")
}


## Specify other inputs ----------------------------------------------------

data <- df
unit_id <- "id"
time_id <- "t"
lag <- 2
lead <- 0:2


# Loop through ------------------------------------------------------------

# Source function
# source("analyze/PanelMatch_function.r")
source("polarization/code/PanelMatch_function.r")

# Run the function for all combinations of treatments and outcomes
for (treatment in treatments) {
  
  # Visualize Treatment Distribution
  treatment_hist <- DisplayTreatment(
    unit.id = unit_id,
    time.id = time_id,
    legend.position = "bottom",
    xlab = "Wave",
    ylab = "ID",
    title = "",
    treatment = treatment,
    data = data,
    hide.y.tick.label = TRUE,
    dense.plot = TRUE
  )
  
  # Saving the histogram
  ggsave(
    filename = paste0(treatment, "_hist.pdf"),
    plot = treatment_hist,
    path = figure_path,
    height = 8, width = 8
  )
  
  for (outcome in outcomes) {
    message(paste("Running analysis for treatment:", treatment, " and outcome:", outcome))
    results <- run_panelmatch(data, treatment, outcome, covariates, lag = lag, lead = lead, figure_path = figure_path, dataframes_path = dataframes_path)
    # Save dataframes
    overall_results_df <- results$overall_results_df
    results_df <- results$results_df
    covariate_balance_df <- results$covariate_balance_df
  }
}


# Write
fwrite(overall_results_df, file = paste0(dataframes_path,"panelmatch_overall_results.csv"))
fwrite(results_df, file = paste0(dataframes_path,"panelmatch_results.csv"))
fwrite(covariate_balance_df, file = paste0(dataframes_path,"panelmatch_covariate_balance_df.csv"))

### END