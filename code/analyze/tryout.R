#tryout

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, PanelMatch, haschaR)

# Load LISS data
df <- fread("data/liss.csv")
# df <- fread("polarization/orig/liss.csv")

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


# Create correct id & t ---------------------------------------------------

# remove wave 15: Politics & Values study was not conducted
df <- df |> filter(wave != 16)

df <- df |>
  filter(!is.na(id)) |>
  group_by(wave) |>
  mutate(t = cur_group_id()) |>
  ungroup() |>
  # Create consecutive new unit id
  arrange(id) |>
  mutate(id = dense_rank(id)) |>
  data.frame()

# Apply Panelmatch function -----------------------------------------------

## Create empty dataframes for results -------------------------------------

# Create an empty dataframe to store the results
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
treatment <-
  c(
    "unemployed"
    # ,
    # "income_cat_decrease",
    # "income_cat_2p_decrease",
    # "income_hh_25p_decrease"
  )

outcome <- c(
  # "partisan_affect",
  # "spread",
  "distance"
  # ,"like_max",
  # "like_min",
  # "generalized_trust",
  # "little_concern_for_others"
  # ,
  # "interested_in_others",
  # "comfy_around_people",
  # "insult_people",
  # "sympathisze_w_others_feelings",
  # "not_interested_in_others_problems",
  # "talk_to_different_people_parties",
  # "not_interested_in_others",
  # "feel_others_emotions",
  # "quiet_around_strangers",
  # "importance_open_minded",
  # "feel_connected_to_others"
)

covariates <-
  c(
    "age_cat",
    "male",
    "education_cat",
    "partner",
    "no_children_hh",
    "l1_net_monthly_income_cat"
  )

## Specify paths -----------------------------------------------------------

figure_path <- "polarization/output/riskaversion/figures/"
dataframes_path <- "polarization/output/riskaversion/datasets/"

# Create folders if they don't exist
dir.create(figure_path, showWarnings = FALSE)
dir.create(dataframes_path, showWarnings = FALSE)

## Load already existing results dataframes --------------------------------

# Check if panelmatch_results.csv exists
# if (file.exists(paste0(dataframes_path, "panelmatch_results.csv"))) {
#   results_df <- fread(paste0(dataframes_path, "panelmatch_results.csv"))
#   message("Loaded existing panelmatch_results.csv.")
# } else {
#   message("panelmatch_results.csv does not exist. Initializing as NULL.")
# }
# 
# # Check if panelmatch_covariate_balance_df.csv exists
# if (file.exists(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))) {
#   covariate_balance_df <- fread(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))
#   message("Loaded existing panelmatch_covariate_balance_df.csv.")
# } else {
#   message("panelmatch_covariate_balance_df.csv does not exist. Initializing as NULL.")
# }


## Specify other inputs ----------------------------------------------------

data <- df
unit_id <- "id"
time_id <- "t"
lag <- 2
lead <- 0:2


# Loop through ------------------------------------------------------------

# Source function
# source("analyze/PanelMatch_function.r")
# source("polarization/code/PanelMatch_function.r")

# Run the function for all combinations of treatments and outcomes



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
  height = 8,
  width = 8
)

# Match individuals on covariates and lagged outcome
cov_formula <-
  as.formula(paste("~", paste(covariates, collapse = " + ")))

# Adding the lag of the outcome variable
lagged_outcome <- sprintf("I(lag(%s, 1:%d))", outcome, lag)
match_formula <-
  as.formula(paste("~", paste(c(
    covariates, lagged_outcome
  ), collapse = " + ")))

# Create matched set
match <- PanelMatch(
  lag = lag,
  time.id = time_id,
  unit.id = unit_id,
  covs.formula = match_formula,
  treatment = treatment,
  refinement.method = "CBPS.weight",
  data = data,
  match.missing = TRUE,
  qoi = "att",
  outcome.var = outcome,
  lead = lead,
  forbid.treatment.reversal = FALSE
)

# Create a scatter plot to analyze covariate balance
pdf(paste0(figure_path,
           treatment,
           "_",
           outcome,
           "_cb_scat.pdf"))

balance_scatter(
  matched_set_list = list(match$att),
  data = data,
  covariates = c(covariates, outcome)
)
# Close the pdf file
dev.off()

# Create covariate balance before treatment
# Note that this includes covariates as well as outcome
cb <- as_tibble(get_covariate_balance(
  match$att,
  data = data,
  covariates = c(covariates, outcome),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(
    cols = c(covariates, outcome),
    names_to = "covariate",
    values_to = "covbal"
  ) |>
  mutate(t = as.integer(str_replace(t, "t_", "-")),
         treatment = treatment,
         outcome = outcome)

# cb df
covariate_balance_df <- rbind(covariate_balance_df, cb)

# Create Covariance Balance Plot
# Create a named color vector where the outcome is "black" and all other covariates are "grey"
all_vars <- c(covariates, outcome)
color_mapping <-
  setNames(rep("grey", length(all_vars)), all_vars)
color_mapping[outcome] <- "black"

# Calculate the maximum absolute value of covbal in the data
max_abs_covbal <- max(abs(cb$covbal))

# Set y-limits based on the calculated max
if (max_abs_covbal > 0.5) {
  y_limits <- c(-max_abs_covbal, max_abs_covbal)
} else {
  y_limits <- c(-0.5, 0.5)
}

cb_plot <- cb |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = color_mapping) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = lag)) +
  scale_y_continuous(limits = y_limits) +  # Use the dynamic y-limits here
  theme_hanno() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")

# Save
ggsave(
  filename = paste0(treatment, "_", outcome, "_cb_pre.pdf"),
  plot = cb_plot,
  path = figure_path,
  height = 7,
  width = 8
)



# Match individuals on covariates and lagged outcome
cov_formula <-
  as.formula(paste("~", paste(covariates, collapse = " + ")))

match_formula <-
  as.formula(paste("~", paste(covariates, collapse = " + ")))

match <- PanelMatch(
  lag = lag,
  time.id = time_id,
  unit.id = unit_id,
  covs.formula = match_formula,
  treatment = treatment,
  refinement.method = "CBPS.weight",
  data = data,
  match.missing = TRUE,
  qoi = "att",
  outcome.var = outcome,
  lead = lead,
  forbid.treatment.reversal = FALSE,
  placebo.test = TRUE
)

est_pooled <-
  PanelEstimate(sets = match,
                data = data,
                se.method = se.method,
                pooled = TRUE)

summary_est_pooled <- summary(est_pooled)$summary

overall_results <- data.frame(
  treatment = treatment,
  outcome = outcome, 
  # Now dynamic
  estimate = summary_est_pooled[1],
  conf.low = summary_est_pooled[3],
  conf.high = summary_est_pooled[4],
  # calculate 90% confidence intervals
  conf.low90 = summary_est_pooled[1] - summary_est_pooled[2] * qnorm(0.95),
  conf.high90 = summary_est_pooled[1] + summary_est_pooled[2] * qnorm(0.95),
  stringsAsFactors = FALSE
)

# Estimate ATT
est <-
  PanelEstimate(sets = match,
                data = data,
                se.method = "conditional")
est90 <-
  PanelEstimate(
    sets = match,
    data = data,
    se.method =  "conditional",
    confidence.level = .90
  )

# Extract relevant portions of the summary data frames for each lead
summary_est <- summary(est)$summary
summary_est90 <- summary(est90)$summary

lead_rows <-
  lead + 1  # Assuming leads start from 0 and the summary is indexed starting from 1
tmp_results <- data.frame(
  treatment = rep(treatment, length(lead)),
  outcome = rep(outcome, length(lead)),
  t = rep(lead, each = 1),
  # Now dynamic
  estimate = summary_est[lead_rows, 1],
  conf.low = summary_est[lead_rows, 3],
  conf.high = summary_est[lead_rows, 4],
  conf.low90 = summary_est90[lead_rows, 3],
  conf.high90 = summary_est90[lead_rows, 4],
  stringsAsFactors = FALSE
)

# Add these results to the overall dataframe
results_df <- rbind(results_df, tmp_results)

# Plot results
plot_data <- data.frame(
  t = lead,
  estimate = summary_est[lead_rows, 1],
  conf.low = summary_est[lead_rows, 3],
  conf.high = summary_est[lead_rows, 4],
  conf.low90 = summary_est90[lead_rows, 3],
  conf.high90 = summary_est90[lead_rows, 4]
)

# Plotting...
plot <- plot_data |>
  ggplot(aes(x = t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0,
                linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90),
                width = 0,
                linewidth = 1.25) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = .25,
    alpha = 0.75
  ) +
  labs(y = "ATT", x = "Relative Time") +
  scale_x_continuous(breaks = lead) +
  haschaR::theme_hanno()

ggsave(
  filename = paste0(treatment, "_", outcome, "_att.pdf"),
  plot = plot,
  path = figure_path,
  width = 5,
  height = 5.5
)



# Moderator ---------------------------------------------------------------

est <-
  PanelEstimate(sets = match,
                data = data,
                se.method = "conditional",
                moderator = "male")

# Extract relevant portions of the summary data frames for each lead
summary_est_female <- summary(est[[1]])$summary
summary_est_male <- summary(est[[2]])$summary

lead_rows <-
  lead + 1  # Assuming leads start from 0 and the summary is indexed starting from 1
tmp_results <- data.frame(
  treatment = rep(treatment, length(lead)),
  outcome = rep(outcome, length(lead)),
  t = rep(lead, each = 1),
  # Now dynamic
  estimate = summary_est[lead_rows, 1],
  conf.low = summary_est[lead_rows, 3],
  conf.high = summary_est[lead_rows, 4],
  # calculate 90% confidence intervals
  conf.low90 = summary_est[lead_rows, 1] - summary_est[lead_rows, 2] * qnorm(0.95),
  conf.high90 = summary_est[lead_rows, 1] + summary_est[lead_rows, 2] * qnorm(0.95),
  stringsAsFactors = FALSE
)


# Write
# fwrite(results_df, file = paste0(dataframes_path,"panelmatch_results.csv"))
# fwrite(results_df, file = paste0(dataframes_path,"panelmatch_covariate_balance_df.csv"))
