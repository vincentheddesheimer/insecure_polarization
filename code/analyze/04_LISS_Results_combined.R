### # Heddesheimer, Bryson - Replication code - Month XX, 2023
# vincent.heddesheimer@princeton.edu
#
#
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, haschaR)


# Create folders if they don't exist
dir.create("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/combined", showWarnings = FALSE)


# Dynamic results ---------------------------------------------------------


# Load data

# Results
results_polar <-
  # fread("output_10_03/polarization/datasets/panelmatch_results.csv") |>
  fread("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/polarization/datasets/panelmatch_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (2p)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  )) |>
  distinct()
results_trust <-
  # fread("output_10_03/trust/datasets/panelmatch_results.csv") |>
  fread("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/trust/datasets/panelmatch_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (2p)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))|>
  distinct()
results_riskaversion <-
  # fread("output_10_03/riskaversion/datasets/panelmatch_results.csv") |>
  fread("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/riskaversion/datasets/panelmatch_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (2p)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))|>
  distinct()



## Polarization ----------------------------------------------------

# Define a vector of outcome names
outcomes <- results_polar |> 
  distinct(outcome) |> 
  pull()

# Loop over the outcome names and create a plot for each one
for (out in outcomes) {
  # Filter the data for the current outcome
  data <- results_polar %>% filter(outcome == out & t != -1)
  
  # Create the plot
  plot <- ggplot(data, aes(x = t, y = estimate)) +
    facet_wrap(~ factor(
      treatment,
      levels = c(
        'Unemployed',
        'Sizable Income \n    Drop (2p)',
        'Income Drop',
        'Sizable Household \n    Income Drop (>25%)'
      )
    ),
    nrow = 1,
    scales = "fixed") +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = .25,
      alpha = 0.75
    ) +
    geom_vline(
      xintercept = -1,
      linetype = "dashed",
      color = "black",
      linewidth = .25,
      alpha = 0.75
    ) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0,
      linewidth = .5,
      position = position_dodge(0.4)
    ) +
    geom_errorbar(
      aes(ymin = conf.low90, ymax = conf.high90),
      width = 0,
      linewidth = 1,
      position = position_dodge(0.4)
    ) +
    geom_point(
      aes(x = t, y = estimate),
      shape = 21,
      fill = "white",
      size = 2,
      position = position_dodge(0.4)
    ) +
    labs(y = "ATT", x = "Time relative to treatment") +
    theme_hanno()
  
  # Save the plot
  ggsave(
    filename = paste0("combined_results_", out, "_att.pdf"),
    plot = plot,
    path = "~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/combined",
    width = 9, height = 3.5
  )
}



## Trust -----------------------------------------------------------

# Create the plot
plot <- ggplot(results_trust |> filter(t != -1), aes(x = t, y = estimate)) +
  facet_wrap(~ factor(
    treatment,
    levels = c(
      'Unemployed',
      'Sizable Income \n    Drop (2p)',
      'Income Drop',
      'Sizable Household \n    Income Drop (>25%)'
    )
  ),
  nrow = 1,
  scales = "fixed") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = .25,
    alpha = 0.75
  ) +
  geom_vline(
    xintercept = -1,
    linetype = "dashed",
    color = "black",
    linewidth = .25,
    alpha = 0.75
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = .5,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90),
    width = 0,
    linewidth = 1,
    position = position_dodge(0.4)
  ) +
  geom_point(
    aes(x = t, y = estimate),
    shape = 21,
    fill = "white",
    size = 2,
    position = position_dodge(0.4)
  ) +
  labs(y = "ATT", x = "Time relative to treatment") +
  theme_hanno()

# Save the plot
ggsave(
  filename = "combined_results_trust_att.pdf",
  plot = plot,
  path = "~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/combined",
  width = 9, height = 3.5
)



## risk aversion ---------------------------------------------------


# Define a vector of outcome names
outcomes <- results_riskaversion |> distinct(outcome) |> pull()

# Loop over the outcome names and create a plot for each one
for (out in outcomes) {
  # Filter the data for the current outcome
  data <- results_riskaversion %>% filter(outcome == out & t != -1)
  
  # Create the plot
  plot <- ggplot(data, aes(x = t, y = estimate)) +
    facet_wrap(~ factor(
      treatment,
      levels = c(
        'Unemployed',
        'Sizable Income \n    Drop (2p)',
        'Income Drop',
        'Sizable Household \n    Income Drop (>25%)'
      )
    ),
    nrow = 1,
    scales = "fixed") +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = .25,
      alpha = 0.75
    ) +
    geom_vline(
      xintercept = -1,
      linetype = "dashed",
      color = "black",
      linewidth = .25,
      alpha = 0.75
    ) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0,
      linewidth = .5,
      position = position_dodge(0.4)
    ) +
    geom_errorbar(
      aes(ymin = conf.low90, ymax = conf.high90),
      width = 0,
      linewidth = 1,
      position = position_dodge(0.4)
    ) +
    geom_point(
      aes(x = t, y = estimate),
      shape = 21,
      fill = "white",
      size = 2,
      position = position_dodge(0.4)
    ) +
    labs(y = "ATT", x = "Time relative to treatment") +
    theme_hanno()
  
  # Save the plot
  ggsave(
    filename = paste0("combined_results_", out, "_att.pdf"),
    plot = plot,
    path = "~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/combined",
    width = 9, height = 3.5
  )
}



# Overall results ---------------------------------------------------------

# Load data

# Results
polar <-
  # fread("output_10_03/polarization/datasets/panelmatch_results.csv") |>
  fread("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/polarization/datasets/panelmatch_overall_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (2p)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))
trust <-
  # fread("output_10_03/trust/datasets/panelmatch_results.csv") |>
  fread("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/trust/datasets/panelmatch_overall_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (2p)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))
riskaversion <-
  # fread("output_10_03/riskaversion/datasets/panelmatch_results.csv") |>
  fread("~/Dropbox (Princeton)/insecure_polarization/output_24_01_26/riskaversion/datasets/panelmatch_overall_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (2p)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))


# Combine results
polar_plot_df <- polar |>
  filter(outcome %in% c("distance", "like_max",
                        "like_min", "partisan_affect", "spread"))

# Create the plot
# four facets for each treatment
# y- axis is outcome
# x-axis is ATT + confidence intervals
polar_plot_df |>
  ggplot(aes(y = outcome, x = estimate)) +
  facet_wrap(~ factor(
    treatment,
    levels = c(
      'Unemployed',
      'Sizable Income \n    Drop (2p)',
      'Income Drop',
      'Sizable Household \n    Income Drop (>25%)'
    )
  ),
  nrow = 2,
  scales = "fixed") +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = .25,
    alpha = 0.75
  ) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0,
    linewidth = .5,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    width = 0,
    linewidth = 1,
    position = position_dodge(0.4)
  ) +
  geom_point(
    aes(y = outcome, x = estimate),
    shape = 21,
    fill = "white",
    size = 2
  ) +
  labs(y = "Outcome", x = "ATT") +
  theme_hanno()

# Combine results
distance_plot_df <- riskaversion|>
  filter(outcome %in% c("red_overall_mean_distance", "red_distance_age_rel_mean",
                   "red_distance_education_rel_mean", "red_distance_gender_mean",
                   "red_distance_ethnicity_rel_mean"))

# Create the plot
# four facets for each treatment
# y- axis is outcome
# x-axis is ATT + confidence intervals
distance_plot_df |>
  ggplot(aes(y = outcome, x = estimate)) +
  facet_wrap(~ factor(
    treatment,
    levels = c(
      'Unemployed',
      'Sizable Income \n    Drop (2p)',
      'Income Drop',
      'Sizable Household \n    Income Drop (>25%)'
    )
  ),
  nrow = 2,
  scales = "fixed") +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = .25,
    alpha = 0.75
  ) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0,
    linewidth = .5,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    width = 0,
    linewidth = 1,
    position = position_dodge(0.4)
  ) +
  geom_point(
    aes(y = outcome, x = estimate),
    shape = 21,
    fill = "white",
    size = 2
  ) +
  labs(y = "Outcome", x = "ATT") +
  theme_hanno()


### END
