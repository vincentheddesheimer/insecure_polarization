### # Heddesheimer, Bryson - Replication code - Month XX, 2023
# vincent.heddesheimer@princeton.edu
#
#
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, haschaR)

# Load data

# Results
results_polar <-
  fread("output_09_28/polarization/datasets/panelmatch_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (>25%)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))
results_trust <-
  fread("output_09_28/trust/datasets/panelmatch_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (>25%)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))
results_riskaversion <-
  fread("output_09_28/riskaversion/datasets/panelmatch_results.csv") |>
  # Rename treatment categories: Unemployment, Income Drop, Sizable Income Drop (>25%),Sizable Household Income Drop (>25%)
  mutate(treatment = case_when(
    treatment == "unemployed" ~ "Unemployed",
    treatment == "income_cat_decrease" ~ "Income Drop",
    treatment == "income_cat_2p_decrease" ~ "Sizable Income 
    Drop (>25%)",
    treatment == "income_hh_25p_decrease" ~ "Sizable Household 
    Income Drop (>25%)"
  ))



# Polarization Results ----------------------------------------------------

# Define a vector of outcome names
outcomes <- results_polar |> 
  distinct(outcome) |> 
  pull()

# Loop over the outcome names and create a plot for each one
for (out in outcomes) {
  # Filter the data for the current outcome
  data <- results_polar %>% filter(outcome == out)
  
  # Create the plot
  plot <- ggplot(data, aes(x = t, y = estimate)) +
    facet_wrap( ~ treatment, nrow = 1, scales = "fixed") +
    geom_point(aes(x = t, y = estimate),
               size = 2,
               position = position_dodge(0.4)) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0,
      linewidth = .5,
      position = position_dodge(0.4)
    ) +
    geom_errorbar(
      aes(ymin = conf.low90, ymax = conf.high90),
      width = 0,
      linewidth = 1.25,
      position = position_dodge(0.4)
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "red",
      linewidth = .25,
      alpha = 0.75
    ) +
    labs(y = "ATT", x = "Relative Time") +
    scale_x_continuous(breaks = 0:2) +
    theme_hanno()
  
  # Save the plot
  ggsave(
    filename = paste0("combined_results_", out, "_att.pdf"),
    plot = plot,
    path = "output_09_28/combined",
    width = 9,
    height = 5.5
  )
}



# Trust Results -----------------------------------------------------------

# Create the plot
plot <- ggplot(results_trust, aes(x = t, y = estimate)) +
  facet_wrap( ~ treatment, nrow = 1, scales = "fixed") +
  geom_point(aes(x = t, y = estimate),
             size = 2,
             position = position_dodge(0.4)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = .5,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90),
    width = 0,
    linewidth = 1.25,
    position = position_dodge(0.4)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = .25,
    alpha = 0.75
  ) +
  labs(y = "ATT", x = "Relative Time") +
  scale_x_continuous(breaks = 0:2) +
  theme_hanno()

# Save the plot
ggsave(
  filename = "combined_results_trust_att.pdf",
  plot = plot,
  path = "output_09_27/combined",
  width = 9,
  height = 5.5
)



# Results risk aversion ---------------------------------------------------


# Create the plot
results_polar |>
  filter(outcome == "spread") |>
  ggplot(aes(x = t, y = estimate)) +
  facet_wrap(~ treatment, nrow = 1, scales = "fixed") +
  geom_point(aes(x = t, y = estimate),
             size = 2,
             position = position_dodge(0.4)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = .5,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90),
    width = 0,
    linewidth = 1.25,
    position = position_dodge(0.4)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = .25,
    alpha = 0.75
  ) +
  labs(y = "ATT", x = "Relative Time") +
  scale_x_continuous(breaks = 0:2) +
  theme_hanno()

# Save the plot
ggsave("results/figures/spread.pdf", width = 9, height = 3.5)

