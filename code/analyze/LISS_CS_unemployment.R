### # Heddesheimer, Bryson - Replication code - March 20, 2025
# vincent.heddesheimer@princeton.edu
#
# Tables reproduced in this file:
# 
# Figures reproduced in this file:
# 
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, did, haschaR, modelsummary, HonestDiD)

# Load LISS data
df <- fread("data/liss.csv")

# source honestdid function for Callaway Sant'Anaa
source("code/analyze/honestdid_cs.R")
source("code/analyze/cs_event_study.R")

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
  arrange(nomem_encr,wave) |>
  group_by(nomem_encr) |>
  mutate(id = cur_group_id()) |>
  ungroup() |>
  data.frame()

rm(dupl, dupldf)


# compare observations for distance, spread, and partisan
table(is.na(df$distance))
table(is.na(df$spread))
table(is.na(df$partisan_affect))


# check how often people get unemployed more than once in the data
df |>
  filter(unemployment_shock == 1) |>
  count(nomem_encr) |>
  filter(n > 1) |>
  nrow()

inspect <- df |>
  select(nomem_encr, wave, unemployed, unemployment_shock)


# Unemployment ------------------------------------------------------------

# Create treatment variable
df1 <- df |> mutate(treated = ifelse(unemployed == 1, 1, 0))

# Get treated units
treat <- df1 |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df1 <- df1 |>
  group_by(id) |>
  mutate(
    # assign treatment group
    treat = ifelse(id %in% treat, 1, 0), 
    # calculate time to treatment
    time_to_treatment = ifelse(treat == 1,
                               t - first(na.omit(t[treated == 1])),
                               0),
    # calculate period when first treated
    first_treatment_period = ifelse(treat == 1,
                                    first(na.omit(t[treated == 1])),
                                    0) # never-treated should be zero or NA
  )

# table: unemployed each year
df1 |>
  janitor::tabyl(t, unemployed) |>
  summarise(
    min = min(`1`),
    max = max(`1`)
  )

# how many treated?
df1 |>
  filter(treat == 1) |>
  distinct(nomem_encr) |>
  nrow()

# As data.frame
df1 <- as.data.frame(df1)

# outcome variables
pvars <- c(
  "partisan_affect", "spread", "distance", "like_max",
  "like_min",
  "red_overall_mean_distance", "red_distance_age_rel_mean",
  "red_distance_education_rel_mean", "red_distance_gender_mean",
  "red_distance_ethnicity_rel_mean", "generalized_trust"
)

# get summary statistics for each variable, including standard deviation, with datasummary
df1 |>
  select(all_of(pvars)) |>
  datasummary_skim()

# wo controls -------------------------------------------------------------

# # Run DiD analysis
# # Initialize an empty list to store the results
# event_study_results <- list()
# 
# # specify input
# post_treatment_periods <- 0:8
# Mbarvec <- c(0.5, 1)
# 
# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1,
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     # xformla = ~ education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat + house_owner,
#     run_sensitivity = TRUE,
#     post_treatment_periods = post_treatment_periods,
#     Mbarvec = Mbarvec
#   )
#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }
# 
# # Combine the results into a single data frame
# combined_results <- bind_rows(event_study_results)
# 
# # save
# fwrite(combined_results, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_09_12/unemp_CS.csv")


# load
combined_results <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_09_12/unemp_CS.csv")

# Define the desired order of facet labels with sample size placeholders
desired_order <- c(
  "Distance \\(N=",
  "Spread \\(N=",
  "Like Min \\(N=",
  "Like Max \\(N=",
  "Generalized Trust \\(N=",
  "Outgroup Distance \\(N="
)

# Plot
combined_results |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Spread",
             dv == "distance" ~ "Distance",
             dv == "like_min" ~ "Like Min",
             dv == "like_max" ~ "Like Max",
             dv == "red_overall_mean_distance" ~ "Outgroup Distance",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>  
  mutate(dv = paste0(dv, " (N=", n, ")")) |>
  # Order the facets based on the desired order
  mutate(dv = factor(dv, levels = sapply(desired_order, function(x) grep(x, dv, value = TRUE)[1]))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0, linewidth = 0.5, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = estimate - 1.64 * std.error, ymax = estimate + 1.64 * std.error), width = 0, linewidth = 1, position = position_dodge(0.4)) +
  geom_point(position = position_dodge(0.4), shape = 21, fill = "white", size = 2) +
  # set color to black and darkgrey
  scale_color_manual(values = c("black", "darkgrey")) +
  theme_hanno() +
  theme(legend.position = "none") +
  labs(
    x = "Time relative to treatment",
    y = "ATT",
    color = "Polarization Measure"
  ) +
  # x axis from -5 to 8
  scale_x_continuous(breaks = seq(-5, 8, 1)) +
  facet_wrap(~ dv, scales = "free", ncol = 2)

# save
ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_09_12/unemp_all.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all.pdf", width = 7, height = 7)


# with controls -----------------------------------------------------------

# # Run DiD analysis
# # Initialize an empty list to store the results
# event_study_results <- list()
# 
# # specify input
# post_treatment_periods <- 0:8
# Mbarvec <- c(0.5, 1)
# 
# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1,
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     xformla = ~ education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat + house_owner,
#     run_sensitivity = TRUE,
#     post_treatment_periods = post_treatment_periods,
#     Mbarvec = Mbarvec
#   )
#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }
# 
# # Combine the results into a single data frame
# combined_results_c <- bind_rows(event_study_results)
# 
# # save
# fwrite(combined_results_c, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_09_12/unemp_CS_wcontrols.csv")

# load
combined_results_c <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_09_12/unemp_CS_wcontrols.csv")

# Define the desired order of facet labels with sample size placeholders
desired_order <- c(
  "Distance \\(N=",
  "Spread \\(N=",
  "Like Min \\(N=",
  "Like Max \\(N=",
  "Generalized Trust \\(N=",
  "Outgroup Distance \\(N="
)

# Plot
combined_results_c |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Spread",
             dv == "distance" ~ "Distance",
             dv == "like_min" ~ "Like Min",
             dv == "like_max" ~ "Like Max",
             dv == "red_overall_mean_distance" ~ "Outgroup Distance",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>  
  mutate(dv = paste0(dv, " (N=", n, ")")) |>
  # Order the facets based on the desired order
  mutate(dv = factor(dv, levels = sapply(desired_order, function(x) grep(x, dv, value = TRUE)[1]))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0, linewidth = 0.5, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = estimate - 1.64 * std.error, ymax = estimate + 1.64 * std.error), width = 0, linewidth = 1, position = position_dodge(0.4)) +
  geom_point(position = position_dodge(0.4), shape = 21, fill = "white", size = 2) +
  # set color to black and darkgrey
  scale_color_manual(values = c("black", "darkgrey")) +
  theme_hanno() +
  theme(legend.position = "none") +
  labs(
    x = "Time relative to treatment",
    y = "ATT",
    color = "Polarization Measure"
  ) +
  # x axis from -5 to 8
  scale_x_continuous(breaks = seq(-5, 8, 1)) +
  facet_wrap(~ dv, scales = "free", ncol = 2)

# save
ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_09_12/unemp_all_controls.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all_controls.pdf", width = 7, height = 7)




### END