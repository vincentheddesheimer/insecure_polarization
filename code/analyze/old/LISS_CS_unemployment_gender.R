### # Heddesheimer, Bryson - Replication code - Month XX, 2024
# vincent.heddesheimer@princeton.edu
#
# Tables reproduced in this file:
# 
# Figures reproduced in this file:
# 
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, did2s, haschaR, modelsummary)

# Load LISS data
df <- fread("data/liss.csv")

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

# # get summary statistics for each variable, including standard deviation, with datasummary
# df1 |>
#   select(all_of(pvars)) |>
#   datasummary_skim()



# Men only ----------------------------------------------------------------

# # Define a function to perform event study analysis for a single variable
# perform_event_study <- function(data, yname, idname, tname, gname, estimator, pvar) {
#   event_study(
#     data = data, yname = pvar, idname = idname,
#     tname = tname, gname = gname,
#     estimator = estimator
#   ) %>%
#     mutate(dv = pvar)
# }
# 
# # Initialize an empty list to store the results
# event_study_results <- list()
# 
# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1 |> filter(male == 1),
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     estimator = "did",
#     pvar = pvar
#   )
# 
#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }
# 
# # Combine the results into a single data frame
# combined_results <- bind_rows(event_study_results)
# 
# # save
# fwrite(combined_results, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_men.csv")

combined_results <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_men.csv")

# polarization results
combined_results |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Affective Polarization (Spread)",
             dv == "distance" ~ "Affective Polarization (Distance)",
             dv == "like_min" ~ "Outgroup Aversion",
             dv == "like_max" ~ "Ingroup Affinity",
             dv == "red_overall_mean_distance" ~ "Heterophily (close social ties)",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>     
  # reorder
  mutate(dv = factor(dv, levels = c("Affective Polarization (Distance)", "Affective Polarization (Spread)", "Outgroup Aversion", "Ingroup Affinity", "Heterophily (close social ties)", "Generalized Trust"))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
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

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_all_wo_controls_men.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all_wo_controls_men.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/unemp_all_wo_controls_men.pdf", width = 7, height = 7)


## With controls -----------------------------------------------------------


# # Define a function to perform event study analysis for a single variable
# perform_event_study <- function(data, yname, idname, tname, gname, estimator, pvar) {
#   event_study(
#     data = data, yname = pvar, idname = idname,
#     tname = tname, gname = gname,
#     estimator = estimator,
#     xformla = ~ education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat + house_owner
#   ) %>%
#     mutate(dv = pvar)
# }
# 
# # Initialize an empty list to store the results
# event_study_results <- list()
# 
# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1 |> filter(male == 1),
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     estimator = "did",
#     pvar = pvar
#   )
# 
#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }
# 
# # Combine the results into a single data frame
# combined_results_c <- bind_rows(event_study_results)
# 
# # save
# fwrite(combined_results_c, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_controls_men.csv")


# fread
combined_results_c <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_controls_men.csv")


# polarization results
combined_results_c |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Affective Polarization (Spread)",
             dv == "distance" ~ "Affective Polarization (Distance)",
             dv == "like_min" ~ "Outgroup Aversion",
             dv == "like_max" ~ "Ingroup Affinity",
             dv == "red_overall_mean_distance" ~ "Heterophily (close social ties)",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>     
  # reorder
  mutate(dv = factor(dv, levels = c("Affective Polarization (Distance)", "Affective Polarization (Spread)", "Outgroup Aversion", "Ingroup Affinity", "Heterophily (close social ties)", "Generalized Trust"))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
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

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_all_controls_men.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all_controls_men.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/unemp_all_controls_men.pdf", width = 7, height = 7)



# Women only --------------------------------------------------------------


# # Define a function to perform event study analysis for a single variable
# perform_event_study <- function(data, yname, idname, tname, gname, estimator, pvar) {
#   event_study(
#     data = data, yname = pvar, idname = idname,
#     tname = tname, gname = gname,
#     estimator = estimator
#   ) %>%
#     mutate(dv = pvar)
# }
# 
# # Initialize an empty list to store the results
# event_study_results <- list()
# 
# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1 |> filter(male == 0),
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     estimator = "did",
#     pvar = pvar
#   )
# 
#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }
# 
# # Combine the results into a single data frame
# combined_results <- bind_rows(event_study_results)
# 
# # save
# fwrite(combined_results, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_women.csv")

combined_results <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_women.csv")

# polarization results
combined_results |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Affective Polarization (Spread)",
             dv == "distance" ~ "Affective Polarization (Distance)",
             dv == "like_min" ~ "Outgroup Aversion",
             dv == "like_max" ~ "Ingroup Affinity",
             dv == "red_overall_mean_distance" ~ "Heterophily (close social ties)",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>     
  # reorder
  mutate(dv = factor(dv, levels = c("Affective Polarization (Distance)", "Affective Polarization (Spread)", "Outgroup Aversion", "Ingroup Affinity", "Heterophily (close social ties)", "Generalized Trust"))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
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

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_all_wo_controls_women.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all_wo_controls_women.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/unemp_all_wo_controls_women.pdf", width = 7, height = 7)


## With controls -----------------------------------------------------------


# # Define a function to perform event study analysis for a single variable
# perform_event_study <- function(data, yname, idname, tname, gname, estimator, pvar) {
#   event_study(
#     data = data, yname = pvar, idname = idname,
#     tname = tname, gname = gname,
#     estimator = estimator,
#     xformla = ~ education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat + house_owner
#   ) %>%
#     mutate(dv = pvar)
# }
# 
# # Initialize an empty list to store the results
# event_study_results <- list()
# 
# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1 |> filter(male == 0),
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     estimator = "did",
#     pvar = pvar
#   )
# 
#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }
# 
# # Combine the results into a single data frame
# combined_results_c <- bind_rows(event_study_results)
# 
# # save
# fwrite(combined_results_c, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_controls_women.csv")


# fread
combined_results_c <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_controls_women.csv")


# polarization results
combined_results_c |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Affective Polarization (Spread)",
             dv == "distance" ~ "Affective Polarization (Distance)",
             dv == "like_min" ~ "Outgroup Aversion",
             dv == "like_max" ~ "Ingroup Affinity",
             dv == "red_overall_mean_distance" ~ "Heterophily (close social ties)",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>     
  # reorder
  mutate(dv = factor(dv, levels = c("Affective Polarization (Distance)", "Affective Polarization (Spread)", "Outgroup Aversion", "Ingroup Affinity", "Heterophily (close social ties)", "Generalized Trust"))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
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

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_all_controls_women.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all_controls_women.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/unemp_all_controls_women.pdf", width = 7, height = 7)


# Get into the same plot and dodge & color by gender --------------------------------

combined_results_men <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_men.csv") |> 
  mutate(gender = "Men")
combined_results_women <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_women.csv") |> 
  mutate(gender = "Women")

# Combine the results
combined_results <- bind_rows(combined_results_men, combined_results_women)

# Add gender variable
combined_results <- combined_results |>
  mutate(gender = factor(gender, levels = c("Men", "Women")))

# Plot
combined_results |>
filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("spread", "distance", "like_min", "like_max", "red_overall_mean_distance", "generalized_trust")) |>
  mutate(dv = 
           case_when(
             dv == "spread" ~ "Affective Polarization (Spread)",
             dv == "distance" ~ "Affective Polarization (Distance)",
             dv == "like_min" ~ "Outgroup Aversion",
             dv == "like_max" ~ "Ingroup Affinity",
             dv == "red_overall_mean_distance" ~ "Heterophily (close social ties)",
             dv == "generalized_trust" ~ "Generalized Trust"
           )) |>     
  # reorder
  mutate(dv = factor(dv, levels = c("Affective Polarization (Distance)", "Affective Polarization (Spread)", "Outgroup Aversion", "Ingroup Affinity", "Heterophily (close social ties)", "Generalized Trust"))) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = gender, shape = gender)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0, linewidth = 0.5, position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = estimate - 1.64 * std.error, ymax = estimate + 1.64 * std.error), width = 0, linewidth = 1, position = position_dodge(0.6)) +
  geom_point(position = position_dodge(0.6), fill = "white", size = 2) +
  # set color to black and darkgrey
  #scale_color_manual(values = c("darkgrey", "black")) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(21, 24)) +
  theme_hanno() +
  theme(legend.position = "bottom") +
  labs(
    x = "Time relative to treatment",
    y = "ATT",
    color = "Gender",
    shape = "Gender"
  ) +
  # x axis from -5 to 8
  scale_x_continuous(breaks = seq(-5, 8, 1)) +
  facet_wrap(~ dv, scales = "free", ncol = 2)

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/unemp_all_wo_controls_gender.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/unemp_all_wo_controls_gender.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/unemp_all_wo_controls_gender.pdf", width = 7, height = 7)


