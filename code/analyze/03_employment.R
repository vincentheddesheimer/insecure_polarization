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
pacman::p_load(tidyverse, data.table, did2s, haschaR)

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


# Employed ------------------------------------------------------------

inspect <- df |>
  select(id, t, occupation, employed, unemployed, student, retired)

# Filter out all students, retired
df <- df |>
  filter(unemployed == 1 | employed == 1)

# Create treatment variable
df1 <- df |> 
  group_by(id) |>
  arrange(t) |>
  mutate(unemployed_l1 = lag(unemployed)) |>
  ungroup() |>
  mutate(treated = ifelse(employed == 1 & unemployed_l1 == 1, 1, 0))

inspect <- df1 |>
  select(id, t, occupation, employed, unemployed, student, retired, treated)

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

# table: employed each year
df1 |>
  janitor::tabyl(t, employed) |>
  summarise(
    min = min(`1`),
    max = max(`1`)
  )

inspect <- df1 |>
  select(id, t, occupation, employed, unemployed, student, retired, treat, time_to_treatment, first_treatment_period)

# # As data.frame
# df1 <- as.data.frame(df1)

# outcome variables
pvars <- c(
  "partisan_affect", "spread", "distance", "like_max",
  "like_min",
  "red_overall_mean_distance", "red_distance_age_rel_mean",
  "red_distance_education_rel_mean", "red_distance_gender_mean",
  "red_distance_ethnicity_rel_mean", "generalized_trust"
)

# # Define a function to perform event study analysis for a single variable
# perform_event_study <- function(data, yname, idname, tname, gname, estimator, pvar) {
#   event_study(
#     data = data, yname = pvar, idname = idname,
#     tname = tname, gname = gname,
#     estimator = estimator
#   ) %>%
#     mutate(dv = pvar)
# }

# # Initialize an empty list to store the results
# event_study_results <- list()

# # Loop over each variable in pvars
# for (pvar in pvars) {
#   # Perform event study analysis for the current variable
#   result <- perform_event_study(
#     data = df1,
#     yname = pvar,
#     idname = "id",
#     tname = "t",
#     gname = "first_treatment_period",
#     estimator = "did",
#     pvar = pvar
#   )

#   # Append the result to the list
#   event_study_results[[pvar]] <- result
# }

# # Combine the results into a single data frame
# combined_results <- bind_rows(event_study_results)

# # save
# fwrite(combined_results, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_emp.csv")

combined_results <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_emp.csv")

# polarization
combined_results |>
  filter(dv %in% c("partisan_affect", "spread", "distance", "like_max", "like_min")) |>
  filter(term >= -5 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0, linewidth = 0.5, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = estimate - 1.64 * std.error, ymax = estimate + 1.64 * std.error), width = 0, linewidth = 1, position = position_dodge(0.4)) +
  geom_point(position = position_dodge(0.4), shape = 21, fill = "white", size = 2) +
  facet_wrap(~ dv + estimator, scales = "free") +
  theme_hanno() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(
    title = "Event Study Analysis",
    x = "Time (in months)",
    y = "Effect size",
    color = "Estimator"
  ) +
  # x axis from -5 to 5
  scale_x_continuous(breaks = seq(-5, 8, 1))

# outgroup
combined_results |>
  filter(dv %in% c("red_overall_mean_distance", "red_distance_age_rel_mean",
                   "red_distance_education_rel_mean", "red_distance_gender_mean",
                   "red_distance_ethnicity_rel_mean")) |>
  filter(term >= -5 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0, linewidth = 0.5, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = estimate - 1.64 * std.error, ymax = estimate + 1.64 * std.error), width = 0, linewidth = 1, position = position_dodge(0.4)) +
  geom_point(position = position_dodge(0.4), shape = 21, fill = "white", size = 2) +
  facet_wrap(~ dv + estimator, scales = "free") +
  theme_hanno() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(
    title = "Event Study Analysis",
    x = "Time (in months)",
    y = "Effect size",
    color = "Estimator"
  ) +
  # x axis from -5 to 5
  scale_x_continuous(breaks = seq(-5, 8, 1))


# use only callaway sant anna ---------------------------------------------


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

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/emp_polar_wo_controls.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/emp_polar_wo_controls.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/emp_polar_wo_controls.pdf", width = 7, height = 7)


# With controls -----------------------------------------------------------


# Define a function to perform event study analysis for a single variable
perform_event_study <- function(data, yname, idname, tname, gname, estimator, pvar) {
  event_study(
    data = data, yname = pvar, idname = idname,
    tname = tname, gname = gname,
    estimator = estimator,
    xformla = ~ education_cat + no_children_hh + partner + l1_net_monthly_income_cat + house_owner
  ) %>%
    mutate(dv = pvar)
}

# Initialize an empty list to store the results
event_study_results <- list()

# Loop over each variable in pvars
for (pvar in pvars) {
  # Perform event study analysis for the current variable
  result <- perform_event_study(
    data = df1,
    yname = pvar,
    idname = "id",
    tname = "t",
    gname = "first_treatment_period",
    estimator = "did",
    pvar = pvar
  )

  # Append the result to the list
  event_study_results[[pvar]] <- result
}

# Combine the results into a single data frame
combined_results_c <- bind_rows(event_study_results)

# save
fwrite(combined_results_c, "~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_controls_emp.csv")


# fread
combined_results_c <- fread("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/combined_results_controls_emp.csv")

# polarization results
combined_results_c |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("spread", "distance")) |>
  mutate(dv = ifelse(dv == "spread", "Affective Polarization (Spread)", "Affective Polarization (Distance)")) |>
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
  facet_wrap(~ dv, scales = "free")

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/emp_polar_controls.pdf", width = 7, height = 4)


# like min max results
combined_results_c |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("like_min", "like_max")) |>
  mutate(dv = ifelse(dv == "like_min", "Outgroup Aversion", "Ingroup Affinity")) |>
  filter(term >= -3 & term <= 8 & !is.na(term)) |>
  ggplot(aes(x = term, y = estimate, color = estimator)) +
  geom_point(position = position_dodge(0.4)) +
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
    color = "Like Measure"
  ) +
  # x axis from -5 to 8
  scale_x_continuous(breaks = seq(-5, 8, 1)) +
  facet_wrap(~ dv, scales = "free")


ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/emp_min_max_controls.pdf", width = 7, height = 4)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/emp_min_max_controls.pdf", width = 7, height = 4)

# outgroup and trust
combined_results_c |>
  filter(estimator %in% c("Callaway and Sant'Anna (2020)")) |>
  filter(dv %in% c("red_overall_mean_distance", "generalized_trust")) |>
  mutate(
    dv = ifelse(dv == "red_overall_mean_distance", "Heterophily (close social ties)", "Generalized Trust")
  ) |>
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
    color = "DV"
  ) +
  # x axis from -5 to 8
  scale_x_continuous(breaks = seq(-5, 8, 1)) +
  facet_wrap(~ dv, scales = "free")


ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/emp_aversion_control.pdf", width = 7, height = 4)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/emp_aversion_control.pdf", width = 7, height = 4)

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

ggsave("~/Dropbox (Princeton)/insecure_polarization/results/figures/2024_03_28/emp_polar_with_controls.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/Economic Insecurity, Trust, and Polarisation/Plots/emp_polar_with_controls.pdf", width = 7, height = 7)
ggsave("~/Dropbox (Princeton)/Apps/Overleaf/HB_insecurity_polarization/emp_polar_with_controls.pdf", width = 7, height = 7) 


# Time to re-employment analysis ----------------------------------------

glimpse(df1)

# Calculate time to re-employment for treated individuals
reemployment_time <- df1 |>
  filter(treat == 1) |>
  group_by(id) |>
  summarise(
    time_to_reemploy = sum(time_to_treatment <= 0), # counts periods until re-employment
    male = first(male)
  )

# Overall mean time to re-employment
mean_time <- mean(reemployment_time$time_to_reemploy, na.rm = TRUE)

# By gender
gender_means <- reemployment_time |>
  group_by(male) |>
  summarise(
    mean_time = mean(time_to_reemploy, na.rm = TRUE),
    sd_time = sd(time_to_reemploy, na.rm = TRUE),
    n = n()
  )


# T-test for gender difference
t_test_result <- t.test(time_to_reemploy ~ male, data = reemployment_time)


# Income recovery analysis ---------------------------------------------

names(df1)

income_change <- df1 |>
  filter(treat == 1) |>
  group_by(id) |>
  summarise(
    pre_personal_income = first(na.omit(net_monthly_income_cat[time_to_treatment == -1])),
    post_personal_income = first(na.omit(net_monthly_income_cat[time_to_treatment == 1])),
    pre_hh_income = first(na.omit(net_monthly_income_hh[time_to_treatment == -1])),
    post_hh_income = first(na.omit(net_monthly_income_hh[time_to_treatment == 1])),
    male = first(male)
  ) |>
  mutate(
    personal_income_change = post_personal_income - pre_personal_income,
    hh_income_change = post_hh_income - pre_hh_income
  )


# Income changes by gender
income_gender <- income_change |>
  group_by(male) |>
  summarise(
    mean_personal_change = mean(personal_income_change, na.rm = TRUE),
    mean_hh_change = mean(hh_income_change, na.rm = TRUE),
    n = n()
  )



t.test(personal_income_change ~ male, data = income_change)
t.test(hh_income_change ~ male, data = income_change)

