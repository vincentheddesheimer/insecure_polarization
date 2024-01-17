### # Heddesheimer, Sairam, Bryson - Replication code - Month XX, 2023
# vincent.heddesheimer@princeton.edu
#
# Tables reproduced in this file:
# 
# Figures reproduced in this file:
# 
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, did2s, PanelMatch)

# Set wd
setwd("~/Documents/GitHub/insecure_polarization/")

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


# # Delete observations that only appear once
# single_obs <- df |>
#   count(id) |>
#   filter(n < 2) |>
#   pull(id)
# 
# df <- df |> filter(!(id %in% single_obs))
# 
# # Create consecutive time period
# ids <- df |>
#   distinct(id) |>
#   pull(id)
# 
# data.frame <- tibble(id = rep(ids, 15)) |>
#   arrange(id) |>
#   mutate(t = rep(c(1:15), 13615))
# 
# df <- df |> right_join(data.frame, by = c("id", "t"))


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


# As data.frame
df1 <- as.data.frame(df1)

# Visualize Treatment Distribution
DisplayTreatment(unit.id = "id",
                 time.id = "wave", legend.position = "none",
                 xlab = "Wave", ylab = "ID", title = "",
                 treatment = "unemployed", data = df1, 
                 hide.y.tick.label = TRUE,
                 dense.plot = TRUE)

## Partisan Affect ---------------------------------------------------------

### Fixed effects model ----------------------------------------------------
m1_fe <- feols(partisan_affect ~ unemployment_shock 
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df1, yname = "partisan_affect", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m1_cs <- m1_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m1_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match -----------------------------------------------------------
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(partisan_affect, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df1,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "partisan_affect",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:partisan_affect), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "partisan_affect" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


## Spread ------------------------------------------------------------------

### Fixed effects model -----------------------------------------------------

m2_fe <- feols(spread ~ unemployment_shock 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna ------------------------------------------------------

m2_cs <- event_study(
  data = df1, yname = "spread", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat,
  estimator = "did")

coefs_m2_cs <- m2_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coefs_m2_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ------------------------
m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(spread, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df1,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "spread",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

balance_scatter(
  matched_set_list = list(m2_pm$att),
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  )
)

as_tibble(get_covariate_balance(
  m2_pm$att,
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:spread), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "spread" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Distance ------------------------------------------------------------------

### Fixed effects model ------------------------------------------------------
m3_fe <- feols(distance ~ unemployment_shock 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -------------------------------------------------------
m3_cs <- event_study(
  data = df1, yname = "distance", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m3_cs <- m3_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m3_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ---------------------------------------------------------

m3_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(distance, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df1,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "distance",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m3_pm)

balance_scatter(
  matched_set_list = list(m3_pm$att),
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  )
)

as_tibble(get_covariate_balance(
  m3_pm$att,
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:distance), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "distance" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))




# Income decrease ---------------------------------------------------------

# Create treatment variable
df2 <- df |> mutate(treated = ifelse(income_cat_decrease == 1, 1, 0))

# Get treated units
treat <- df2 |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df2 <- df2 |>
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
                                    0)
  )

# As data.frame
df2 <- as.data.frame(df2)

# Visualize Treatment Distribution
DisplayTreatment(unit.id = "id",
                 time.id = "wave", legend.position = "none",
                 xlab = "Wave", ylab = "ID", title = "",
                 treatment = "income_cat_decrease", data = df2, 
                 hide.y.tick.label = TRUE,
                 dense.plot = TRUE)

## Partisan Affect ---------------------------------------------------------

### Fixed effects model ----------------------------------------------------
m1_fe <- feols(partisan_affect ~ income_cat_decrease 
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df2, yname = "partisan_affect", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m1_cs <- m1_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m1_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match -----------------------------------------------------------
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(partisan_affect, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df2,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "partisan_affect",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:partisan_affect), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "partisan_affect" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Spread ------------------------------------------------------------------

### Fixed effects model -----------------------------------------------------

m2_fe <- feols(spread ~ income_cat_decrease 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna ------------------------------------------------------

m2_cs <- event_study(
  data = df2, yname = "spread", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat,
  estimator = "did")

coefs_m2_cs <- m2_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coefs_m2_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ------------------------
m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(spread, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df2,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "spread",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

balance_scatter(
  matched_set_list = list(m2_pm$att),
  data = df2,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  )
)

as_tibble(get_covariate_balance(
  m2_pm$att,
  data = df2,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:spread), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "spread" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df2, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Distance ------------------------------------------------------------------

### Fixed effects model ------------------------------------------------------
m3_fe <- feols(distance ~ income_cat_decrease 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -------------------------------------------------------
m3_cs <- event_study(
  data = df2, yname = "distance", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m3_cs <- m3_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m3_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ---------------------------------------------------------

m3_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(distance, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df2,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "distance",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m3_pm)

balance_scatter(
  matched_set_list = list(m3_pm$att),
  data = df2,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  )
)

as_tibble(get_covariate_balance(
  m3_pm$att,
  data = df2,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:distance), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "distance" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df2, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))





# Income 2 point decrease ---------------------------------------------------------

# Create treatment variable
df3 <- df |> mutate(treated = ifelse(income_cat_2p_decrease == 1, 1, 0))

# Get treated units
treat <- df3 |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df3 <- df3 |>
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
                                    0)
  )

# As data.frame
df3 <- as.data.frame(df3)

# Visualize Treatment Distribution
DisplayTreatment(unit.id = "id",
                 time.id = "wave", legend.position = "none",
                 xlab = "Wave", ylab = "ID", title = "",
                 treatment = "income_cat_2p_decrease", data = df3, 
                 hide.y.tick.label = TRUE,
                 dense.plot = TRUE)

## Partisan Affect ---------------------------------------------------------

### Fixed effects model ----------------------------------------------------
m1_fe <- feols(partisan_affect ~ income_cat_2p_decrease 
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df3, yname = "partisan_affect", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m1_cs <- m1_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m1_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match -----------------------------------------------------------
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(partisan_affect, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df3,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "partisan_affect",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:partisan_affect), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "partisan_affect" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Spread ------------------------------------------------------------------

### Fixed effects model -----------------------------------------------------

m2_fe <- feols(spread ~ income_cat_2p_decrease 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna ------------------------------------------------------

m2_cs <- event_study(
  data = df3, yname = "spread", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat,
  estimator = "did")

coefs_m2_cs <- m2_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coefs_m2_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ------------------------
m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(spread, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df3,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "spread",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

balance_scatter(
  matched_set_list = list(m2_pm$att),
  data = df3,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  )
)

as_tibble(get_covariate_balance(
  m2_pm$att,
  data = df3,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:spread), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "spread" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df3, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Distance ------------------------------------------------------------------

### Fixed effects model ------------------------------------------------------
m3_fe <- feols(distance ~ income_cat_2p_decrease 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -------------------------------------------------------
m3_cs <- event_study(
  data = df3, yname = "distance", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m3_cs <- m3_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m3_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ---------------------------------------------------------

m3_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(distance, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df3,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "distance",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m3_pm)

balance_scatter(
  matched_set_list = list(m3_pm$att),
  data = df3,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  )
)

as_tibble(get_covariate_balance(
  m3_pm$att,
  data = df3,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:distance), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "distance" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df3, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))






# Household income 25 perc decrease ---------------------------------------

# Create treatment variable
df4 <- df |> mutate(treated = ifelse(income_hh_25p_decrease == 1, 1, 0))

# Get treated units
treat <- df4 |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df4 <- df4 |>
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
                                    0)
  )

# As data.frame
df4 <- as.data.frame(df4)

# Visualize Treatment Distribution
DisplayTreatment(unit.id = "id",
                 time.id = "wave", legend.position = "none",
                 xlab = "Wave", ylab = "ID", title = "",
                 treatment = "income_hh_25p_decrease", data = df4, 
                 hide.y.tick.label = TRUE,
                 dense.plot = TRUE)

## Partisan Affect ---------------------------------------------------------

### Fixed effects model ----------------------------------------------------
m1_fe <- feols(partisan_affect ~ income_hh_25p_decrease 
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df4, yname = "partisan_affect", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m1_cs <- m1_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m1_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match -----------------------------------------------------------
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(partisan_affect, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df4,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "partisan_affect",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df1,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:partisan_affect), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "partisan_affect" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Spread ------------------------------------------------------------------

### Fixed effects model -----------------------------------------------------

m2_fe <- feols(spread ~ income_hh_25p_decrease 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna ------------------------------------------------------

m2_cs <- event_study(
  data = df4, yname = "spread", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat,
  estimator = "did")

coefs_m2_cs <- m2_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coefs_m2_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ------------------------
m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(spread, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df4,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "spread",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

balance_scatter(
  matched_set_list = list(m2_pm$att),
  data = df4,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  )
)

as_tibble(get_covariate_balance(
  m2_pm$att,
  data = df4,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "spread"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:spread), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "spread" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df4, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))



## Distance ------------------------------------------------------------------

### Fixed effects model ------------------------------------------------------
m3_fe <- feols(distance ~ income_hh_25p_decrease 
               + age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat
               | nomem_encr + wave,
               cluster = "nomem_encr",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -------------------------------------------------------
m3_cs <- event_study(
  data = df4, yname = "distance", idname = "id",
  tname = "t", gname = "first_treatment_period",
  xformla = ~ age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat,
  estimator = "did")

coef_m3_cs <- m3_cs |> 
  select(estimator, t = term, estimate, std.error) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    t = as.numeric(t)
  )

coef_m3_cs |> filter(t >= -5 & t <= 10) |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### Panel Match ---------------------------------------------------------

m3_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(distance, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df4,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "distance",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m3_pm)

balance_scatter(
  matched_set_list = list(m3_pm$att),
  data = df4,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  )
)

as_tibble(get_covariate_balance(
  m3_pm$att,
  data = df4,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "distance"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:distance), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "distance" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df4, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  select(t, estimate, conf.low, conf.high) |>
  mutate(estimator = "Panel Match")

# Plot
coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative Time") + 
  guides(col = guide_legend(nrow = 3))


### END