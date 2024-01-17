# Outgroup risk aversion --------------------------------------------------


## Unemployment ------------------------------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(unemployed == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)


### Fixed effects model ----------------------------------------------------
m1_fe <- feols(out_group_risk_aversion_raw ~ unemployment_shock
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "out_group_risk_aversion_raw", idname = "id",
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
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:out_group_risk_aversion_raw), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "out_group_risk_aversion_raw" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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


## Income decrease ---------------------------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(income_cat_decrease == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)


### Fixed effects model ----------------------------------------------------
m1_fe <- feols(out_group_risk_aversion_raw ~ income_cat_decrease
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)


### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "out_group_risk_aversion_raw", idname = "id",
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
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:out_group_risk_aversion_raw), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "out_group_risk_aversion_raw" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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



## Income 2 point decrease ---------------------------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(income_cat_2p_decrease == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)

### Fixed effects model ----------------------------------------------------
m1_fe <- feols(out_group_risk_aversion_raw ~ income_cat_2p_decrease
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "out_group_risk_aversion_raw", idname = "id",
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
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:out_group_risk_aversion_raw), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "out_group_risk_aversion_raw" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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



## Household income 25 perc decrease ---------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(income_hh_25p_decrease == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)


### Fixed effects model ----------------------------------------------------
m1_fe <- feols(out_group_risk_aversion_raw ~ income_hh_25p_decrease
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "out_group_risk_aversion_raw", idname = "id",
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
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "out_group_risk_aversion_raw"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:out_group_risk_aversion_raw), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "out_group_risk_aversion_raw" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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




# Feel connected to others ------------------------------------------------


## Unemployment ------------------------------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(unemployed == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)


### Fixed effects model ----------------------------------------------------
m1_fe <- feols(feel_connected_to_others ~ unemployment_shock
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "feel_connected_to_others", idname = "id",
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
    I(lag(feel_connected_to_others, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "feel_connected_to_others",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:feel_connected_to_others), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "feel_connected_to_others" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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


## Income decrease ---------------------------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(income_cat_decrease == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)


### Fixed effects model ----------------------------------------------------
m1_fe <- feols(feel_connected_to_others ~ income_cat_decrease
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)


### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "feel_connected_to_others", idname = "id",
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
    I(lag(feel_connected_to_others, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "feel_connected_to_others",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:feel_connected_to_others), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "feel_connected_to_others" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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



## Income 2 point decrease ---------------------------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(income_cat_2p_decrease == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)

### Fixed effects model ----------------------------------------------------
m1_fe <- feols(feel_connected_to_others ~ income_cat_2p_decrease
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "feel_connected_to_others", idname = "id",
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
    I(lag(feel_connected_to_others, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "feel_connected_to_others",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:feel_connected_to_others), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "feel_connected_to_others" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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



## Household income 25 perc decrease ---------------------------------------

# Create treatment variable
df <- df |> mutate(treated = ifelse(income_hh_25p_decrease == 1, 1, 0))

# Get treated units
treat <- df |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df <- df |>
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
df <- as.data.frame(df)


### Fixed effects model ----------------------------------------------------
m1_fe <- feols(feel_connected_to_others ~ income_hh_25p_decrease
               + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
               | id + t,
               cluster = "id",
               data = df)
summary(m1_fe)

### Callaway Sant'Anna -----------------------------------------------------
m1_cs <- event_study(
  data = df, yname = "feel_connected_to_others", idname = "id",
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
    I(lag(feel_connected_to_others, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "feel_connected_to_others",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  )
)

as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "feel_connected_to_others"
  ),
  plot = FALSE
),
rownames = "t") |>
  pivot_longer(cols = c(age:feel_connected_to_others), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey",
                                "no_children_hh" = "grey", "partner" = "grey",
                                "l1_net_monthly_income_cat" = "grey", "feel_connected_to_others" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df, se.method = "conditional")
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

