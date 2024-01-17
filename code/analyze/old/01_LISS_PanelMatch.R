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
pacman::p_load(tidyverse, data.table, PanelMatch)

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
treatment_hist <- DisplayTreatment(
  unit.id = "id",
  time.id = "wave",
  legend.position = "none",
  xlab = "Wave",
  ylab = "ID",
  title = "",
  treatment = "unemployed",
  data = df1,
  hide.y.tick.label = TRUE,
  dense.plot = TRUE
)

ggsave(filename = "unemployed_treatment_hist.pdf",
       plot = treatment_hist,
       path = "output/liss_panelmatch/",
       height = 8, width = 8)

## Partisan Affect ---------------------------------------------------------

# Match
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

# Open a pdf file
pdf("output/liss_panelmatch/unemp_pa_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off() 


# Covariate Balance Pre
cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "unemp_pa_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Partisan Affect")

# Plot
unemp_pa <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "unemp_pa_results.pdf",
       plot = unemp_pa,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Spread ------------------------------------------------------------------


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

# Open a pdf file
pdf("output/liss_panelmatch/unemp_sp_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()


cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "unemp_sp_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)

# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Spread")

# Plot
unemp_sp <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()
  
ggsave(filename = "unemp_sp_results.pdf",
       plot = unemp_sp,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Distance ------------------------------------------------------------------

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

# Open a pdf file
pdf("output/liss_panelmatch/unemp_di_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()



cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "unemp_di_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df1, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Distance")

# Plot
unemp_di <- coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "unemp_di_results.pdf",
       plot = unemp_di,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)



## Combined ----------------------------------------------------------------

unemp_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm) |>
  bind_rows(coefs_m3_pm)

plot_comb <- unemp_comb |>
  ggplot(aes(x=t, y = estimate, color = outcome, group = outcome)) +
  geom_point(aes(x = t, y = estimate),  size = 1, position = position_dodge(0.4)) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75, position = position_dodge(0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "") +
  scale_fill_discrete(name = "")

ggsave(filename = "unemp_results.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/unemp_results.png", width = 8, height = 5.5, dpi = 600)



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
                                    0) # never-treated should be zero or NA
  )


# As data.frame
df2 <- as.data.frame(df2)

# Visualize Treatment Distribution
treatment_hist <- DisplayTreatment(
  unit.id = "id",
  time.id = "wave",
  legend.position = "none",
  xlab = "Wave",
  ylab = "ID",
  title = "",
  treatment = "income_cat_decrease",
  data = df2,
  hide.y.tick.label = TRUE,
  dense.plot = TRUE
)

ggsave(filename = "incdec_treatment_hist.pdf",
       plot = treatment_hist,
       path = "output/liss_panelmatch/",
       height = 8, width = 8)

## Partisan Affect ---------------------------------------------------------

# Match
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

# Open a pdf file
pdf("output/liss_panelmatch/incdec_pa_bs.pdf") 
# 2. Create a plot
balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df2,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)
# Close the pdf file
dev.off() 


# Covariate Balance Pre
cb <- as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df2,
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

ggsave(filename = "incdec_pa_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df2, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Partisan Affect")

# Plot
incdec_pa <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdec_pa_results.pdf",
       plot = incdec_pa,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Spread ------------------------------------------------------------------


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

# Open a pdf file
pdf("output/liss_panelmatch/incdec_sp_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()


cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "incdec_sp_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)

# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df2, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Spread")

# Plot
incdec_sp <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "incdec_sp_results.pdf",
       plot = incdec_sp,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Distance ------------------------------------------------------------------

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

# Open a pdf file
pdf("output/liss_panelmatch/incdec_di_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()



cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "incdec_di_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df2, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Distance")

# Plot
incdec_di <- coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdec_di_results.pdf",
       plot = incdec_di,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)

## Combined ----------------------------------------------------------------

incdec_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm) |>
  bind_rows(coefs_m3_pm)

plot_comb <- incdec_comb |>
  ggplot(aes(x=t, y = estimate, color = outcome, group = outcome)) +
  geom_point(aes(x = t, y = estimate),  size = 1, position = position_dodge(0.4)) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75, position = position_dodge(0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "") +
  scale_fill_discrete(name = "")

ggsave(filename = "incdec_results.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/incdec_results.png", width = 8, height = 5.5, dpi = 600)


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
                                    0) # never-treated should be zero or NA
  )


# As data.frame
df3 <- as.data.frame(df3)

# Visualize Treatment Distribution
treatment_hist <- DisplayTreatment(
  unit.id = "id",
  time.id = "wave",
  legend.position = "none",
  xlab = "Wave",
  ylab = "ID",
  title = "",
  treatment = "income_cat_2p_decrease",
  data = df3,
  hide.y.tick.label = TRUE,
  dense.plot = TRUE
)

ggsave(filename = "incdec2p_treatment_hist.pdf",
       plot = treatment_hist,
       path = "output/liss_panelmatch/",
       height = 8, width = 8)

## Partisan Affect ---------------------------------------------------------

# Match
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

# Open a pdf file
pdf("output/liss_panelmatch/incdec2p_pa_bs.pdf") 
# 2. Create a plot
balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df3,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)
# Close the pdf file
dev.off() 


# Covariate Balance Pre
cb <- as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df3,
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

ggsave(filename = "incdec2p_pa_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df3, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Partisan Affect")

# Plot
incdec2p_pa <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdec2p_pa_results.pdf",
       plot = incdec2p_pa,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Spread ------------------------------------------------------------------


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

# Open a pdf file
pdf("output/liss_panelmatch/incdec2p_sp_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()


cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "incdec2p_sp_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)

# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df3, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Spread")

# Plot
incdec2p_sp <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "incdec2p_sp_results.pdf",
       plot = incdec2p_sp,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Distance ------------------------------------------------------------------

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

# Open a pdf file
pdf("output/liss_panelmatch/incdec2p_di_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()



cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "incdec2p_di_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df3, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Distance")

# Plot
incdec2p_di <- coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdec2p_di_results.pdf",
       plot = incdec2p_di,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)

## Combined ----------------------------------------------------------------

incdec2p_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm) |>
  bind_rows(coefs_m3_pm)

plot_comb <- incdec2p_comb |>
  ggplot(aes(x=t, y = estimate, color = outcome, group = outcome)) +
  geom_point(aes(x = t, y = estimate),  size = 1, position = position_dodge(0.4)) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75, position = position_dodge(0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "") +
  scale_fill_discrete(name = "")

ggsave(filename = "incdec2p_results.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/incdec2p_results.png", width = 8, height = 5.5, dpi = 600)



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
                                    0) # never-treated should be zero or NA
  )


# As data.frame
df4 <- as.data.frame(df4)

# Visualize Treatment Distribution
treatment_hist <- DisplayTreatment(
  unit.id = "id",
  time.id = "wave",
  legend.position = "none",
  xlab = "Wave",
  ylab = "ID",
  title = "",
  treatment = "income_hh_25p_decrease",
  data = df4,
  hide.y.tick.label = TRUE,
  dense.plot = TRUE
)

ggsave(filename = "incdechh_treatment_hist.pdf",
       plot = treatment_hist,
       path = "output/liss_panelmatch/",
       height = 8, width = 8)

## Partisan Affect ---------------------------------------------------------

# Match
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

# Open a pdf file
pdf("output/liss_panelmatch/incdechh_pa_bs.pdf") 
# 2. Create a plot
balance_scatter(
  matched_set_list = list(m1_pm$att),
  data = df4,
  covariates = c(
    "age",
    "education_cat",
    "no_children_hh",
    "partner",
    "l1_net_monthly_income_cat",
    "partisan_affect"
  )
)
# Close the pdf file
dev.off() 


# Covariate Balance Pre
cb <- as_tibble(get_covariate_balance(
  m1_pm$att,
  data = df4,
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

ggsave(filename = "incdechh_pa_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m1_pm, data = df4, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m1_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Partisan Affect")

# Plot
incdechh_pa <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdechh_pa_results.pdf",
       plot = incdechh_pa,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Spread ------------------------------------------------------------------


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

# Open a pdf file
pdf("output/liss_panelmatch/incdechh_sp_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()


cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "incdechh_sp_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)

# Estimate ATT
PM_est <- PanelEstimate(sets = m2_pm, data = df4, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m2_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Spread")

# Plot
incdechh_sp <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "incdechh_sp_results.pdf",
       plot = incdechh_sp,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Distance ------------------------------------------------------------------

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

# Open a pdf file
pdf("output/liss_panelmatch/incdechh_di_bs.pdf") 
# 2. Create a plot
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
# Close the pdf file
dev.off()



cb <- as_tibble(get_covariate_balance(
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

ggsave(filename = "incdechh_di_cov_bal.pdf",
       plot = cb,
       path = "output/liss_panelmatch/",
       height = 7, width = 8)


# Estimate ATT
PM_est <- PanelEstimate(sets = m3_pm, data = df4, se.method = "conditional")
summary(PM_est)

# Get tibble
coefs_m3_pm <- tibble(t = c(0, 1, 2, 3, 4), 
                      estimate = summary(PM_est)$summary[, 1], 
                      conf.low = summary(PM_est)$summary[, 3], 
                      conf.high = summary(PM_est)$summary[, 4]) |>
  mutate(outcome = "Distance")

# Plot
incdechh_di <- coefs_m3_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdechh_di_results.pdf",
       plot = incdechh_di,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)

## Combined ----------------------------------------------------------------

incdechh_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm) |>
  bind_rows(coefs_m3_pm)

plot_comb <- incdechh_comb |>
  ggplot(aes(x=t, y = estimate, color = outcome, group = outcome)) +
  geom_point(aes(x = t, y = estimate),  size = 1, position = position_dodge(0.4)) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75, position = position_dodge(0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "") +
  scale_fill_discrete(name = "")

ggsave(filename = "incdechh_results.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/incdechh_results.png", width = 8, height = 5.5, dpi = 600)


### END