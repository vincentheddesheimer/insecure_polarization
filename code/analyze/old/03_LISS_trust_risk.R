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


## Generalized Trust ---------------------------------------------------------

# Match
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(generalized_trust, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df1,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "generalized_trust",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

# Open a pdf file
pdf("output/liss_panelmatch/unemp_gt_bs.pdf") 
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
    "generalized_trust"
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
    "generalized_trust"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:generalized_trust), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "generalized_trust" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")

ggsave(filename = "unemp_gt_cov_bal.pdf",
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
  mutate(outcome = "Generalized Trust")

# Plot
unemp_gt <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "unemp_gt_results.pdf",
       plot = unemp_gt,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Risk aversion ------------------------------------------------------------------

m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df1,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

# Open a pdf file
pdf("output/liss_panelmatch/unemp_ra_bs.pdf") 
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
    "out_group_risk_aversion_raw"
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

ggsave(filename = "unemp_ra_cov_bal.pdf",
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
  mutate(outcome = "Out Group Risk Aversion")

# Plot
unemp_ra <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "unemp_ra_results.pdf",
       plot = unemp_ra,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)



## Combined ----------------------------------------------------------------

unemp_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm)

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

ggsave(filename = "unemp_results_mechanisms.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/unemp_results_mechanisms.png", width = 8, height = 5.5, dpi = 600)



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


## Generalized Trust ---------------------------------------------------------

# Match
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(generalized_trust, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df2,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "generalized_trust",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

# Open a pdf file
pdf("output/liss_panelmatch/incdec_gt_bs.pdf") 
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
    "generalized_trust"
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
    "generalized_trust"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:generalized_trust), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "generalized_trust" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")

ggsave(filename = "incdec_gt_cov_bal.pdf",
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
  mutate(outcome = "Generalized Trust")

# Plot
incdec_gt <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdec_gt_results.pdf",
       plot = incdec_gt,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Risk aversion ------------------------------------------------------------------

m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df2,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

# Open a pdf file
pdf("output/liss_panelmatch/incdec_ra_bs.pdf") 
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
    "out_group_risk_aversion_raw"
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

ggsave(filename = "incdec_ra_cov_bal.pdf",
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
  mutate(outcome = "Out Group Risk Aversion")

# Plot
incdec_ra <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "incdec_ra_results.pdf",
       plot = incdec_ra,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)



## Combined ----------------------------------------------------------------

incdec_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm)

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

ggsave(filename = "incdec_results_mechanisms.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/incdec_results_mechanisms.png", width = 8, height = 5.5, dpi = 600)




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


## Generalized Trust ---------------------------------------------------------

# Match
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(generalized_trust, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df3,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "generalized_trust",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

# Open a pdf file
pdf("output/liss_panelmatch/incdec2p_gt_bs.pdf") 
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
    "generalized_trust"
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
    "generalized_trust"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:generalized_trust), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "generalized_trust" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")

ggsave(filename = "incdec2p_gt_cov_bal.pdf",
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
  mutate(outcome = "Generalized Trust")

# Plot
incdec2p_gt <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdec2p_gt_results.pdf",
       plot = incdec2p_gt,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Risk aversion ------------------------------------------------------------------

m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df3,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

# Open a pdf file
pdf("output/liss_panelmatch/incdec2p_ra_bs.pdf") 
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
    "out_group_risk_aversion_raw"
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

ggsave(filename = "incdec2p_ra_cov_bal.pdf",
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
  mutate(outcome = "Out Group Risk Aversion")

# Plot
incdec2p_ra <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "incdec2p_ra_results.pdf",
       plot = incdec2p_ra,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)



## Combined ----------------------------------------------------------------

incdec2p_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm)

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

ggsave(filename = "incdec2p_results_mechanisms.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/incdec2p_results_mechanisms.png", width = 8, height = 5.5, dpi = 600)



# Household income 25 percent decrease ---------------------------------------------------------

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


## Generalized Trust ---------------------------------------------------------

# Match
m1_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(generalized_trust, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df4,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "generalized_trust",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m1_pm)

# Open a pdf file
pdf("output/liss_panelmatch/incdechh_gt_bs.pdf") 
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
    "generalized_trust"
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
    "generalized_trust"
  ),
  plot = FALSE
),
rownames = "t") |> 
  pivot_longer(cols = c(age:generalized_trust), names_to = "covariate", values_to = "covbal") |>
  mutate(t = as.integer(str_replace(t, "t_", "-"))) |>
  ggplot(aes(x = t, y = covbal, color = covariate)) +
  geom_line() +
  scale_color_manual(values = c("age" = "grey", "education_cat" = "grey", 
                                "no_children_hh" = "grey", "partner" = "grey", 
                                "l1_net_monthly_income_cat" = "grey", "generalized_trust" = "black")) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = -1, linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(-.5,.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y="Standardized Mean Difference of Covariates", x = "Time periods relative to administration of treatment")

ggsave(filename = "incdechh_gt_cov_bal.pdf",
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
  mutate(outcome = "Generalized Trust")

# Plot
incdechh_gt <- coefs_m1_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") +
  haschaR::theme_hanno()

ggsave(filename = "incdechh_gt_results.pdf",
       plot = incdechh_gt,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)


## Risk aversion ------------------------------------------------------------------

m2_pm <- PanelMatch(
  lag = 4,
  time.id = "t",
  unit.id = "id",
  covs.formula = ~ age + education_cat + no_children_hh + partner + l1_net_monthly_income_cat +
    I(lag(out_group_risk_aversion_raw, 1:4)), # control for pre-treatment outcome variable (parallel trends)
  treatment = "treated",
  refinement.method = "CBPS.weight",
  data = df4,
  match.missing = TRUE,
  qoi = "att" ,
  outcome.var = "out_group_risk_aversion_raw",
  lead = 0:4,
  forbid.treatment.reversal = FALSE
)


# Inspect covariate balance of matched set
print(m2_pm)

# Open a pdf file
pdf("output/liss_panelmatch/incdechh_ra_bs.pdf") 
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
    "out_group_risk_aversion_raw"
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

ggsave(filename = "incdechh_ra_cov_bal.pdf",
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
  mutate(outcome = "Out Group Risk Aversion")

# Plot
incdechh_ra <- coefs_m2_pm |>
  ggplot(aes(x=t, y = estimate)) +
  geom_point(aes(x = t, y = estimate),  size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
  labs(y="ATT", x = "Relative Time") + 
  haschaR::theme_hanno()

ggsave(filename = "incdechh_ra_results.pdf",
       plot = incdechh_ra,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)



## Combined ----------------------------------------------------------------

incdechh_comb <- coefs_m1_pm |>
  bind_rows(coefs_m2_pm)

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

ggsave(filename = "incdechh_results_mechanisms.pdf",
       plot = plot_comb,
       path = "output/liss_panelmatch/",
       width = 8, height = 5.5)
ggsave("report/images/incdechh_results_mechanisms.png", width = 8, height = 5.5, dpi = 600)




### END