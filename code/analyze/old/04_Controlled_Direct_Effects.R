### # Heddesheimer, Sairam, Bryson - Replication code - Month XX, 2023
# vincent.heddesheimer@princeton.edu
#
# Tables reproduced in this file:
# 
# Figures reproduced in this file:
# 
### ### ### ###

rm(list = ls())

# Install development version of DirectEffects package
# install.packages("devtools")
# devtools::install_github("mattblackwell/DirectEffects", build_vignettes = TRUE)

# Load packages
pacman::p_load(tidyverse, data.table, hdm, DirectEffects, mediation)

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




# Code mediator -----------------------------------------------------------

df <- df |>
  mutate(
    generalized_trust_disc = case_when(
      generalized_trust < quantile(df$generalized_trust, probs = .25, na.rm = T, names = F) ~ 1,
      generalized_trust >= quantile(df$generalized_trust, probs = .25, na.rm = T, names = F) & generalized_trust < quantile(df$generalized_trust, probs = .75, na.rm = T, names = F)  ~  2,
      generalized_trust >= quantile(df$generalized_trust, probs = .75, na.rm = T, names = F) ~ 3
    ),
    generalized_trust_change = case_when(
      delta_generalized_trust < 0 ~ 1,
      delta_generalized_trust == 0 ~ 2,
      delta_generalized_trust > 0 ~ 3
    )
  )


# Transform dataframe -----------------------------------------------------

df <- df |>
  group_by(nomem_encr) |>
  mutate(
    # Treatment (t-2)
    unemployment_shock = ifelse(is.na(unemployment_shock), 0, unemployment_shock),
    treat_unemp_shock = lag(unemployment_shock, n = 2, order_by = wave),
    # Mediator (t-1)
    med_general_trust = lag(generalized_trust_disc, n = 1, order_by = wave),
    # Pre-treatment controls (t-2)
    age_l2 = lag(age, n = 2, order_by = wave),
    edu_l2 = lag(education_cat, n = 2, order_by = wave),
    no_children_hh_l2 = lag(no_children_hh, n = 2, order_by = wave),
    partner_l2 = lag(partner, n = 2, order_by = wave),
    student_l2 = lag(student, n = 2, order_by = wave),
    retired_l2 = lag(retired, n = 2, order_by = wave)
  ) |>
  ungroup()

# inspect <- df |>
#   select(nomem_encr, wave, unemployment_shock, treat_unemp_shock, generalized_trust_disc, med_general_trust)


# Controlled direct effects -----------------------------------------------

## Unemployment ------------------------------------------------------------

m1 <- cde_did_aipw(base_mediator = med_general_trust) |>
  set_treatment(treat_unemp_shock,
                ~ age_l2 + edu_l2 + no_children_hh_l2 + partner_l2 + student_l2 + retired_l2) |>
  treat_model(engine = "logit") |>
  outreg_model(engine = "lm") |>
  set_treatment(med_general_trust, ~ l1_net_monthly_income_cat) |>
  treat_model(engine = "multinom") |>
  outreg_model(engine = "lm") |>
  estimate(delta_spread ~ treat_unemp_shock + med_general_trust, data = df)

summary(m1)