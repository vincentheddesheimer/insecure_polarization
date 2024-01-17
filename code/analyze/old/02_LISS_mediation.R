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



# Mediation package -------------------------------------------------------

med.fit <- lm(out_group_risk_aversion_raw ~ unemployment_shock
              + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
              + factor(id) + factor(t),
              data = df)

out.fit <- lm(delta_spread ~ unemployment_shock + out_group_risk_aversion_raw 
              + age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat
              + factor(id) + factor(t),
              data = df)

med.out <- mediate(med.fit, out.fit, 
                   treat = "unemployment_shock", mediator = "out_group_risk_aversion_raw",
                   boot = TRUE, robustSE = TRUE, sims = 100)
summary(med.out)


sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)

plot(sens.out, sens.par = "rho", main = "Out Group Risk Aversion", ylim = c(-0.2, 0.2))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")



