#rerun analysis old data netherlands Leonard Baum 20.09.23
# Tables reproduced in this file:
# 
# Figures reproduced in this file:
# 
### ### ### ###

rm(list = ls())

# set wd
setwd("/Users/leonardbaum/Desktop/insecure_polarization/Data")
#needs to be adjusted

# load packages
library(tidyverse)  # 1.3.2
library(modelsummary) # 1.0.2
library(kableExtra)
library(marginaleffects)  # 0.6.0
library(lme4) # 3.1-3     
library(clubSandwich)
library(fixest)
library(stargazer)
#library(ivreg)
# library(RColorBrewer)         
# library(gridExtra)
# library(plm)

# library(sandwich)
# library(lmtest)


# library(effectsize)
# 
# library(viridis)



### Part 1 Macro Analysis

# Macro including all countries
# Table 1 -----------------------------------------------------------------

# load data
load("df.RData")

# Macro level: polarization (API)

# rename iso3c variable for robust standard errors in tables
df <- df |> rename(country = iso3c)


# Models
models <- list(
  "Model 1" = lm(dislike_AP ~ gini_disp + gdpgrowth + unempILO, data = df),
  "Model 2" = lm(dislike_AP ~ gini_disp + gdpgrowth + unempILO + agedep, data = df),
  "Model 3" = lm(dislike_AP ~ gini_disp + gdpgrowth + unempILO + agedep + EFindex, data = df),
  "Model 4" = lm(dislike_AP ~  gdpgrowth + unempILO + agedep + factor(country), data = df),
  "Model 5" = lm(dislike_AP ~  gdpgrowth + unempILO + agedep + EFindex + factor(country), data = df))

cm <- c("gini_disp" = "Income Inequality", 
        "gdpgrowth" = "GDP Growth", 
        "unempILO" = "Unemployment",
        "agedep"    = "Age Dependency", 
        "EFindex"   = "Ethnic Fragmentation")

rows <- tribble(
  ~term, ~'Model 1', ~'Model 2', ~'Model 3', ~'Model 4', ~'Model 5',
  "Country FEs", "", "", "", '\\checkmark', '\\checkmark')

attr(rows, 'position') <- c(11)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  vcov = ~ country,
  add_rows = rows,
  title = 'Effect of Economic Insecurity on Affective Polarization (WOPE)',
  coef_map = cm,
  gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  # ,output = "latex"
)

########macro analysis only for netherlands


#subset netherlands

nld <- subset(df, country == "NLD")

# won't give us much information with only one country at the macro level, so let's just do a simplified version of the lm models
nld1 <- lm(dislike_AP ~ gini_disp + gdpgrowth + unempILO, data = nld)
nld2 <- lm(dislike_AP ~ gini_disp + gdpgrowth + unempILO + agedep, data = nld)
nld3 <- lm(dislike_AP ~ gini_disp + gdpgrowth + unempILO + agedep + EFindex, data = nld)
nld4 <- lm(dislike_AP ~ gdpgrowth + unempILO + agedep, data = nld)
nld5 <- lm(dislike_AP ~ gdpgrowth + unempILO + agedep + EFindex, data = nld)

nld_models <- list(nld1, nld2, nld3, nld4, nld5)

# Summarize the models using stargazer
stargazer(nld_models, 
          title = "LM Models for Netherlands",
          type = "text", 
          star.cutoffs = c(0.1, 0.05, 0.01) 
)
##unsurprisingly with 4 observation points there is no significance across models

#### let's check as well for table 2



# Redoing Table 2 -----------------------------------------------------------------
# Macro level: trust (ESS)

models <- list(
  "Model 1" = lm(trustperc_ess ~ gini_disp + gdpgrowth + unempILO, data = df),
  "Model 2" = lm(trustperc_ess ~ gini_disp + gdpgrowth + unempILO + agedep, data = df),
  "Model 3" = lm(trustperc_ess ~ gini_disp + gdpgrowth + unempILO + agedep + EFindex, data = df),
  "Model 4" = lm(trustperc_ess ~  gdpgrowth + unempILO + agedep + factor(country), data = df),
  "Model 5" = lm(trustperc_ess ~  gdpgrowth + unempILO + agedep + EFindex + factor(country), data = df))

cm <- c("gini_disp" = "Income Inequality", 
        "gdpgrowth" = "GDP Growth", 
        "unempILO" = "Unemployment",
        "agedep"    = "Age Dependency", 
        "EFindex"   = "Ethnic Fragmentation")

rows <- tribble(
  ~term, ~'Model 1', ~'Model 2', ~'Model 3', ~'Model 4', ~'Model 5',
  "Country FEs", "", "", "", '\\checkmark', '\\checkmark')

attr(rows, 'position') <- c(11)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  vcov = ~ country,
  add_rows = rows,
  title = 'Effect of Economic Insecurity on Trust (ESS)',
  coef_map = cm,
  gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  # ,output = "latex"
)

#now we check again for the nld subset 

nld6 <- lm(trustperc_ess ~ gini_disp + gdpgrowth + unempILO, data = nld)
nld7 <- lm(trustperc_ess ~ gini_disp + gdpgrowth + unempILO + agedep, data = nld)
nld8 <- lm(trustperc_ess ~ gini_disp + gdpgrowth + unempILO + agedep + EFindex, data = nld)
nld9 <- lm(trustperc_ess ~ gdpgrowth + unempILO + agedep, data = nld)
nld10 <- lm(trustperc_ess ~ gdpgrowth + unempILO + agedep + EFindex, data = nld)

# Create a list of the models
nld_models_ess <- list(nld6, nld7, nld8, nld9, nld10)

# Summarize the models using stargazer
stargazer(nld_models_ess, 
          title = "Linear Regression Models for Netherlands (ESS)",
          type = "text", 
          star.cutoffs = c(0.1, 0.05, 0.01) 
)

#only in model 5 ethic fragmentation is significant, but, again, generally with 8 observation points macro models are not informative



####### now we come to the more relevant micro analysis

# load data
ess <- read_csv("ess.csv")
load("cses_micro.RData")

# rename region for clustering presentation
ess <- ess |>
  rename(
    region = nutscode,
    country = cntry,
    unemp = unempl,
    education = eduyrs
  ) |>
  # create dummy variable for low income
  mutate(
    low_income = ifelse(income < 3, 1, 0),
    country = as.factor(country),
    year = as.factor(year),
    region = as.factor(region),
    # inverse bad_econ to map with cses coding
    bad_economy = abs(stfeco-10)
  )

# get country averages for macroeconomic variables
av_gini_disp_ess = ess |>
  group_by(country) |>
  summarize_at("gini_disp", mean, na.rm=T) |>
  rename(av_gini_disp = gini_disp)

av_gdpgrowth_ess = ess |>
  group_by(country) |>
  summarize_at("gdpgrowth", mean, na.rm=T) |>
  rename(av_gdpgrowth = gdpgrowth)

av_unempILO_ess = ess |>
  group_by(country) |>
  summarize_at("unempILO", mean, na.rm=T) |>
  rename(av_unempILO = unempILO)

# merge
ess <- ess |>
  left_join(av_gini_disp_ess, by = "country") |>
  left_join(av_gdpgrowth_ess, by = "country") |>
  left_join(av_unempILO_ess, by = "country")


# rename umemp for cses
cses <- cses |>
  rename(unempILO = unemp_tot,
         income = income_household,
         relig = religiosity) |>
  mutate(
    country = as.factor(country),
    year = as.factor(year),
    region = as.factor(region)
  )

# get country averages for macroeconomic variables
av_gini_disp_cses = cses |>
  group_by(country) |>
  summarize_at("gini_disp", mean, na.rm=T) |>
  rename(av_gini_disp = gini_disp)

av_gdpgrowth_cses = cses |>
  group_by(country) |>
  summarize_at("gdpgrowth", mean, na.rm=T) |>
  rename(av_gdpgrowth = gdpgrowth)

av_unempILO_cses = cses |>
  group_by(country) |>
  summarize_at("unempILO", mean, na.rm=T) |>
  rename(av_unempILO = unempILO)

# merge
cses <- cses |>
  left_join(av_gini_disp_cses, by = "country") |>
  left_join(av_gdpgrowth_cses, by = "country") |>
  left_join(av_unempILO_cses, by = "country")

####creat a subset with only for the netherlands

nld_micro1 <- cses[cses$country == "Netherlands", ]
nld_micro2 <- ess[ess$country == "NL", ]

# Models using netherlands subset
mm1 = feols(likedistwgt ~ income + unemp + bad_economy + age + education + female + relig + union + urban | year, data = nld_micro1)

#mm2 = lmer(likedistwgt ~ income + unemp + bad_economy + age + education + female + relig + union + urban + I(gini_disp - av_gini_disp) + I(gdpgrowth - av_gdpgrowth) + I(unempILO - av_unempILO) + av_gini_disp + av_gdpgrowth + av_unempILO + (1|year), data = nld_micro1)
#m2 doesn't work due to error "grouping factors must have > 1 sampled level
#not sure why random effects model doesn't work (year variable still has multiple factors)

#alternative approach model 2
#linear regression regression model without random effects
m2alt <- lm(likedistwgt ~ income + unemp + bad_economy + age + education + female + relig + union + urban + 
                 I(gini_disp - av_gini_disp) + I(gdpgrowth - av_gdpgrowth) + I(unempILO - av_unempILO) + 
                 av_gini_disp + av_gdpgrowth + av_unempILO, data = nld_micro1)

summary(m2alt)

#reminder: alternative approach doesn't account for potential correlations or unobserved variation between years as effectively as a mixed-effects model would




#m3 = feols(likedistwgt ~ income + unemp + bad_economy + age + education + female + relig + union + urban | year | shock_china_ind ~ inst_china_ind, data = nld_micro1)
#m3 doesn't work with the given Netherlands subset because  variables shock_china_ind and inst_china_ind contain only NAs. 
# all observations contain NA values

mm4 = feols(trust_people ~ income + unemp + bad_economy + age + education + female + relig + union + urban | year, data = nld_micro2)
mm5 = lmer(trust_people ~ income + unemp + bad_economy + age + education + female + relig + union + urban + I(gini_disp - av_gini_disp) + I(gdpgrowth - av_gdpgrowth) + I(unempILO - av_unempILO) + av_gini_disp + av_gdpgrowth + av_unempILO + (1|year), data = nld_micro2)
mm6 = feols(trust_people ~ income + unemp + bad_economy + age + education + female + relig + union + urban | year | shock_china_ind ~ inst_china_ind, data = nld_micro2)

###going forward with the 4 functioning models

#looking at the models separately
summary(mm1)
summary(mm4)
summary(mm5)
summary(mm6)


