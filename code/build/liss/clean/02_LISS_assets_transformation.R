### LISS assets transformation


rm(list=ls())

# packages
library(data.table)
library(tidyverse)
library(readstata13)
library(furrr)
library(lubridate)


# use multisession evaluation
plan(multisession)

# set wd
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/assets/")

# read .dta files 
ass <- 
  # list all .dta files in directory
  list.files(pattern = "*.dta") |>
  # read all .dta files in directory
  future_map(~read.dta13(.)) |>
  # delete first five characters of all variable names starting with c
  future_map(~rename_with(.,str_sub, start = 6L, .cols = starts_with("c"))) |>
  # delete first first characters of all variable names starting with _
  future_map(~rename_with(.,str_sub, start = 2L, .cols = starts_with("_"))) |>
  # every column to character
  future_map(~mutate(.x, across(everything(), as.character))) |>
  # bind all data frames
  bind_rows() |>
  # create year and month variable
  mutate(year = year(ym(m)),
         month = month(ym(m)),
         wave = str_sub(start=3L, year)) |>
  # select & rename
  select(nomem_encr:nohouse_encr,
         wave,
         date = m,
         year, month,
         partner_a = `001`, 
         fin_matters = `002`, 
         hh_position_a = `003`, 
         a_savings = `004`, 
         a_savings_2 = `078`,
         a_insurance = `005`,
         a_investments = `006`,
         a_estate = `007`,
         a_vehicles = `008`,
         a_none = `009`,
         a_loanedout = `010`,
         a_other = `011`,
         a_savings_val = `012`,
         a_savings_cat = `013`,
         a_insurance_val = `014`,
         a_insurance_cat = `015`,
         a_investments_val = `016`,
         a_investments_cat = `017`,
         a_estate_val = `018`,
         a_estate_cat = `019`,
         a_estate_mortgage = `020`,
         a_estate_mortgage_val = `021`,
         a_estate_mortgage_cat = `022`,
         a_vehicles_val = `023`,
         a_vehicles_cat = `024`,
         a_loanedout_val = `025`,
         a_loanedout_cat = `026`,
         a_other_val = `027`,
         a_other_cat = `028`,
         priv_lim_comp = `029`,
         priv_lim_comp_2 = `029`,
         priv_pension = `030`,
         priv_pension_stake = `031`,
         priv_pension_val = `032`,
         priv_pension_cat = `033`,
         priv_lim_comp_stake = `034`,
         priv_lim_comp_val = `035`,
         priv_lim_comp_cat = `036`,
         priv_lim_comp_loan = `037`,
         priv_lim_comp_loan_val = `038`,
         priv_lim_comp_loan_cat = `039`,
         partnership = `040`,
         partnership_2 = `080`,
         partnership_fisc_y = `041`,
         partnership_val = `042`,
         partnership_cat = `043`,
         partnership_val_2 = `083`,
         partnership_cat_2 = `084`,
         outside_partnership = `044`,
         outside_partnership_val = `045`,
         outside_partnership_cat = `046`,
         a_self_empl = `047`,
         a_self_empl_2 = `081`,
         a_self_empl_3 = `085`,
         a_self_empl_fisc_y = `048`,
         a_self_empl_val = `049`,
         a_self_empl_cat = `050`,
         a_self_empl_val_2 = `049`,
         a_self_empl_cat_2 = `050`,
         FOR = `051`,
         FOR_val = `052`,
         FOR_cat = `053`,
         study_debt = `054`,
         study_debt_val = `055`,
         study_debt_cat = `056`,
         # credit
         c_hirepurchase = `057`,
         c_pledge = `058`,
         c_creditcard_overdue = `059`,
         c_creditcard_overdue_2 = `087`,
         c_loan_family = `060`,
         c_other = `061`,
         c_none = `062`,
         c_tot_val = `063`,
         c_tot_cat = `064`,
         payoff_hard_scale = `065`
  ) |>
  # transform double variables
  mutate(a_savings = ifelse(!is.na(a_savings),
                            a_savings,
                            a_savings_2),
         priv_lim_comp = ifelse(!is.na(priv_lim_comp),
                                priv_lim_comp,
                                priv_lim_comp_2),
         partnership = ifelse(!is.na(partnership),
                              partnership,
                              partnership_2),
         a_self_empl = ifelse(!is.na(a_self_empl),
                              a_self_empl,
                              a_self_empl_2),
         a_self_empl_val = ifelse(!is.na(a_self_empl_val),
                                  a_self_empl_val,
                                  a_self_empl_val_2),
         a_self_empl_cat = ifelse(!is.na(a_self_empl_cat),
                                  a_self_empl_cat,
                                  a_self_empl_cat_2),
         partnership_val = ifelse(!is.na(partnership_val),
                                  partnership_val,
                                  partnership_val_2),
         partnership_cat = ifelse(!is.na(partnership_cat),
                                  partnership_cat,
                                  partnership_cat_2),
         c_creditcard_overdue = ifelse(!is.na(c_creditcard_overdue),
                                       c_creditcard_overdue,
                                       c_creditcard_overdue_2)
         
  ) |>
  # delete double variables
  select(-c(a_savings_2,priv_lim_comp_2,partnership_2,a_self_empl_2,
            a_self_empl_val_2,a_self_empl_cat_2,partnership_val_2,
            partnership_cat_2,c_creditcard_overdue_2)) |>
  # arrange
  arrange(nomem_encr)


# write .csv file
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
fwrite(ass, file = "liss_assets.csv")

### END