### LISS housing transformation


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
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/housing/")

# read .dta files 
house <- 
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
         dwelling_sat = `001`,
         dwelling_sat_2 = `091`,
         vicinity_sat = `002`,
         vicinity_sat_2 = `092`,
         housing_sit = `003`,
         housing_sit_other = `004`,
         rent_bin = `005`,
         rent_time = `006`,
         rent_time_2 = `093`,
         rent_time_other = `007`,
         rent_cost = `008`,
         rent_add_inclu = `009`,
         rent_tot_add = `010`,
         rent_benef_rec = `011`,
         rent_benef_rec_2 = `086`,
         rent_benef_rec_3 = `088`,
         rent_benef_incl_in_tot = `087`,
         rent_benef_rec_time = `012`,
         rent_benef_rec_amount = `013`,
         purchase_year = `014`,
         mortgage_bin = `015`,
         bridging_bin = `016`,
         mortgage_remain = `017`,
         mortgage_pay = `018`,
         mortgage_pay_amount_2007 = `019`,
         mortgage_pay_amount = `083`,
         mortgage_interest = `020`,
         mortgage_interest_amount = `021`,
         mortgage_lifeinsurance = `022`,
         mortgage_lifeinsurance_amount = `023`,
         property_value = `024`,
         property_purchase_price = `025`,
         property_purchase_currency = `026`,
         property_purchase_price_buycost = `027`,
         property_value_appraisal = `028`,
         property_value_appraisal_year = `029`,
         association_costs_bin = `030`,
         association_period = `031`,
         association_period_2 = `094`,
         association_period_other = `032`,
         association_costs = `033`,
         rooms_no = `034`,
         health_adjustments = `035`,
         residence_year = `036`,
         municipality_year = `037`,
         dwelling_type_h = `038`,
         floors_no = `039`,
         steps_no = `040`,
         probl_toosmall = `041`,
         probl_toolarge = `089`,
         probl_toodark = `042`,
         probl_heating = `043`,
         probl_leak = `044`,
         probl_damp = `045`,
         probl_rot = `046`,
         probl_noisy = `047`,
         probl_none = `048`,
         noise_neighb = `049`,
         noise_fac_traf = `050`,
         noise_airtraf = `090`,
         dirt_fac_traf = `051`,
         vandal_crime = `052`,
         envir_none = `053`,
         rent_out = `054`,
         residence_elsew = `055`,
         residence_where = `056`,
         residence_cntr = `057`,
         scnd_dwel = `058`,
         scnd_dwel_sit = `060`,
         scnd_dwel_mortg = `061`,
         scnd_dwel_mortg_remain = `062`,
         scnd_dwel_value = `063`,
         scnd_dwel_rent_time = `064`,
         scnd_dwel_rent_time_2 = `095`,
         scnd_dwel_rent_time_other = `065`,
         scnd_dwel_rent_cost = `066`,
         scnd_dwel_rent_add_incl = `067`,
         scnd_dwel_rent_add_incl_2 = `084`,
         scnd_dwel_rent_tot_add = `068`,
         scnd_dwel_rent_tot_add_2 = `085`,
         scnd_dwel_assoc_costs_bin = `069`,
         scnd_dwel_assoc_period = `070`,
         scnd_dwel_assoc_period_2 = `096`,
         scnd_dwel_assoc_period_other = `071`,
         scnd_dwel_serv_cost = `072`
         ) |>
  # transform double variables
  mutate(dwelling_sat = ifelse(!is.na(dwelling_sat),
                               dwelling_sat,
                               dwelling_sat_2),
         vicinity_sat = ifelse(!is.na(vicinity_sat),
                               vicinity_sat,
                               vicinity_sat_2),
         rent_time = ifelse(!is.na(dwelling_sat),
                            rent_time,
                            rent_time_2),
         rent_benef_rec = ifelse(!is.na(rent_benef_rec),
                                 rent_benef_rec,
                                 ifelse(!is.na(rent_benef_rec_2),
                                        rent_benef_rec_2,
                                        rent_benef_rec_3)),
         mortgage_pay_amount = ifelse(!is.na(mortgage_pay_amount_2007),
                                      mortgage_pay_amount_2007,
                                      mortgage_pay_amount),
         association_period = ifelse(!is.na(association_period),
                                     association_period,
                                     association_period_2),
         scnd_dwel_rent_time = ifelse(!is.na(scnd_dwel_rent_time),
                                      scnd_dwel_rent_time,
                                      scnd_dwel_rent_time_2),
         scnd_dwel_rent_add_incl = ifelse(!is.na(scnd_dwel_rent_add_incl),
                                          scnd_dwel_rent_add_incl,
                                          scnd_dwel_rent_add_incl_2),
         scnd_dwel_rent_tot_add = ifelse(!is.na(scnd_dwel_rent_tot_add),
                                         scnd_dwel_rent_tot_add,
                                         scnd_dwel_rent_tot_add_2),
         scnd_dwel_assoc_period = ifelse(!is.na(scnd_dwel_assoc_period),
                                         scnd_dwel_assoc_period,
                                         scnd_dwel_assoc_period_2)
  ) |>
  # delete double variables
  select(-c(dwelling_sat_2,vicinity_sat_2, rent_time_2, rent_benef_rec_2,rent_benef_rec_3,
            mortgage_pay_amount_2007,association_period_2,
            scnd_dwel_rent_time_2,scnd_dwel_rent_add_incl_2,
            scnd_dwel_rent_tot_add_2,scnd_dwel_assoc_period_2)) |>
  # arrange
  arrange(nomem_encr)


# write .csv file
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
fwrite(house, file = "liss_housing.csv")

### End