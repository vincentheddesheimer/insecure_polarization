### LISS background variables transformation


rm(list=ls())

# packages
library(data.table)
library(tidyverse)
library(readstata13)
library(furrr)
library(lubridate)


# use multisession evaluation
plan(sequential)

# paths (liss_raw, liss_clean)
here::i_am("code/build/liss/clean/05_LISS_background_transformation.R")
source(here::here("code", "build", "liss", "00_paths.R"))


# create dataframe
backg <- 
  # list all .dta files in directory
  list.files(file.path(liss_raw, "background"), pattern = "\\.dta$", full.names = TRUE) |>
  # read all .dta files in directory
  future_map_dfr(~read.dta13(.)) |>
  # get year and month
  mutate(year = year(ym(wave)),
         month = month(ym(wave)),
         # combine gender variables
         gender = ifelse(!is.na(geslacht),
                         geslacht,
                         gender),
         ) |>
  # select & rename
  select(nomem_encr,
         date = wave,
         year,month,
         gender,
         hh_position = positie,
         birth_year = gebjaar,
         age = leeftijd,
         age_cat = lftdcat,
         age_hh_head = lftdhhh,
         no_hh = aantalhh,
         no_children_hh = aantalki,
         partner,
         civil_status = burgstat,
         domestic_sit = woonvorm,
         dwelling_type = woning,
         place_caharacter = sted,
         occupation = belbezig,
         gross_monthly_income = brutoink_f,
         gross_monthly_income_cat = brutocat,
         net_monthly_income = nettoink_f,
         net_monthly_income_cat = nettocat,
         gross_monthly_income_hh = brutohh_f,
         net_monthly_income_hh = nettohh_f,
         education_wo = oplzon,
         education_with = oplmet,
         education_cat = oplcat,
         hh_partic = doetmee,
         first_wave = werving,
         origin = herkomstgroep
         ) |>
  # arrange
  arrange(nomem_encr)

# write .csv
fwrite(backg, file.path(liss_clean, "liss_background_to26.csv"))

### End