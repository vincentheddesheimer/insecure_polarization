### LISS Data Transformation Social Integration and Leisure

rm(list=ls())

# packages
pacman::p_load(data.table, tidyverse, readstata13, furrr, lubridate, psych, BBmisc)

# set wd
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/social_integration_leisure/")

# read .dta files  --------------------------------------------------------

# use multisession evaluation
plan(multisession)

social_leisure <- 
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
         wave = str_sub(start=3L, year),
         month = paste0("0",month),
         month = recode(month,
                        `010` = "10",
                        `011` = "11",
                        `012` = "12"),
         # get date
         date = paste0(year,month)
         ) 

# Rename ------------------------------------------------------------------
# select & rename based on the first codebook
social_leisure <- social_leisure |>
  select(
    nomem_encr,
    nohouse_encr,
    wave,
    date,
    year,
    month,
    ### Close contacts ###
    # Name
    close_contact_1_name = `294`,
    close_contact_2_name = `295`,
    close_contact_3_name = `296`,
    close_contact_4_name = `297`,
    close_contact_5_name = `298`,
    # Which are very dear to you
    close_contact_1_dear = `300`,
    close_contact_2_dear = `301`,
    close_contact_3_dear = `302`,
    close_contact_4_dear = `303`,
    close_contact_5_dear = `304`,
    # How close to each other?
    close_contact_1_2_close = `305`,
    close_contact_1_3_close = `306`,
    close_contact_1_4_close = `307`,
    close_contact_1_5_close = `308`,
    close_contact_2_3_close = `309`,
    close_contact_2_4_close = `310`,
    close_contact_2_5_close = `311`,
    close_contact_3_4_close = `312`,
    close_contact_3_5_close = `313`,
    close_contact_4_5_close = `314`,
    # Gender
    close_contact_1_gender = `315`,
    close_contact_2_gender = `316`,
    close_contact_3_gender = `317`,
    close_contact_4_gender = `318`,
    close_contact_5_gender = `319`,
    # Origin
    close_contact_1_origin = `320`,
    close_contact_2_origin = `331`,
    close_contact_3_origin = `342`,
    close_contact_4_origin = `353`,
    close_contact_5_origin = `364`,
    # How do you know person?
    close_contact_1_relation = `321`,
    close_contact_2_relation = `332`,
    close_contact_3_relation = `343`,
    close_contact_4_relation = `354`,
    close_contact_5_relation = `365`,
    # How often do you talk to person?
    close_contact_1_frequency = `323`,
    close_contact_2_frequency = `334`,
    close_contact_3_frequency = `345`,
    close_contact_4_frequency = `356`,
    close_contact_5_frequency = `367`,
    # How often do you discuss political issues?
    close_contact_1_politics = `324`,
    close_contact_2_politics = `335`,
    close_contact_3_politics = `346`,
    close_contact_4_politics = `357`,
    close_contact_5_politics = `368`,
    # How long have you know person?
    close_contact_1_duration = `325`,
    close_contact_2_duration = `336`,
    close_contact_3_duration = `347`,
    close_contact_4_duration = `358`,
    close_contact_5_duration = `369`,
    # Highest education?
    close_contact_1_education = `326`,
    close_contact_2_education = `337`,
    close_contact_3_education = `348`,
    close_contact_4_education = `359`,
    close_contact_5_education = `370`,
    # Work full-time, part-time, not at all?
    close_contact_1_workload = `327`,
    close_contact_2_workload = `338`,
    close_contact_3_workload = `349`,
    close_contact_4_workload = `360`,
    close_contact_5_workload = `371`,
    # Age
    close_contact_1_age = `329`,
    close_contact_2_age = `340`,
    close_contact_3_age = `351`,
    close_contact_4_age = `362`,
    close_contact_5_age = `373`,
    # Home situation
    close_contact_1_homesit = `330`,
    close_contact_2_homesit = `341`,
    close_contact_3_homesit = `352`,
    close_contact_4_homesit = `363`,
    close_contact_5_homesit = `374`,
    # Home situation
    close_contact_1_homesit = `330`,
    close_contact_2_homesit = `341`,
    close_contact_3_homesit = `352`,
    close_contact_4_homesit = `363`,
    close_contact_5_homesit = `374`
  )

# write .csv
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
fwrite(social_leisure, file = "liss_social_integration_leisure.csv")

### END