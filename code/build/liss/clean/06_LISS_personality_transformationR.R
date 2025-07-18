### LISS Data Transformation Personality

rm(list=ls())

# packages
pacman::p_load(data.table, tidyverse, readstata13, furrr, lubridate, psych, BBmisc)

# set wd
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/personality/")


# read .dta files  --------------------------------------------------------

# use multisession evaluation
plan(multisession)

pers <- 
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
         date = paste0(year,month),
         # get wave
         wave = recode(date, 
                       `200805` = "08",
                       `200808` = "08",
                       `200905` = "09",
                       `200906` = "09",
                       `201005` = "10",
                       `201006` = "10",
                       `201105` = "11",
                       `201106` = "11",
                       `201205` = "12",
                       `201206` = "12",
                       `201305` = "13",
                       `201306` = "13",
                       `201411` = "14",
                       `201412` = "14",
                       `201511` = "15",
                       `201512` = "15",
                       `201705` = "17",
                       `201706` = "17",
                       `201805` = "18",
                       `201806` = "18",
                       `201905` = "19",
                       `201906` = "19",
                       `202005` = "20",
                       `202006` = "20",
                       `202105` = "21",
                       `202106` = "21",
                       `202205` = "22",
                       `202206` = "22",
                       `202305` = "23",
                       `202306` = "23")) |>
  # Rename ------------------------------------------------------------------
# select & rename
select(nomem_encr,
       nohouse_encr,
       wave,
       date,
       year,
       month,
       # 0 can't be too careful - 10 most people can be trusted
       generalized_trust = `019`,
       # 1 very inaccurate - 5 very accurate
       little_concern_for_others = `021`, 
       interested_in_others = `026`,
       comfy_around_people = `030`,
       insult_people = `031`,
       sympathisze_w_others_feelings = `036`,
       not_interested_in_others_problems = `041`,
       talk_to_different_people_parties = `050`,
       not_interested_in_others = `051`,
       feel_others_emotions = `061`,
       quiet_around_strangers = `065`,
       # 1 extremely unimportant - 7 extremely important
       importance_open_minded = `103`,
       # 1 not connected (circle) - 7 very connected (circle)
       feel_connected_to_others = `135`
       ) |>
  mutate(
    generalized_trust = ifelse(generalized_trust == -9 | generalized_trust == 999,
                               NA,
                               as.numeric(generalized_trust))
  )




# Principal Component Analysis --------------------------------------------

# Construct out group risk aversion variable

# Loop through PCA per year
wave_list <- pers |> distinct(wave) |> pull()

df <- pers |>
  select(-c(nohouse_encr, date:month)) |>
  mutate(across(generalized_trust:feel_connected_to_others, as.numeric))

var_list1 <- colnames(df)

pca <- list()

for(i in 1:length(wave_list)){
  d <- df |> filter(wave %in% wave_list[i])
  
  nomem_encr <- as.character(d$nomem_encr)
  
  d <- d |> select(-c(nomem_encr, wave))
  
  pca_out <- principal(d, nfactors = 1, scores=T, missing=T)
  
  pca[[i]] <- data.table(cbind(nomem_encr, pca_out$scores, wave = wave_list[i]))
}

pca <- do.call("rbind", pca) |>
  mutate(PC1 = as.numeric(PC1),
         # Normalize 
         PC1_norm = normalize(PC1, method = "range", range = c(0, 1)))

# Merge onto pers dataframe
pers <- pers |>
  left_join(pca, by = c("nomem_encr", "wave")) |>
  rename(out_group_risk_aversion_raw = "PC1",
         out_group_risk_aversion = "PC1_norm")

# # Inspect
# inspect <- pers |> select(nomem_encr, wave, generalized_trust:feel_connected_to_others, out_group_risk_aversion)

### Interpretation: 
# Higher values denote lower risk aversion towards out group
# Lower values denote higher risk aversion towards out group



# write .csv
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
fwrite(pers, file = "liss_personality.csv")


### END