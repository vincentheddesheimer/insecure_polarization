### LISS combine to panel

rm(list=ls())

# packages
pacman::p_load(data.table, tidyverse)

# set wd
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")

# read data sets: all .csv files
file.names <- list.files(pattern = "*.csv") 
file.names <- file.names[! file.names %in% c("liss_combined.csv")]
df_list <- lapply(file.names, fread)

# merge
# politics as base df
df <- df_list[[6]] |>
  # merge with social_ties
  full_join(df_list[[7]] |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_s = date),
            by = c("nomem_encr", "wave")) |>
  # merge with personality
  full_join(df_list[[5]] |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_p = date),
            by = c("nomem_encr", "wave")) |>
  # merge with assets
  full_join(df_list[[1]] |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_a = date),
            by = c("nomem_encr", "wave")) |>
  # merge with housing
  full_join(df_list[[3]] |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_h = date),
            by = c("nomem_encr", "wave")) |>
  # merge with income
  full_join(df_list[[4]] |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_i = date),
            by = c("nomem_encr", "wave")) |>
  mutate(date = na_if(date, "NA01"),
         date = na_if(date, "NA02"),
         date = ifelse(!is.na(date),
                       date,
                       ifelse(!is.na(date_a),
                              date_a,
                              ifelse(!is.na(date_h),
                                     date_h,
                                     date_i)))) |>
  # merge with background 
  left_join(df_list[[2]] |>
              select(-c(year,month)) |>
              mutate(date = as.character(date)),
            by = c("nomem_encr", "date"))

# write as .csv
fwrite(df, "liss_combined.csv")

### END