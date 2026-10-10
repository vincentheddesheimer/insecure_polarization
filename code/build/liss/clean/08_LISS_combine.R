### LISS combine to panel

rm(list=ls())

# packages
pacman::p_load(data.table, tidyverse)

# paths (liss_raw, liss_clean)
here::i_am("code/build/liss/clean/08_LISS_combine.R")
source(here::here("code", "build", "liss", "00_paths.R"))

# read the cleaned module files by name
read_module <- function(module) fread(file.path(liss_clean, paste0("liss_", module, "_to26.csv")))
politics    <- read_module("politics")
social      <- read_module("social_integration_leisure")
personality <- read_module("personality")
assets      <- read_module("assets")
housing     <- read_module("housing")
income      <- read_module("income")
background  <- read_module("background")

# merge
# politics as base df
df <- politics |>
  # merge with social_ties
  full_join(social |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_s = date),
            by = c("nomem_encr", "wave")) |>
  # merge with personality
  full_join(personality |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_p = date),
            by = c("nomem_encr", "wave")) |>
  # merge with assets
  full_join(assets |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_a = date),
            by = c("nomem_encr", "wave")) |>
  # merge with housing
  full_join(housing |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_h = date),
            by = c("nomem_encr", "wave")) |>
  # merge with income
  full_join(income |>
              select(-c(year,month, nohouse_encr)) |>
              rename(date_i = date),
            by = c("nomem_encr", "wave")) |>
  # interview month: politics first, then the other modules
  mutate(date = dplyr::coalesce(date, date_p, date_s, date_i, date_h, date_a)) |>
  # merge with background
  left_join(background |>
              select(-c(year,month)),
            by = c("nomem_encr", "date"))

# write as .csv
fwrite(df, file.path(liss_clean, "liss_combined_to26.csv"))

### END
