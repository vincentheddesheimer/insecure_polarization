# run normal fixest

rm(list=ls())



# load packages
pacman::p_load(fixest, data.table, tidyverse)


# load data
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
  arrange(nomem_encr, wave) |>
  group_by(nomem_encr) |>
  mutate(id = cur_group_id()) |>
  ungroup()

rm(dupl, dupldf)


# Create correct id & t ---------------------------------------------------

# remove wave 15: Politics & Values study was not conducted
df <- df |> filter(wave != 15)

df <- df |>
  filter(!is.na(id)) |>
  group_by(wave) |>
  mutate(t = cur_group_id()) |>
  ungroup() |>
  # Create consecutive new unit id
  arrange(id) |>
  mutate(id = dense_rank(id))



# Event study with fixest -------------------------------------------------


pvars <- c(
  "partisan_affect",
  "spread",
  "distance",
  "like_max",
  "like_min"
)



fr_did_adjusted <- feols(
  .[pvars] ~  i(t, unemployed, ref = 2015)+ 
    age + education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat | id + t,
  df,
  cluster = ~id
)
