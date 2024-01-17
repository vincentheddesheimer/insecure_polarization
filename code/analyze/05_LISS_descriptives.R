### # Heddesheimer, Bryson - Replication code - Month XX, 2023
# vincent.heddesheimer@princeton.edu
#
# 
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, modelsummary, kableExtra)

# Load LISS data
df <- fread("data/liss.csv")
# df <- fread("~/Dropbox (Princeton)/insecure_polarization/Data/orig/liss.csv")

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




# Number of observations per year -----------------------------------------

print(df |> group_by(wave) |> count(), n = 20)
df |> 
  group_by(wave) |> 
  count() |> 
  ungroup() |>
  summarize(mean = mean(n, na.rm = T))



# Summary statistics ------------------------------------------------------

# Rename
df2 <- df |>
  select(
    Unemployed = unemployed,
    `Job lost` = unemployment_shock, 
    `Income decrease (2 levels)` =  income_cat_2p_decrease,
    `Income decrease (any)` = income_cat_decrease,
    `Household income decrease (25pp)` = income_hh_25p_decrease,
    `Partisan affect` = partisan_affect,
    `Spread` = spread,
    `Distance` = distance,
    `Most liked` = like_max,
    `Least liked` = like_min,
    `Peer Group Distance` = red_overall_mean_distance,
    `Peer Group Age Distance` = red_distance_age_rel_mean,
    `Peer Group Education Distance` = red_distance_education_rel_mean,
    `Peer Group Gender Distance` = red_distance_gender_mean,
    `Peer Group Origin Distance` = red_distance_ethnicity_rel_mean,
    `Generalized trust` = generalized_trust,
    `Age (categorical)` = age_cat,
    Male = male,
    `Education (categorial)`= education_cat,
    `Partner` = partner,
    `No. of children in household` = no_children_hh,
    `Net monthly income (t-1)` = l1_net_monthly_income_cat
  ) |>
  mutate_all(as.numeric)


datasummary(
  All(df2) ~ Mean + SD + Min + Max + N + (`NA (\\%)` = PercentMissing),
  df2,
  output = "results/tables/summary_table.tex",
  escape = FALSE,
  fmt = 3)


# END