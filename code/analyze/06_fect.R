### # Heddesheimer, Bryson - Replication code - Month XX, 2024
# vincent.heddesheimer@princeton.edu
#
# Tables reproduced in this file:
#
# Figures reproduced in this file:
#
### ### ### ###

rm(list = ls())

# Load packages
pacman::p_load(tidyverse, data.table, fect)

# Load LISS data
df <- fread("data/liss.csv")

# Remove duplicates -------------------------------------------------------

# create ID & time
df <- df |>
  mutate(
    id = paste0(nomem_encr, wave),
    t = as.integer(wave - 7)
  )

dupl <- df |>
  count(id) |>
  filter(n > 1)

dupldf <- df |>
  filter(id %in% dupl$id)

df <- df |>
  filter(!(id %in% dupl$id) & !is.na(wave)) |>
  # create identifier
  arrange(nomem_encr, wave) |>
  group_by(nomem_encr) |>
  mutate(id = cur_group_id()) |>
  ungroup() |>
  data.frame()

rm(dupl, dupldf)


# compare observations for distance, spread, and partisan
table(is.na(df$distance))
table(is.na(df$spread))
table(is.na(df$partisan_affect))


# check how often people get unemployed more than once in the data
df |>
  filter(unemployment_shock == 1) |>
  count(nomem_encr) |>
  filter(n > 1) |>
  nrow()

inspect <- df |>
  select(nomem_encr, wave, unemployed, unemployment_shock)
inspect

# Unemployment ------------------------------------------------------------

# Create treatment variable
df1 <- df |> mutate(treated = ifelse(unemployed == 1, 1, 0))

# Get treated units
treat <- df1 |>
  filter(treated == 1) |>
  distinct(id) |>
  pull()

# Create DiD variables
df1 <- df1 |>
  group_by(id) |>
  mutate(
    # assign treatment group
    treat = ifelse(id %in% treat, 1, 0),
    # calculate time to treatment
    time_to_treatment = ifelse(treat == 1,
      t - first(na.omit(t[treated == 1])),
      0
    ),
    # calculate period when first treated
    first_treatment_period = ifelse(treat == 1,
      first(na.omit(t[treated == 1])),
      0
    ) # never-treated should be zero or NA
  )

# table: unemployed each year
df1 |>
  janitor::tabyl(t, unemployed) |>
  summarise(
    min = min(`1`),
    max = max(`1`)
  )

# how many treated?
df1 |>
  filter(treat == 1) |>
  distinct(nomem_encr) |>
  nrow()

# As data.frame
df1 <- as.data.frame(df1)

# outcome variables
pvars <- c(
  "partisan_affect", "spread", "distance", "like_max",
  "like_min",
  "red_overall_mean_distance", "red_distance_age_rel_mean",
  "red_distance_education_rel_mean", "red_distance_gender_mean",
  "red_distance_ethnicity_rel_mean", "generalized_trust"
)



# FECT ------------------------------------------------------------
?fect

m_spread <- fect(
  data = df1,
  Y = "spread",
  D = "unemployed",
  index = c("id", "t"),
  force = "two-way",
  se = TRUE,
  parallel = TRUE,
  na.rm = TRUE
)

plot(m_spread)

glimpse(df1)

df1 |>
  select(id, t, unemployed, treated, treat, unemployment_shock)


# unemployed stays 1 forever after first treatment
df1 <- df1 |>
  group_by(id) |>
  arrange(t, .by_group = TRUE) |>
  mutate(unemployed_absorb = as.integer(cummax(unemployed))) |>
  ungroup()

df1 |>
  select(id, t, unemployed, treated, treat, unemployment_shock, unemployed_absorb) |>
  arrange(id, t) |>
  print(n = 200)


m_spread_absorb <- fect(
  data = df1,
  Y = "spread",
  D = "unemployed_absorb",
  index = c("id", "t"),
  force = "two-way",
  se = TRUE,
  parallel = TRUE,
  na.rm = TRUE
)

plot(m_spread_absorb)
