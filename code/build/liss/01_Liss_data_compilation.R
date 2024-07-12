### LISS data compilation

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, scales)

# Set wd
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")

# Load data
df <- fread("liss_combined.csv")



# Create outgroup-aversion measures ---------------------------------------

df2 <- df %>%
  # Replace 8 with NA for all columns ending with "_politics"
  mutate(across(ends_with("_politics"), ~replace(., . %in% c(8), NA))) %>%
  # Reverse order for all columns ending with "_politics"
  mutate(across(ends_with("_politics"), ~abs(. - 7))) %>%
  # Scale within 0-1
  mutate(across(ends_with("_politics"), ~rescale(., to = c(0, 1)))) %>%
  # Replace -9 and 14 with NA for all columns ending with "_age"
  mutate(across(ends_with("_age"), ~replace(., . %in% c(-9, 14), NA))) %>%
  # New matching categorical age variable for respondent
  mutate(
    age_cat_2 = case_when(
      age < 16 ~ 1,
      age >= 16 & age <= 20 ~ 2,
      age >= 21 & age <= 25 ~ 3,
      age >= 26 & age <= 30 ~ 4,
      age >= 31 & age <= 35 ~ 5,
      age >= 36 & age <= 40 ~ 6,
      age >= 41 & age <= 45 ~ 7,
      age >= 46 & age <= 50 ~ 8,
      age >= 51 & age <= 55 ~ 9,
      age >= 56 & age <= 60 ~ 10,
      age >= 61 & age <= 65 ~ 11,
      age >= 66 & age <= 70 ~ 12,
      TRUE ~ NA_integer_
    )
  ) %>%
  # Recode respondents origin
  mutate(
    origin = case_when(
      origin == 0 ~ 1,
      origin == 101 | origin == 201 ~ 2,
      origin == 102 | origin == 202 ~ 3,
      origin == 999 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  # Recode close contacts' origins
  mutate(across(ends_with("_origin"), 
                ~case_when(
                  . == 1 ~ 1,
                  . %in% 2:7 ~ 3,
                  . == 8 ~ 2,
                  . == 9 ~ NA_integer_,
                  TRUE ~ NA_integer_
                )
  )) %>%
  # Subtract one for close contact education
  mutate(across(ends_with("_education"), ~ . - 1)) %>%
  # Code redundant categories as NAs for close contact
  mutate(across(ends_with("_education"), ~replace(., . %in% c(-10, -9, 0, 7), NA)))

# Calculate distance metrics
metrics <- c("age_abs", "age_rel", "gender", "ethnicity", "ethnicity_rel", "education", "education_rel")
for (metric in metrics) {
  for (i in 1:5) {
    df2 <- df2 %>%
      mutate(!!paste0("distance_", metric, "_", i) := 
               if (metric == "age_abs") abs(age_cat_2 - get(paste0("close_contact_", i, "_age"))) else
                 if (metric == "age_rel") (abs(age_cat_2 - get(paste0("close_contact_", i, "_age"))) / 13) else
                   if (metric == "gender") ifelse(gender == get(paste0("close_contact_", i, "_gender")), 0, 1) else
                     if (metric == "ethnicity") abs(origin - get(paste0("close_contact_", i, "_origin"))) else
                       if (metric == "ethnicity_rel") (abs(origin - get(paste0("close_contact_", i, "_origin"))) / 2) else
                         if (metric == "education") abs(education_cat - get(paste0("close_contact_", i, "_education"))) else
                           (abs(get("education_cat") - get(paste0("close_contact_", i, "_education")))) / 5)
  }
}

# Calculate means across close contacts
df2 <- df2 %>%
  mutate(
    distance_age_abs_mean = rowMeans(select(., starts_with("distance_age_abs_")), na.rm = TRUE),
    distance_age_rel_mean = rowMeans(select(., starts_with("distance_age_rel_")), na.rm = TRUE),
    distance_gender_mean = rowMeans(select(., starts_with("distance_gender_")), na.rm = TRUE),
    distance_ethnicity_mean = rowMeans(select(., starts_with("distance_ethnicity_")), na.rm = TRUE),
    distance_ethnicity_rel_mean = rowMeans(select(., starts_with("distance_ethnicity_rel_")), na.rm = TRUE),
    distance_education_mean = rowMeans(select(., starts_with("distance_education_")), na.rm = TRUE),
    distance_education_rel_mean = rowMeans(select(., starts_with("distance_education_rel_")), na.rm = TRUE)
  )



# Exclude family members and romantic partners ----------------------------

df3 <- df2
for (metric in metrics) {
  for (i in 1:5) {
    df3 <- df3 %>%
      mutate(!!paste0("red_distance_", metric, "_", i) := 
               ifelse(get(paste0("close_contact_", i, "_relation")) %in% 1:5, NA, 
                      get(paste0("distance_", metric, "_", i))))
  }
}

# Calculate means for reduced dataset
df3 <- df3 %>%
  mutate(
    red_distance_age_abs_mean = rowMeans(select(., starts_with("red_distance_age_abs_")), na.rm = TRUE),
    red_distance_age_rel_mean = rowMeans(select(., starts_with("red_distance_age_rel_")), na.rm = TRUE),
    red_distance_gender_mean = rowMeans(select(., starts_with("red_distance_gender_")), na.rm = TRUE),
    red_distance_ethnicity_mean = rowMeans(select(., starts_with("red_distance_ethnicity_")), na.rm = TRUE),
    red_distance_ethnicity_rel_mean = rowMeans(select(., starts_with("red_distance_ethnicity_rel_")), na.rm = TRUE),
    red_distance_education_mean = rowMeans(select(., starts_with("red_distance_education_")), na.rm = TRUE),
    red_distance_education_rel_mean = rowMeans(select(., starts_with("red_distance_education_rel_")), na.rm = TRUE)
  )



# Weight by politics variable ---------------------------------------------

# Weighted distance metrics
for (metric in metrics) {
  for (i in 1:5) {
    df3 <- df3 %>%
      mutate(!!paste0("weighted_distance_", metric, "_", i) := 
               get(paste0("distance_", metric, "_", i)) * get(paste0("close_contact_", i, "_politics")))
  }
}

# Calculate weighted means across close contacts
df4 <- df3 %>%
  mutate(
    weighted_distance_age_abs_mean = rowMeans(select(., starts_with("weighted_distance_age_abs_")), na.rm = TRUE),
    weighted_distance_age_rel_mean = rowMeans(select(., starts_with("weighted_distance_age_rel_")), na.rm = TRUE),
    weighted_distance_gender_mean = rowMeans(select(., starts_with("weighted_distance_gender_")), na.rm = TRUE),
    weighted_distance_ethnicity_mean = rowMeans(select(., starts_with("weighted_distance_ethnicity_")), na.rm = TRUE),
    weighted_distance_ethnicity_rel_mean = rowMeans(select(., starts_with("weighted_distance_ethnicity_rel_")), na.rm = TRUE),
    weighted_distance_education_mean = rowMeans(select(., starts_with("weighted_distance_education_")), na.rm = TRUE),
    weighted_distance_education_rel_mean = rowMeans(select(., starts_with("weighted_distance_education_rel_")), na.rm = TRUE)
  )



# Politics weighting for reduced ------------------------------------------


# Weighted distance metrics for reduced dataset
for (metric in metrics) {
  for (i in 1:5) {
    df4 <- df4 %>%
      mutate(!!paste0("red_weighted_distance_", metric, "_", i) := 
               get(paste0("red_distance_", metric, "_", i)) * get(paste0("close_contact_", i, "_politics")))
  }
}

# Calculate weighted means across close contacts for reduced dataset
df5 <- df4 %>%
  mutate(
    red_weighted_distance_age_abs_mean = rowMeans(select(., starts_with("red_weighted_distance_age_abs_")), na.rm = TRUE),
    red_weighted_distance_age_rel_mean = rowMeans(select(., starts_with("red_weighted_distance_age_rel_")), na.rm = TRUE),
    red_weighted_distance_gender_mean = rowMeans(select(., starts_with("red_weighted_distance_gender_")), na.rm = TRUE),
    red_weighted_distance_ethnicity_mean = rowMeans(select(., starts_with("red_weighted_distance_ethnicity_")), na.rm = TRUE),
    red_weighted_distance_ethnicity_rel_mean = rowMeans(select(., starts_with("red_weighted_distance_ethnicity_rel_")), na.rm = TRUE),
    red_weighted_distance_education_mean = rowMeans(select(., starts_with("red_weighted_distance_education_")), na.rm = TRUE),
    red_weighted_distance_education_rel_mean = rowMeans(select(., starts_with("red_weighted_distance_education_rel_")), na.rm = TRUE)
  )


### Inspect ###
# inspect
df5 |> janitor::tabyl(red_weighted_distance_gender_mean, wave)

# Age
df5 |>
  arrange(nomem_encr, wave) |>
  select(nomem_encr, wave, age_cat_2, matches("_age")) |>
  glimpse()

# Gender
df5 |>
  arrange(nomem_encr, wave) |>
  select(nomem_encr, wave, gender, matches("_gender")) |>
  glimpse()

# Ethnicity 
df5 |>
  arrange(nomem_encr, wave) |>
  select(nomem_encr, wave, origin,  matches("_origin"), matches("_ethnicity_")) |>
  glimpse()




# Function to calculate overall mean for given prefix
calculate_overall_mean <- function(df, prefix = "") {
  df %>%
    mutate(
      !!paste0(prefix, "overall_mean_distance") := rowMeans(
        select(., starts_with(paste0(prefix, "distance_age_rel_")),
               starts_with(paste0(prefix, "distance_gender_")),
               starts_with(paste0(prefix, "distance_ethnicity_rel_")),
               starts_with(paste0(prefix, "distance_education_rel_"))),
        na.rm = TRUE
      )
    )
}

# Apply the function for each type of distance
df6 <- df5 %>%
  calculate_overall_mean() %>%
  calculate_overall_mean("red_") %>%
  calculate_overall_mean("weighted_") %>%
  calculate_overall_mean("red_weighted_")


# inspect
df6 |>
  arrange(nomem_encr, wave) |>
  select(nomem_encr, wave, 
         distance_age_rel_mean, distance_gender_mean, distance_ethnicity_rel_mean, distance_education_rel_mean, overall_mean_distance
         ) |>
  glimpse()

### Worked well!


# Transform variables -----------------------------------------------------

df6 <- df6 |>
  mutate(
    age_sq = age^2,
    # unemployed_broad: job seeker following job loss, first time job seeker, exempted from job seeking following job loss, takes care of the housekeeping
    unemployed_broad = case_when(
      occupation %in% c(4, 5, 6, 8, 11) ~ 1,
      !(occupation %in% c(4, 5, 6, 8, 11)) & !is.na(occupation) ~ 0,
      TRUE ~ NA_real_
    ),
    # job seeker following job loss, exempted from job seeking following job loss
    unemployed = case_when(
      occupation %in% c(4, 6) ~ 1,
      !(occupation %in% c(4, 6)) & !is.na(occupation) ~ 0,
      TRUE ~ NA_real_
    ),
    student = case_when(
      occupation == 7 ~ 1,
      occupation != 7 & !is.na(occupation) ~ 0,
      TRUE ~ NA_real_
    ),
    retired = case_when(
      occupation == 9 ~ 1,
      occupation != 9 & !is.na(occupation) ~ 0,
      TRUE ~ NA_real_
    ),
    employed = case_when(
      occupation %in% c(1, 2, 3, 10) ~ 1,
      !(occupation %in% c(1, 2, 3, 10)) & !is.na(occupation) ~ 0,
      TRUE ~ NA_real_
    ),
    male = ifelse(gender==1,1,0),
    house_owner = case_when(
      dwelling_type == 1 ~ 1,
      dwelling_type != 1 & !is.na(dwelling_type) ~ 0,
      TRUE ~ NA_real_
    ),
    renter = case_when(
      dwelling_type %in% c(2, 3) ~ 1,
      !(dwelling_type %in% c(2, 3)) & !is.na(dwelling_type) ~ 0,
      TRUE ~ NA_real_
    ),
    net_monthly_income_cat = ifelse(net_monthly_income_cat == 13 | net_monthly_income_cat == 14, NA, net_monthly_income_cat),
    # calculate net monthly income + 1 to avoid 0 when calculating percentage changes
    net_monthly_income_cat = net_monthly_income_cat + 1,
  ) |>
  # Change variables
  group_by(nomem_encr) |>
  arrange(wave) |>
  mutate(
    # treatments
    delta_unemployed = unemployed - lag(unemployed),
    unemployment_shock = case_when(
      delta_unemployed == 1 ~ 1,
      delta_unemployed <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    delta_income_cat = net_monthly_income_cat - lag(net_monthly_income_cat),
    income_cat_decrease = case_when(
      delta_income_cat < 0 ~ 1,
      delta_income_cat >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    income_cat_2p_decrease = case_when(
      delta_income_cat < -1 ~ 1,
      delta_income_cat >= -1 ~ 0,
      TRUE ~ NA_real_
    ),
    # relative income cat decrease
    delta_income_cat_rel = delta_income_cat / lag(net_monthly_income_cat),
    income_cat_rel_decrease_10p = case_when(
      delta_income_cat_rel < -0.1 ~ 1,
      delta_income_cat_rel >= -0.1 ~ 0,
      TRUE ~ NA_real_
    ),
    income_cat_rel_decrease_20p = case_when(
      delta_income_cat_rel < -0.2 ~ 1,
      delta_income_cat_rel >= -0.2 ~ 0,
      TRUE ~ NA_real_
    ),
    income_cat_rel_decrease_50p = case_when(
      delta_income_cat_rel < -0.5 ~ 1,
      delta_income_cat_rel >= -0.5 ~ 0,
      TRUE ~ NA_real_
    ),
    # above mean: > 0.3
    income_cat_rel_decrease_mean = case_when(
      delta_income_cat_rel < -0.3 ~ 1,
      delta_income_cat_rel >= -0.3 ~ 0,
      TRUE ~ NA_real_
    ),
    # above median: > 0.24
    income_cat_rel_decrease_median = case_when(
      delta_income_cat_rel < -0.25 ~ 1,
      delta_income_cat_rel >= -0.25 ~ 0,
      TRUE ~ NA_real_
    ),
    delta_income_hh = net_monthly_income_hh - lag(net_monthly_income_hh),
    delta_income_hh_rel = delta_income_hh / lag(net_monthly_income_hh),
    income_hh_decrease = case_when(
      delta_income_hh < 0 ~ 1,
      delta_income_hh >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    income_hh_25p_decrease = case_when(
      income_hh_decrease == 1 & (abs(delta_income_hh) / lag(net_monthly_income_hh) > .25) ~ 1,
      income_hh_decrease == 1 & (abs(delta_income_hh) / lag(net_monthly_income_hh) <= .25) ~ 0,
      TRUE ~ NA_real_
    ),
    # above mean: > .2
    income_hh_rel_decrease_mean = case_when(
      delta_income_hh_rel < -0.2 ~ 1,
      delta_income_hh_rel >= -0.2 ~ 0,
      TRUE ~ NA_real_
    ),
    # above median: > .1
    income_hh_rel_decrease_median = case_when(
      delta_income_hh_rel < -0.1 ~ 1,
      delta_income_hh_rel >= -0.1 ~ 0,
      TRUE ~ NA_real_
    ),
    delta_income = net_monthly_income - lag(net_monthly_income),
    income_decrease = case_when(
      delta_income < 0 ~ 1,
      delta_income >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    income_10p_decrease = case_when(
      income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) > .10) ~ 1,
      income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) <= .10) ~ 0,
      TRUE ~ NA_real_
    ),
    income_25p_decrease = case_when(
      income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) > .25) ~ 1,
      income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) <= .25) ~ 0,
      TRUE ~ NA_real_
    ),
    income_50p_decrease = case_when(
      income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) > .50) ~ 1,
      income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) <= .50) ~ 0,
      TRUE ~ NA_real_
    ),
    # perceived
    delta_fin_sit_better = fin_sit_better - lag(fin_sit_better),
    fin_sit_better_decrease = case_when(
      delta_fin_sit_better < 0 ~ 1,
      delta_fin_sit_better >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    delta_easy_live_inc = easy_live_inc - lag(easy_live_inc),
    easy_live_inc_decrease = case_when(
      delta_easy_live_inc < 0 ~ 1,
      delta_easy_live_inc >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    delta_confr_trouble_ends_meet = confr_trouble_ends_meet - lag(confr_trouble_ends_meet),
    delta_hh_fin_sat = hh_fin_sat - lag(hh_fin_sat),
    hh_fin_sat_decrease = case_when(
      delta_hh_fin_sat < 0 ~ 1,
      delta_hh_fin_sat >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    # outcomes
    delta_partisan_affect = partisan_affect - lag(partisan_affect),
    delta_spread = spread - lag(spread),
    delta_distance = distance - lag(distance),
    delta_generalized_trust = generalized_trust - lag(generalized_trust),
    # controls
    l1_net_monthly_income_cat = lag(net_monthly_income_cat)
  ) |>
  ungroup() |>
  arrange(nomem_encr, wave) 

# # inspect
delta_unemployed <- df6 |> select(nomem_encr, wave, unemployed, delta_unemployed, unemployment_shock)
delta_income_cat <- df6 |> select(nomem_encr, wave, net_monthly_income_cat, delta_income_cat,
                                 income_cat_decrease, income_cat_2p_decrease)
delta_income_cat_rel <- df6 |> 
  group_by(nomem_encr) |>
  arrange(nomem_encr, wave) |>
  mutate(net_monthly_income_cat_l1 = lag(net_monthly_income_cat)) |>
  ungroup() |>
  select(
    nomem_encr,
    wave,
    net_monthly_income_cat,
    delta_income_cat,
    net_monthly_income_cat_l1,
    delta_income_cat_rel,
    net_monthly_income_cat_l1,
    income_cat_rel_decrease_10p,
    income_cat_rel_decrease_50p
)

# delta_income_hh <- df6 |> select(nomem_encr, wave, net_monthly_income_hh, delta_income_hh,
#                                 income_hh_decrease, income_hh_25p_decrease)
# delta_income <- df6 |> select(nomem_encr, wave, net_monthly_income, delta_income,
#                              income_decrease, income_25p_decrease)
table(is.na(df6$net_monthly_income))
table(is.na(df6$net_monthly_income_cat))

glimpse(df6)

# Select necessary variables
df7 <- df6 |>
  select(nomem_encr, wave,
         # outcomes
         partisan_affect, delta_partisan_affect, spread, delta_spread, distance, like_max, like_min,
         delta_distance, generalized_trust, delta_generalized_trust,
         little_concern_for_others, interested_in_others, comfy_around_people, insult_people,
         sympathisze_w_others_feelings, not_interested_in_others_problems, talk_to_different_people_parties,
         not_interested_in_others, feel_others_emotions, quiet_around_strangers,
         importance_open_minded, feel_connected_to_others, 
         out_group_risk_aversion_raw, out_group_risk_aversion,
         
         distance_age_abs_mean:distance_education_rel_mean,
         red_distance_age_abs_mean:red_distance_education_rel_mean,
         weighted_distance_age_abs_mean:weighted_distance_education_rel_mean,
         red_weighted_distance_age_abs_mean:red_weighted_overall_mean_distance,
         
         # treatments
         unemployed, delta_unemployed, unemployment_shock,
         net_monthly_income_cat, delta_income_cat, income_cat_decrease, income_cat_2p_decrease,
         net_monthly_income_hh, delta_income_hh, income_hh_decrease, 
            income_hh_25p_decrease, income_hh_rel_decrease_mean, income_hh_rel_decrease_median,
         net_monthly_income, delta_income, 
            income_decrease, income_10p_decrease, income_25p_decrease, income_50p_decrease,
         delta_income_cat_rel, income_cat_rel_decrease_10p, income_cat_rel_decrease_20p,
            income_cat_rel_decrease_50p, income_cat_rel_decrease_mean, income_cat_rel_decrease_median,
         
         fin_sit_better, delta_fin_sit_better, fin_sit_better_decrease,
         easy_live_inc, delta_easy_live_inc, easy_live_inc_decrease,
         confr_trouble_ends_meet, delta_confr_trouble_ends_meet,
         hh_fin_sat, delta_hh_fin_sat, hh_fin_sat_decrease,
         # controls
         age, age_cat, education_cat, student, retired, employed, no_children_hh, partner,
         l1_net_monthly_income_cat, male, house_owner, occupation
  )

# Write
fwrite(df7, file = "~/Documents/GitHub/insecure_polarization/data/liss.csv")




# Inspect  ----------------------------------------------------------------

pacman::p_load(dlookr)
# Inspect income: mean & median
df6 |> ungroup() |>
  diagnose_numeric(delta_income_cat,  delta_income_cat_rel, delta_income_hh, delta_income)

# among those for which deltas are negative
df6 |> ungroup() |>
  filter(income_cat_decrease == 1) |>
  diagnose_numeric(delta_income_cat,  delta_income_cat_rel)
# delta_income_cat: mean = -1.5, median = -1
# delta_income_cat_rel: mean = -0.3, median = -0.25

# among those for which deltas are negative
df6 |> ungroup() |>
  filter(income_hh_decrease == 1) |>
  diagnose_numeric(delta_income_hh_rel)
# delta_income_hh: mean = -0.19, median = -0.11



### END