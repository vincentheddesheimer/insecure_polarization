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
    unemployed_broad = ifelse(occupation == 4 | occupation == 5 | occupation == 6 | occupation == 8 | occupation == 11,1,0),
    # job seeker following job loss, exempted from job seeking following job loss
    unemployed = ifelse(occupation == 4 | occupation == 6,1,0),
    student = ifelse(occupation == 7,1,0),
    retired = ifelse(occupation == 9,1,0),
    employed = ifelse(occupation < 4,1,0),
    male = ifelse(gender==1,1,0),
    house_owner = ifelse(dwelling_type == 1,1,0),
    renter = ifelse(dwelling_type ==2 | dwelling_type==3, 1, 0),
    net_monthly_income_cat = ifelse(net_monthly_income_cat == 13 | net_monthly_income_cat == 14, NA, net_monthly_income_cat)
  ) |>
  # Change variables
  group_by(nomem_encr) |>
  arrange(wave) |>
  mutate(
    # treatments
    delta_unemployed = unemployed - lag(unemployed),
    unemployment_shock = ifelse(delta_unemployed == 1, 1, 0), 
    delta_income_cat = net_monthly_income_cat - lag(net_monthly_income_cat),
    income_cat_decrease = ifelse(delta_income_cat < 0, 1, 0),
    income_cat_2p_decrease = ifelse(delta_income_cat < -1, 1, 0),
    delta_income_hh = net_monthly_income_hh - lag(net_monthly_income_hh),
    income_hh_decrease = ifelse(delta_income_hh < 0, 1, 0),
    income_hh_25p_decrease = ifelse(income_hh_decrease == 1 & (abs(delta_income_hh) / lag(net_monthly_income_hh) > .25), 1, 0),
    delta_income = net_monthly_income - lag(net_monthly_income),
    income_decrease = ifelse(delta_income < 0, 1, 0),
    income_25p_decrease = ifelse(income_decrease == 1 & (abs(delta_income) / lag(net_monthly_income) > .25), 1, 0),
    # perceived
    delta_fin_sit_better = fin_sit_better - lag(fin_sit_better),
    fin_sit_better_decrease = ifelse(delta_fin_sit_better < 0, 1, 0),
    delta_easy_live_inc = easy_live_inc - lag(easy_live_inc),
    easy_live_inc_decrease = ifelse(delta_easy_live_inc < 0, 1, 0),
    delta_confr_trouble_ends_meet = confr_trouble_ends_meet - lag(confr_trouble_ends_meet),
    delta_hh_fin_sat = hh_fin_sat - lag(hh_fin_sat),
    hh_fin_sat_decrease = ifelse(delta_hh_fin_sat < 0, 1, 0),
    # outcomes
    delta_partisan_affect = partisan_affect - lag(partisan_affect),
    delta_spread = spread - lag(spread),
    delta_distance = distance - lag(distance),
    delta_generalized_trust = generalized_trust - lag(generalized_trust),
    # controls
    l1_net_monthly_income_cat = lag(net_monthly_income_cat)
  ) |>
  arrange(nomem_encr, wave)

# # inspect
# delta_unemployed <- df |> select(nomem_encr, wave, unemployed, delta_unemployed, unemployment_shock)
# delta_income_cat <- df |> select(nomem_encr, wave, net_monthly_income_cat, delta_income_cat,
#                                  income_cat_decrease, income_cat_2p_decrease)
# delta_income_hh <- df |> select(nomem_encr, wave, net_monthly_income_hh, delta_income_hh,
#                                 income_hh_decrease, income_hh_25p_decrease)
# delta_income <- df |> select(nomem_encr, wave, net_monthly_income, delta_income,
#                              income_decrease, income_25p_decrease)


# Select necessary variables
df6 <- df6 |>
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
         net_monthly_income_hh, delta_income_hh, income_hh_decrease, income_hh_25p_decrease,
         net_monthly_income, delta_income, income_decrease, income_25p_decrease,
         fin_sit_better, delta_fin_sit_better, fin_sit_better_decrease,
         easy_live_inc, delta_easy_live_inc, easy_live_inc_decrease,
         confr_trouble_ends_meet, delta_confr_trouble_ends_meet,
         hh_fin_sat, delta_hh_fin_sat, hh_fin_sat_decrease,
         # controls
         age, age_cat, education_cat, student, retired, employed, no_children_hh, partner,
         l1_net_monthly_income_cat, male
  )

# Write
fwrite(df6, file = "~/Documents/GitHub/insecure_polarization/data/liss.csv")

### END