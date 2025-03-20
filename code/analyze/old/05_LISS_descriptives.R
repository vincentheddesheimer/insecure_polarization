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
  )

# Function to calculate percent missing
percent_missing <- function(x) {
  mean(is.na(x)) * 100
}

# Function to create summary statistics
create_summary_stats <- function(df) {
  # Get summary stats for each column
  summary_stats <- lapply(df, function(x) {
    if(is.numeric(x)) {
      c(
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        n = sum(!is.na(x)),
        missing = percent_missing(x)
      )
    } else {
      rep(NA, 6)
    }
  })
  
  # Convert to data frame
  stats_df <- do.call(rbind, summary_stats)
  return(stats_df)
}

# Calculate statistics
stats <- create_summary_stats(df2)

# Create LaTeX table
latex_table <- "\\begin{tabular}{lcccccc}\n"
latex_table <- paste0(latex_table, "\\hline\n")
latex_table <- paste0(latex_table, "Variable & Mean & Std. Dev. & Min & Max & N & Missing (\\%) \\\\\n")
latex_table <- paste0(latex_table, "\\hline\n")

# Add rows
for(var in rownames(stats)) {
  latex_table <- paste0(
    latex_table,
    var, " & ",
    sprintf("%.3f", stats[var, "mean"]), " & ",
    sprintf("%.3f", stats[var, "sd"]), " & ",
    sprintf("%.3f", stats[var, "min"]), " & ",
    sprintf("%.3f", stats[var, "max"]), " & ",
    sprintf("%d", stats[var, "n"]), " & ",
    sprintf("%.1f", stats[var, "missing"]), " \\\\\n"
  )
}

latex_table <- paste0(latex_table, "\\hline\n")
latex_table <- paste0(latex_table, "\\end{tabular}\n")

# Write to file
writeLines(latex_table, "/Users/vincentheddesheimer/Princeton Dropbox/Vincent Heddesheimer/insecure_polarization/results/tables/summary_table_manual.tex")


# # Additional question -------------------------------------------------------

# 1) What proportion of people who recover their jobs are women? 
# 2) And what proportion of women who lose their jobs recover them? 

# Filter out all students, retired
df_red <- df |>
  filter(unemployed == 1 | employed == 1)

# Create treatment variable
df_red <- df_red |> 
  group_by(id) |>
  arrange(t) |>
  mutate(unemployed_l1 = lag(unemployed)) |>
  ungroup() |>
  mutate(reemployed = ifelse(employed == 1 & unemployed_l1 == 1, 1, 0))

# Proportion of people who recover their jobs who are women
df_red |>
  filter(reemployed == 1) |>
  distinct(id) |>
  nrow()
# 594 people who recover their jobs

df_red |> 
  filter(reemployed == 1 & male == 1) |>
  distinct(id) |>
  nrow()
# 291 men who recover their jobs

df_red |>
  filter(reemployed == 1 & male == 0) |>
  distinct(id) |>
  nrow()
# 303 women who recover their jobs




# Proportion of women who lose their jobs who recover them

# get women who are ever unemployed
women_unemployed <- df_red |>
  filter(unemployed == 1 & male == 0) |>
  pull(id) |>
  unique()

length(women_unemployed)
# 577 women who are ever unemployed

# How many of them recover their jobs?
df_red |>
  filter(id %in% women_unemployed & reemployed == 1) |>
  distinct(id) |>
  nrow()

# 303 out of 577
# 52% of women who lose their jobs recover them

# Same for men

men_unemployed <- df_red |>
  filter(unemployed == 1 & male == 1) |>
  pull(id) |>
  unique()

length(men_unemployed)  
# 538 men who are ever unemployed
# How many of them recover their jobs?
df_red |>
  filter(id %in% men_unemployed & reemployed == 1) |>
  distinct(id) |>
  nrow()

# 291 out of 538
# 54% of men who lose their jobs recover them

# How many people live in one household

df |>
  summarise(
    mean(no_hh, na.rm = TRUE),
    n = n()
  )
# 2.59 people per household on average

names(df)


# END