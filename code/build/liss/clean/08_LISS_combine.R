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
  # politics interview month (NA without a politics interview)
  rename(date_pol = date) |>
  # interview month: politics first, then the other modules
  mutate(date = dplyr::coalesce(date_pol, date_p, date_s, date_i, date_h, date_a))
setDT(df)

# Background variables (income, occupation, household) come from one reference month per person-wave,
# so that the treatments (income and job loss) are measured about a year apart in every wave: the
# month of the politics interview; without one (no politics interview, or wave 15, when the module
# was not fielded), the month in which the politics module starts in that wave (December of 1999 + w
# up to wave 17, January of 2000 + w from wave 18). If the person has no background record in that
# month, the nearest record within two months is used; otherwise the background variables are missing.
# (Before Oct 2026 the background came from the first available module month, e.g. May or June
# without a politics interview, which moved the timing of about a quarter of the losses.)
month_index <- function(yyyymm) { x <- as.integer(yyyymm); (x %/% 100L) * 12L + (x %% 100L) - 1L }
index_month <- function(m) as.integer((m %/% 12L) * 100L + m %% 12L + 1L)
ref_month   <- function(w) fifelse(w <= 17L, (1999L + w) * 100L + 12L, (2000L + w) * 100L + 1L)
background <- as.data.table(background)[, `:=`(year = NULL, month = NULL, m_bg = month_index(date))]
df[, m_target := month_index(fifelse(!is.na(date_pol), as.integer(date_pol), ref_month(as.integer(wave))))]
near <- background[, .(nomem_encr, m_bg, m_join = m_bg)][df[, .(nomem_encr, m_join = m_target, row = .I)],
                                                          on = .(nomem_encr, m_join), roll = "nearest"]
df[near$row, m_bg := near$m_bg]
df[abs(m_bg - m_target) > 2L, m_bg := NA_integer_]
message("background matched in the reference month: ", round(100 * df[, mean(m_bg == m_target, na.rm = TRUE) * mean(!is.na(m_bg))], 1),
        "%; within two months: ", round(100 * df[, mean(!is.na(m_bg))], 1), "%")
df <- merge(df, background[, !"date"], by = c("nomem_encr", "m_bg"), all.x = TRUE)
df[, date_bg := index_month(m_bg)][, c("m_bg", "m_target") := NULL]

# write as .csv
fwrite(df, file.path(liss_clean, "liss_combined_to26.csv"))

### END
