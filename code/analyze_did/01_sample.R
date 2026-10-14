### New DiD analysis: build the analysis panels
# Labor-force sample (SPEC.md): ages 18-65; before treatment, and throughout for the comparison
# group, only waves in which the person is employed; after treatment all waves are kept.
# Writes data/analysis/panels.rds and results/tables/sample_sizes.tex.

rm(list = ls())
here::i_am("code/analyze_did/01_sample.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(knitr)

liss <- fread(here("data", "liss_to26.csv"))
liss <- liss[!duplicated(liss[, .(nomem_encr, wave)])]
liss[, `:=`(id = nomem_encr, t = as.integer(wave - 7))]
setorder(liss, id, t)

# employment status from LISS belbezig: 1-3 employed, 4/6 job seeker or exempted after job loss
liss[, `:=`(emp = occupation %in% emp_codes, unemp = occupation %in% unemp_codes)]
liss <- liss[age >= 18 & age <= 65]

keep_cols <- c("id", "t", "wave", "nohouse_encr", "birth_year", names(outcome_labels), names(mechanism_labels), "occupation", "emp", "unemp", "age", "male",
               "education_cat", "no_hh", "net_monthly_income_cat",
               "income_hh_rel_decrease_median", "income_cat_rel_decrease_median", "net_monthly_income")
liss <- liss[, ..keep_cols]
# household: the person's most frequent household id (for standard errors clustered by household)
liss[, hh := { h <- nohouse_encr[!is.na(nohouse_encr)]; if (length(h)) as.integer(names(which.max(table(h)))) else id }, by = id]


# Helpers -------------------------------------------------------------------

# baseline covariates, measured the same way for everyone: in the person's first wave in the panel
# (before any loss, since a loss must follow an employed wave in the panel). Measuring them in the
# last wave before the loss for the treated but in the first wave for the never treated, as before
# Oct 2026, compared people at different ages and points of their careers.
add_baseline <- function(d) {
  d[, base_t := min(t), by = id]
  b <- d[t == base_t, .(id,
    agegrp_base = cut(age, c(17, 29, 44, 54, 65), labels = c("18-29", "30-44", "45-54", "55-65")),
    male_base   = male,          # gender "other" is coded 0 in the build (as in the paper)
    educ_base   = factor(education_cat),
    hhsize_base = pmin(no_hh, 5),
    inccat_base = net_monthly_income_cat)]
  d[b, on = "id", (baseline_covs) := mget(paste0("i.", baseline_covs))]
  d[, base_t := NULL][]
}

# absorbing treatment: G = first wave with the event, which must follow an employed wave
# lf_restrict = FALSE keeps every wave of everyone (with emp), for the within-person check in
# 14_apsr_note.R, which applies one employment rule to treated and never treated alike
make_absorbing <- function(d, event, lf_restrict = TRUE) {
  d <- copy(d)
  d[, ev := get(event) %in% 1]
  d[, G := if (any(ev)) t[which(ev)[1]] else 0L, by = id]
  # employed in the directly preceding wave (not just the previous observed row)
  d[, prev_emp := fifelse(t - shift(t) == 1L, shift(emp), NA), by = id]
  bad <- d[G > 0 & t == G & !(prev_emp %in% TRUE), unique(id)]   # left-censored or no job before
  d <- d[!id %in% bad]
  if (lf_restrict) d <- d[(G > 0 & t >= G) | emp]                 # labor-force restriction
  d[, D := as.integer(G > 0 & t >= G)]
  d[, c("ev", "prev_emp") := NULL]
  add_baseline(d)
}


# Panels --------------------------------------------------------------------

# 1. job loss, absorbing (main)
jobloss <- make_absorbing(liss[, ev_jobloss := as.integer(unemp)], "ev_jobloss")

# 2. job loss, on/off: labor-force waves only (employed or unemployed), D = currently unemployed;
#    people unemployed in their first labor-force wave are dropped (first job loss unknown)
jobloss_onoff <- liss[emp | unemp]
jobloss_onoff <- jobloss_onoff[!id %in% jobloss_onoff[, .(first_unemp = unemp[1]), by = id][first_unemp == TRUE, id]]
jobloss_onoff[, G := 0L]
jobloss_onoff <- add_baseline(jobloss_onoff)
jobloss_onoff[, D := as.integer(unemp)]
jobloss_onoff[, G := if (any(D == 1)) t[which(D == 1)[1]] else 0L, by = id]   # first job loss, for reference

# 3./4. income drops above the median (household >= 10%, personal >= 25%), absorbing
hh_inc <- make_absorbing(liss, "income_hh_rel_decrease_median")
inc    <- make_absorbing(liss, "income_cat_rel_decrease_median")

# 6. large personal income loss: net personal income (background variables, euros) at least 25%
#    below the directly preceding wave, absorbing (12_alt_treatments.R compares candidate treatments)
liss[, inc_l := fifelse(t - shift(t) == 1, shift(net_monthly_income), NA_real_), by = id]
liss[, ev_pinc25 := as.integer(inc_l > 0 & net_monthly_income <= 0.75 * inc_l)]
pinc25 <- make_absorbing(liss, "ev_pinc25")
pinc25_all  <- make_absorbing(liss, "ev_pinc25", lf_restrict = FALSE)
jobloss_all <- make_absorbing(liss[, ev_jobloss := as.integer(unemp)], "ev_jobloss", lf_restrict = FALSE)

# 5. reemployment placebo as in the paper: first wave employed after an unemployed wave,
#    within the labor-force waves (comparison group: never reemployed)
reemp <- copy(jobloss_onoff)[, c("D", "G") := NULL]
reemp[, ev := emp & shift(unemp) %in% TRUE, by = id]
reemp[, G := if (any(ev)) t[which(ev)[1]] else 0L, by = id]
reemp[, D := as.integer(G > 0 & t >= G)][, ev := NULL]

panels <- list(jobloss = jobloss, jobloss_onoff = jobloss_onoff, hh_inc = hh_inc, inc = inc, reemp = reemp, pinc25 = pinc25,
               pinc25_all = pinc25_all, jobloss_all = jobloss_all)
dir.create(dirname(path_panels), recursive = TRUE, showWarnings = FALSE)
saveRDS(panels, path_panels)


# Sample sizes ----------------------------------------------------------------

sizes <- rbindlist(lapply(grep("_all$", names(panels), value = TRUE, invert = TRUE), function(p) {
  d <- panels[[p]]
  data.table(Panel = p, Persons = uniqueN(d$id), `Person-waves` = nrow(d),
             `Treated persons` = uniqueN(d[G > 0, id]), Cohorts = uniqueN(d[G > 0, G]),
             `Treated, men` = uniqueN(d[G > 0 & male_base == 1, id]),
             `Treated, women` = uniqueN(d[G > 0 & male_base == 0, id]))
}))
print(sizes)
writeLines(kable(sizes, format = "latex", booktabs = TRUE, format.args = list(big.mark = ",")),
           path_tab("sample_sizes"))

### END
