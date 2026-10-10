### New DiD analysis: statistical power
# Minimum detectable effect (MDE) at 80% power with a two-sided 5% test: (1.96 + 0.84) x SE.
# Reported in outcome units and in standard deviations: the person-wave SD (the usual
# standardized effect) and the within-person SD (the variation a design with person fixed
# effects uses), both among the never unemployed. Also the range of effects each 95% CI leaves
# open. Covers
#   - job loss (absorbing estimators): average over waves 0-8 and single waves, all, men, women;
#   - being unemployed (on/off): fect's ATT over unemployed person-waves (effect while
#     unemployed), and the average of the dCDH / PanelMatch event studies over waves 0-8, whose
#     SE is not saved: bounds assuming independence across waves (lower) and perfect
#     correlation (upper).
# Needs the saved estimates of 02-08. Saves results/estimates/power/power.rds (aggregates).

rm(list = ls())
here::i_am("code/analyze_did/11_power.R")
source(here::here("code", "analyze_did", "00_setup.R"))

z <- qnorm(0.975) + qnorm(0.8)                      # 2.80

if (do_estimate) {
panels <- load_panels()

# outcome SDs among the never unemployed (job-loss panel)
sds <- rbindlist(lapply(names(outcome_labels), function(y) {
  d <- prep_outcome(panels$jobloss, y)[G == 0]
  d[, dev := get(y) - mean(get(y)), by = id]
  data.table(outcome = y, sd_pw = sd(d[[y]]), sd_within = sd(d[, if (.N > 1) .(dev = dev), by = id]$dev),
             mean = mean(d[[y]]))
}))

est <- lapply(c("cs", "fect", "imputation", "sunab_twfe", "synthdid", "dcdh", "panelmatch"), load_est)
overall <- rbindlist(lapply(est, `[[`, "overall"), fill = TRUE)
tidy    <- rbindlist(lapply(est, `[[`, "tidy"), fill = TRUE)

# main specification of each estimator
main <- overall[spec %in% c("main", "fe") & treatment %in% c("jobloss", "jobloss_onoff") & estimator != "TWFE"]
main[, quantity := fifelse(estimator == "dCDH", "dCDH per unit of treatment",
                           fifelse(treatment == "jobloss_onoff", "while unemployed (on/off)", "job loss, waves 0-8"))]
main <- main[, .(estimator, quantity, sample, outcome, estimate = att, se)]

# dCDH and PanelMatch: average over waves 0-8 from the event study, SE bounds
avg_es <- tidy[estimator %in% c("dCDH", "PanelMatch") & term >= 0 & term <= event_window[2],
               .(estimate = mean(estimate), se_indep = sqrt(sum(se^2)) / .N, se_corr = mean(se), n_terms = .N),
               by = .(estimator, sample, outcome)]

# single waves: typical SE of one post-treatment wave
per_wave <- tidy[spec %in% c("main", "fe") & treatment == "jobloss" & term >= 0 & term <= event_window[2],
                 .(se_wave = median(se, na.rm = TRUE)), by = .(estimator, sample, outcome)]

add <- function(x, se_col = "se") {
  x <- merge(x, sds, by = "outcome")
  x[, `:=`(mde = z * get(se_col), ci_lo = estimate - 1.96 * get(se_col), ci_hi = estimate + 1.96 * get(se_col))]
  x[, `:=`(mde_sd = mde / sd_pw, mde_within_sd = mde / sd_within)][]
}
saveRDS(list(sds = sds, main = add(main), avg_es = add(avg_es, "se_indep")[, mde_corr := z * se_corr],
             per_wave = merge(per_wave, sds, by = "outcome")[, `:=`(mde = z * se_wave, mde_sd = z * se_wave / sd_pw)]),
        path_est("power", "power"))
}


# Table ---------------------------------------------------------------------

if (do_figures) {
pacman::p_load(knitr)
r <- readRDS(path_est("power", "power"))
tab <- r$main[sample == "all", .(Estimator = estimator, Quantity = quantity, Outcome = outcome_labels[outcome],
                                 Estimate = sprintf("%.3f", estimate), SE = sprintf("%.3f", se),
                                 `95% CI` = sprintf("[%.2f, %.2f]", ci_lo, ci_hi),
                                 MDE = sprintf("%.3f", mde), `MDE (SD)` = sprintf("%.2f", mde_sd),
                                 `MDE (within SD)` = sprintf("%.2f", mde_within_sd))]
writeLines(kable(tab[order(Outcome, Quantity, Estimator)], format = "latex", booktabs = TRUE, linesep = ""),
           path_tab("power_mde"))
}

### END
