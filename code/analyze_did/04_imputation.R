### New DiD analysis: imputation estimator (Borusyak, Jaravel & Spiess; didimputation 0.5)
# Untreated outcomes imputed from person and wave fixed effects estimated on untreated
# person-waves. Robustness (job loss, full sample): wave effects by baseline education, gender
# and age group. Horizons 0-8, pre-trend coefficients -5 to -1, clustered by person.

rm(list = ls())
here::i_am("code/analyze_did/04_imputation.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(didimputation, fixest)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()

first_stages <- list(
  main        = ~ 0 | id + t,
  base_x_wave = ~ 0 | id + t^educ_base + t^male_base + t^agegrp_base
)

run_bjs <- function(d, y, fs) {
  d <- prep_outcome(d, y)
  if (!identical(fs, first_stages$main)) d <- d[complete.cases(d[, ..baseline_covs])]
  d <- as.data.frame(d)
  es <- did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id", first_stage = fs,
                       horizon = 0:event_window[2], pretrends = event_window[1]:-1, cluster_var = "id")
  # overall: average over treated person-waves 0-8 waves after treatment
  d$w_overall <- with(d, as.numeric(D == 1 & t - G <= event_window[2]))
  d$w_overall <- d$w_overall / sum(d$w_overall)
  ov <- did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id", first_stage = fs,
                       wtr = "w_overall", cluster_var = "id")
  list(es = as.data.table(es), ov = as.data.table(ov),
       n_units = length(unique(d$id)), n_treated = length(unique(d$id[d$G > 0])))
}

# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) check_bjs(run_bjs, first_stages)

runs <- rbind(runs_absorbing, tibble(treatment = "reemp", sample = "all"))
tidy <- list(); overall <- list()

if (do_estimate) {
for (i in seq_len(nrow(runs))) {
  tr <- runs$treatment[i]; sm <- runs$sample[i]
  d <- get_panel(panels, tr, sm)
  specs <- if (tr == "jobloss" && sm == "all") names(first_stages) else "main"
  for (sp in specs) for (y in outcomes) {
    key <- paste(tr, sm, sp, y, sep = "|")
    r <- try_est(run_bjs(d, y, first_stages[[sp]]), key)
    if (is.null(r)) next
    tidy[[key]] <- as_tidy(r$es[, .(term = as.numeric(term), estimate, se = std.error)],
                           "Imputation (BJS)", sp, tr, sm, y, r$n_units, r$n_treated)
    overall[[key]] <- data.table(estimator = "Imputation (BJS)", spec = sp, treatment = tr, sample = sm,
                                 outcome = y, att = r$ov$estimate[1], se = r$ov$std.error[1])
  }
}

save_est(list(tidy = rbindlist(tidy), overall = rbindlist(overall)), "imputation")
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("imputation")

jl <- res$tidy[treatment == "jobloss" & sample == "all"]
jl[, spec := factor(spec, names(first_stages), c("Person + wave FE", "Baseline covariates x wave"))]
ggsave(path_fig("imputation_jobloss"), plot_es(jl, color = "spec", xlab = "Waves relative to job loss"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(jl$outcome) / 2))

gen <- res$tidy[treatment == "jobloss" & sample %in% c("men", "women")]
gen[, sample := factor(sample, c("men", "women"), c("Men", "Women"))]
ggsave(path_fig("imputation_gender"), plot_es(gen, color = "sample", xlab = "Waves relative to job loss"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(gen$outcome) / 2))

oth <- res$tidy[treatment %in% c("hh_inc", "inc", "reemp")]
oth[, treatment := factor(treatment, c("hh_inc", "inc", "reemp"),
                          c("Household income drop", "Personal income drop", "Reemployment (placebo)"))]
ggsave(path_fig("imputation_other"), plot_es(oth, color = "treatment"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(oth$outcome) / 2))

}

### END
