### New DiD analysis: Callaway-Sant'Anna (did 2.5), main estimator
# Main: never-treated controls, universal base period (t-1), unbalanced panel, doubly robust,
# multiplier bootstrap clustered by person with uniform bands.
# Robustness (job loss, full sample): not-yet-treated controls, balanced panel,
# 1 wave of anticipation, baseline covariates. HonestDiD relative magnitudes on the main estimates.

rm(list = ls())
here::i_am("code/analyze_did/02_cs.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(did, HonestDiD)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()
biters <- if (test_mode) 199 else 999
set.seed(20261007)

cs_specs <- list(
  main     = list(control = "nevertreated",  unbalanced = TRUE,  anticipation = 0, xformla = NULL),
  notyet   = list(control = "notyettreated", unbalanced = TRUE,  anticipation = 0, xformla = NULL),
  balanced = list(control = "nevertreated",  unbalanced = FALSE, anticipation = 0, xformla = NULL),
  antic1   = list(control = "nevertreated",  unbalanced = TRUE,  anticipation = 1, xformla = NULL),
  covars   = list(control = "nevertreated",  unbalanced = TRUE,  anticipation = 0, xformla = xformla_base)
)

run_cs <- function(d, y, s) {
  d <- prep_outcome(d, y)
  if (!s$unbalanced) d <- balance_panel(d)
  d <- as.data.frame(d)
  if (!is.null(s$xformla)) d <- d[complete.cases(d[, baseline_covs]), ]
  att <- att_gt(yname = y, tname = "t", idname = "id", gname = "G", data = d, xformla = s$xformla,
                control_group = s$control, anticipation = s$anticipation, base_period = "universal",
                allow_unbalanced_panel = s$unbalanced, est_method = "dr", clustervars = "id",
                bstrap = TRUE, cband = TRUE, biters = biters, pl = TRUE, cores = n_cores)
  es <- aggte(att, type = "dynamic", min_e = event_window[1], max_e = event_window[2], na.rm = TRUE)
  list(es = es, n_units = att$n, n_treated = length(unique(d$id[d$G > 0])))
}

# HonestDiD relative-magnitude bounds for the average post-treatment effect and for e = 0
honest_cs <- function(es, ref = -1, Mbar = seq(0, 2, by = 0.25)) {
  keep <- es$egt != ref                       # reference period is fixed at 0
  b <- es$att.egt[keep]; e <- es$egt[keep]
  inf <- es$inf.function$dynamic.inf.func.e[, keep, drop = FALSE]
  V <- crossprod(inf) / nrow(inf)^2
  npre <- sum(e < 0); npost <- sum(e >= 0)
  targets <- list(avg_post = rep(1 / npost, npost), e0 = c(1, rep(0, npost - 1)))
  rbindlist(lapply(names(targets), function(nm) {
    l <- targets[[nm]]
    orig <- constructOriginalCS(betahat = b, sigma = V, numPrePeriods = npre, numPostPeriods = npost, l_vec = l)
    rm <- createSensitivityResults_relativeMagnitudes(betahat = b, sigma = V, numPrePeriods = npre,
                                                      numPostPeriods = npost, Mbarvec = Mbar, l_vec = l)
    rbind(as.data.table(orig)[, Mbar := NA_real_], as.data.table(rm), fill = TRUE)[, target := nm]
  }))
}


# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) {
  check_cs_balancing()                # balanced spec relies on balance_panel() (00_setup.R)
  check_cs(run_cs, cs_specs)
}

# Estimation ----------------------------------------------------------------

runs <- rbind(runs_absorbing, tibble(treatment = "reemp", sample = "all"))
tidy <- list(); overall <- list(); honest <- list()

if (do_estimate) {
for (i in seq_len(nrow(runs))) {
  tr <- runs$treatment[i]; sm <- runs$sample[i]
  if (!run_tr(tr)) next
  d <- get_panel(panels, tr, sm)
  specs <- if (tr == "jobloss" && sm == "all") names(cs_specs) else "main"
  for (sp in specs) for (y in outcomes) {
    key <- paste(tr, sm, sp, y, sep = "|")
    r <- try_est(run_cs(d, y, cs_specs[[sp]]), key)
    if (is.null(r)) next
    tidy[[key]] <- as_tidy(data.table(term = r$es$egt, estimate = r$es$att.egt, se = r$es$se.egt,
                                      crit = r$es$crit.val.egt),
                           "CS", sp, tr, sm, y, r$n_units, r$n_treated)
    overall[[key]] <- data.table(estimator = "CS", spec = sp, treatment = tr, sample = sm, outcome = y,
                                 att = r$es$overall.att, se = r$es$overall.se)
    if (tr == "jobloss" && sm == "all" && sp == "main")
      honest[[y]] <- try_est(honest_cs(r$es)[, outcome := y], paste("HonestDiD", y))
  }
}

save_est(list(tidy = rbindlist(tidy), overall = rbindlist(overall), honest = rbindlist(honest)), "cs")
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("cs")
main <- res$tidy[treatment == "jobloss" & sample == "all" & spec == "main"]
ggsave(path_fig("cs_jobloss_main"), plot_es(main, uniform = TRUE, xlab = "Waves relative to job loss"),
       width = 8, height = 2 + 2 * ceiling(uniqueN(main$outcome) / 2))

rob <- res$tidy[treatment == "jobloss" & sample == "all"]
rob[, spec := factor(spec, levels = names(cs_specs),
                     labels = c("Main", "Not-yet-treated", "Balanced panel", "Anticipation 1", "Baseline covariates"))]
ggsave(path_fig("cs_jobloss_robustness"), plot_es(rob, color = "spec", xlab = "Waves relative to job loss"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(rob$outcome) / 2))

gen <- res$tidy[treatment == "jobloss" & sample %in% c("men", "women")]
gen[, sample := factor(sample, c("men", "women"), c("Men", "Women"))]
ggsave(path_fig("cs_jobloss_gender"), plot_es(gen, color = "sample", xlab = "Waves relative to job loss"),
       width = 8, height = 2.5 + 2 * ceiling(uniqueN(gen$outcome) / 2))

for (tr in c("hh_inc", "inc", "reemp")) {
  x <- res$tidy[treatment == tr & spec == "main"]
  if (nrow(x)) ggsave(path_fig(paste0("cs_", tr)), plot_es(x, uniform = TRUE),
                      width = 8, height = 2 + 2 * ceiling(uniqueN(x$outcome) / 2))
}

if (nrow(res$honest)) {
  h <- res$honest[target == "avg_post"]
  h[, outcome_label := factor(outcome_labels[outcome], levels = outcome_labels)]
  p <- ggplot(h[!is.na(Mbar)], aes(Mbar, ymin = lb, ymax = ub)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_linerange(linewidth = 0.8) +
    geom_linerange(data = h[is.na(Mbar)], aes(x = -0.15), color = "grey40", linewidth = 0.8) +
    facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
    theme_hanno() +
    labs(x = expression(bar(M) ~ "(post-period violation relative to largest pre-period violation)"),
         y = "95% CI, average effect waves 0-8", caption = "Grey: original confidence interval.")
  ggsave(path_fig("cs_honestdid"), p, width = 8, height = 2 + 2 * ceiling(uniqueN(h$outcome) / 2))

  # breakdown value: largest Mbar at which the robust CI still excludes zero
  bd <- res$honest[!is.na(Mbar), .(breakdown_Mbar = if (any(lb > 0 | ub < 0)) max(Mbar[lb > 0 | ub < 0]) else NA_real_),
                   by = .(outcome, target)]
  writeLines(knitr::kable(dcast(bd, outcome ~ target, value.var = "breakdown_Mbar"), format = "latex",
                          booktabs = TRUE, digits = 2, linesep = ""), path_tab("cs_honestdid_breakdown"))
}
}

### END
