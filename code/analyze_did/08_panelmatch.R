### New DiD analysis: matching on treatment history (PanelMatch 3.1)
# On/off treatment (currently unemployed). Each switch into unemployment is matched to people with
# the same 4-wave history (employed) who are still employed at t; controls weighted by CBPS on 4
# lagged outcomes and baseline covariates. ATT for waves 0..8 after the switch (relative to t-1),
# each lead matched separately (see est_lead), treatment reversal allowed. Controls may lose their
# job after t (PanelMatch's estimand; see ctrl_treated_share). SEs: PanelMatch's analytic
# 'conditional' method (independence across people, not over time); the block bootstrap takes about a
# day for all outcomes and can be switched on with Sys.setenv(DID_PM_BOOT = "500").
# Placebo test: PanelMatch does not allow the outcome in the refinement when placebo.test = TRUE,
# so the placebo version refines on baseline covariates only and reports t-4..t-2.

rm(list = ls())
here::i_am("code/analyze_did/08_panelmatch.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(PanelMatch)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()
n_boot <- as.integer(Sys.getenv("DID_PM_BOOT", unset = "0"))   # 0 = analytic SEs
set.seed(20261007)

# PanelMatch needs integer ids/time and numeric covariates: education and age group as indicators
# waves are numbered consecutively over the waves in which the outcome was fielded (prep_outcome);
# PanelData would otherwise renumber non-consecutive time itself ("may cause undefined behavior")
prep_pm <- function(d, y) {
  d <- prep_outcome(d, y, absorbing = FALSE)
  d <- as.data.frame(d[, c("id", "t", "D", y, baseline_covs), with = FALSE])
  d$id <- as.integer(d$id); d$t <- as.integer(d$t); d$D <- as.integer(d$D)
  ind <- model.matrix(~ agegrp_base + educ_base, model.frame(~ agegrp_base + educ_base, d, na.action = na.pass))[, -1]
  colnames(ind) <- make.names(colnames(ind))
  d <- cbind(d[, setdiff(names(d), c("agegrp_base", "educ_base"))], ind)
  list(d = d, covs = c("male_base", "hhsize_base", "inccat_base", colnames(ind)))
}

# refinement: CBPS weights on the 4 lagged outcomes and the baseline covariates
pm_formula <- function(y, covs) as.formula(paste("~", paste(c(sprintf("I(lag(%s, 1:4))", y), covs), collapse = " + ")))

# PanelMatch keeps a switch only if treatment is observed in every wave up to the lead, so a joint
# lead window 0..8 keeps very few switches (attrition, leaving the labor force). Each lead is
# therefore matched and estimated separately; the number of switches per lead is reported.
match_lead <- function(pd, f, h)
  PanelMatch(panel.data = pd, lag = 4, refinement.method = "CBPS.weight", qoi = "att", lead = h,
             match.missing = TRUE, covs.formula = f, forbid.treatment.reversal = FALSE)

# Matched controls only need to be employed at t, so some of them lose their job by t+h: PanelMatch's
# ATT is the effect of losing the job at t, whatever happens later, against people employed at t
# (Imai, Kim & Wang 2023). ctrl_treated reports the weighted share of matched controls unemployed
# at t+h, i.e. how far this comparison is from one with people still employed at t+h.
ctrl_treated_share <- function(pm, pd, h) {
  ms <- pm[["att"]]; ms <- ms[lengths(ms) > 0]
  D  <- setNames(pd$D, paste(pd$id, pd$t))
  t0 <- as.integer(sub(".*\\.", "", names(ms)))
  mean(vapply(seq_along(ms), function(k) {
    w <- attr(ms[[k]], "weights")[as.character(ms[[k]])]
    dj <- D[paste(ms[[k]], t0[k] + h)]
    sum((w * dj)[!is.na(dj)]) / sum(w[!is.na(dj)])
  }, numeric(1)), na.rm = TRUE)
}

est_lead <- function(pd, f, h) {
  pm <- match_lead(pd, f, h)
  n <- sum(lengths(pm[["att"]]) > 0)
  if (n < 5) return(NULL)
  pe <- summary(PanelEstimate(sets = pm, panel.data = pd, se.method = if (n_boot > 0) "bootstrap" else "conditional",
                              number.iterations = max(n_boot, 1), parallel = n_boot > 0, num.cores = n_cores))
  data.table(term = h, estimate = pe[1, "estimate"], se = pe[1, "std.error"], n_events = n,
             ctrl_treated = ctrl_treated_share(pm, pd, h))
}

run_pm <- function(dt, y) {
  p  <- prep_pm(dt, y)
  pd <- PanelData(p$d, "id", "t", "D", y)
  f_main <- pm_formula(y, p$covs)
  es <- rbindlist(lapply(0:event_window[2], function(h) tryCatch(est_lead(pd, f_main, h), error = function(e) NULL)))
  if (!nrow(es)) stop("no lead with at least 5 matched switches")

  # placebo: refinement on baseline covariates only. It needs complete outcome data over the lag
  # window for every matched person (only feasible for lead 0 here): try lag.in = 4, then 2,
  # otherwise report no pre-period terms.
  f_pl <- as.formula(paste("~", paste(p$covs, collapse = " + ")))
  pl <- tryCatch({
    pm_pl <- PanelMatch(panel.data = pd, lag = 4, refinement.method = "CBPS.weight", qoi = "att",
                        lead = 0, match.missing = TRUE, covs.formula = f_pl,
                        forbid.treatment.reversal = FALSE, placebo.test = TRUE)
    out <- NULL
    for (k in c(4, 2)) {
      out <- tryCatch(placebo_test(pm_pl, pd, lag.in = k, plot = FALSE,
                                   se.method = if (n_boot > 0) "bootstrap" else "unconditional",
                                   number.iterations = max(n_boot, 1), parallel = n_boot > 0, num.cores = n_cores),
                      error = function(e) NULL)
      if (!is.null(out)) break
    }
    out
  }, error = function(e) { message("  placebo test not possible: ", conditionMessage(e)); NULL })
  pl_dt <- if (is.null(pl)) NULL else
    data.table(term = -as.numeric(sub("t-", "", names(pl$estimates))), estimate = pl$estimates,
               se = as.numeric(pl$standard.errors))
  list(es = rbind(es[, .(term, estimate, se)], pl_dt, data.table(term = -1, estimate = 0, se = NA_real_))[order(term)],
       n_events = es[, .(term, n_events, ctrl_treated)], n_treated = es[term == min(term), n_events], n_units = length(unique(p$d$id)))
}

# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) check_panelmatch(prep_pm, pm_formula, match_lead, est_lead)

tidy <- list(); events <- list()
if (do_estimate) {
for (tr in c("jobloss_onoff", "pinc25")) for (sm in c("all", "men", "women")) {
  if (!run_tr(tr)) next
  d <- get_panel(panels, tr, sm)
  for (y in outcomes) {
    key <- paste(tr, sm, y, sep = "|")
    r <- try_est(run_pm(d, y), key)
    if (is.null(r)) next
    # pre-period terms (if any) come from the covariate-only placebo refinement
    tidy[[key]] <- as_tidy(r$es, "PanelMatch", "main", tr, sm, y, r$n_units, r$n_treated)
    events[[key]] <- r$n_events[, `:=`(treatment = tr, sample = sm, outcome = y)]
  }
}

if (!length(tidy)) stop("PanelMatch: no estimates")
save_est(list(tidy = rbindlist(tidy), events = rbindlist(events)), "panelmatch")
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("panelmatch")

x <- res$tidy[sample == "all"]
ggsave(path_fig("panelmatch_jobloss"), plot_es(x, xlab = "Waves relative to job loss"),
       width = 8, height = 2 + 2 * ceiling(uniqueN(x$outcome) / 2))
gen <- res$tidy[sample %in% c("men", "women")][, sample := factor(sample, c("men", "women"), c("Men", "Women"))]
ggsave(path_fig("panelmatch_gender"), plot_es(gen, color = "sample", xlab = "Waves relative to job loss"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(gen$outcome) / 2))

}

### END
