### New DiD analysis: counterfactual estimators (fect 2.4)
# Methods: FE, IFE (factors by cross-validation), CFE (person-specific linear trends; the preferred
# check if FE shows pre-trends). Treatment coded absorbing (first job loss on) and on/off (currently
# unemployed); the on/off fits also give the effects of leaving unemployment (reemployment).
# Tests (FE): placebo (last 3 pre-periods), carryover (first 3 waves after exit, on/off),
# leave-one-period-out; equivalence (TOST) and pre-trend F tests come with every fit.
# Bootstrap clustered by person, 200 replications.
#
# fect labels the first treated wave as event time 1 (and the first wave after exit as 1);
# all output here is shifted so that 0 = first treated wave, as in the other estimators.
# Runtime: several hours for all outcomes and methods. Subset with DID_OUTCOMES / DID_FECT_METHODS.

rm(list = ls())
here::i_am("code/analyze_did/03_fect.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fect)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()
nboots <- if (test_mode) 50 else 200

fect_methods <- list(
  fe  = list(method = "fe"),
  ife = list(method = "ife", CV = TRUE, r = 0),      # CV searches r = 0..5
  cfe = list(method = "cfe", Q.type = "linear")      # person-specific linear trends
)
methods_run <- if (Sys.getenv("DID_FECT_METHODS") != "") strsplit(Sys.getenv("DID_FECT_METHODS"), ",")[[1]] else
  if (test_mode) "fe" else names(fect_methods)

run_fect <- function(d, y, m, ...) {
  d <- as.data.frame(d[, c("id", "t", "D", y), with = FALSE])
  do.call(fect, c(list(Y = y, D = "D", data = d, index = c("id", "t"), force = "two-way",
                       se = TRUE, vartype = "bootstrap", nboots = nboots, cl = "id",
                       parallel = TRUE, cores = n_cores, min.T0 = 3, seed = 20261007),
                  fect_methods[[m]], list(...)))
}

# event-time table -> tidy (fect time 1 = first treated wave)
fect_tidy <- function(tab, est_col) {
  tab <- as.data.table(tab, keep.rownames = "time")
  data.table(term = as.numeric(tab$time) - 1, estimate = tab[[est_col]], se = tab$S.E.)
}

# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) check_fect(run_fect, fect_tidy)

tidy <- list(); overall <- list(); tests <- list()
# with DID_FECT_METHODS (e.g. one cluster job per method) the files are tagged with the methods
save_tag <- if (Sys.getenv("DID_FECT_METHODS") != "") paste(methods_run, collapse = "-") else ""
save_all <- function() save_est(list(tidy = rbindlist(tidy), overall = rbindlist(overall),
                                     tests = rbindlist(tests, fill = TRUE)), "fect", save_tag)

record <- function(f, tr, sm, m, y) {
  key <- paste(tr, sm, m, y, sep = "|")
  tidy[[key]] <<- as_tidy(fect_tidy(f$est.att, "ATT"), "fect", m, tr, sm, y, f$N, f$Ntr)
  overall[[key]] <<- data.table(estimator = "fect", spec = m, treatment = tr, sample = sm, outcome = y,
                                att = f$est.avg[1, "ATT.avg"], se = f$est.avg[1, "S.E."], r = f$r.cv)
  # pre-trend tests from the main fit: F test (p < .05 = pre-trends), equivalence F and TOST
  # (p < .05 = pre-trends within the equivalence range)
  tests[[key]] <<- data.table(estimator = "fect", spec = m, treatment = tr, sample = sm, outcome = y,
                              test = "pretrend", att = NA_real_, p = f$test.out$f.p,
                              equiv_p = f$test.out$f.equiv.p, tost_p = max(f$test.out$tost.equiv.p))
  if (!is.null(f$est.att.off)) {                       # exit from unemployment = reemployment
    k2 <- paste("reemp_off", sm, m, y, sep = "|")
    tidy[[k2]] <<- as_tidy(fect_tidy(f$est.att.off, "ATT.OFF"), "fect", m, "reemp_off", sm, y, f$N, f$Ntr)
  }
}
# placebo: average 'effect' in the 3 held-out pre-periods; carryover: average effect in the first
# 3 waves after exit; loo: pre-trend tests with leave-one-period-out estimates
record_test <- function(f, tr, sm, m, y, test) {
  to <- if (test == "loo") f$loo.test.out else f$test.out
  tests[[paste(tr, sm, m, y, test, sep = "|")]] <<- data.table(
    estimator = "fect", spec = m, treatment = tr, sample = sm, outcome = y, test = test,
    att     = switch(test, placebo = f$est.placebo[1, 1], carryover = f$est.carryover[1, 1], NA_real_),
    p       = switch(test, placebo = to$placebo.p, carryover = to$carryover.p, loo = to$f.p),
    equiv_p = switch(test, placebo = to$placebo.equiv.p, carryover = to$carryover.equiv.p, loo = to$f.equiv.p),
    tost_p  = if (test == "loo") max(to$tost.equiv.p) else NA_real_)
}


# Estimation ----------------------------------------------------------------

codings <- c(jobloss = "absorbing", jobloss_onoff = "on/off")

if (do_estimate) {
for (tr in names(codings)) for (sm in c("all", "men", "women")) {
  d <- get_panel(panels, tr, sm)
  ms <- if (sm == "all") methods_run else intersect("fe", methods_run)   # gender split: FE only
  for (m in ms) for (y in outcomes) {
    dy <- prep_outcome(d, y, absorbing = codings[[tr]] == "absorbing")
    f <- try_est(run_fect(dy, y, m), paste("fect", tr, sm, m, y))
    if (is.null(f)) next
    record(f, tr, sm, m, y)
    # FE diagnostics for the full sample
    if (m == "fe" && sm == "all") {
      pl <- try_est(run_fect(dy, y, m, placeboTest = TRUE, placebo.period = c(-2, 0)), "  placebo")
      if (!is.null(pl)) record_test(pl, tr, sm, m, y, "placebo")
      lo <- try_est(run_fect(dy, y, m, loo = TRUE), "  leave-one-out")
      if (!is.null(lo)) record_test(lo, tr, sm, m, y, "loo")
      if (codings[[tr]] == "on/off") {
        co <- try_est(run_fect(dy, y, m, carryoverTest = TRUE, carryover.period = c(1, 3)), "  carryover")
        if (!is.null(co)) record_test(co, tr, sm, m, y, "carryover")
      }
      # fect's own diagnostic plots
      try(ggsave(path_fig(paste0("fect_diag_", tr, "_", y, "_equiv")), plot(f, type = "equiv"), width = 7, height = 4.5), silent = TRUE)
      if (!is.null(pl)) try(ggsave(path_fig(paste0("fect_diag_", tr, "_", y, "_placebo")), plot(pl, placeboTest = TRUE), width = 7, height = 4.5), silent = TRUE)
      if (codings[[tr]] == "on/off") try(ggsave(path_fig(paste0("fect_diag_", tr, "_", y, "_exit")), plot(f, type = "exit"), width = 7, height = 4.5), silent = TRUE)
    }
    save_all()
  }
}

# income drops: FE, absorbing
if ("fe" %in% methods_run) for (tr in c("hh_inc", "inc")) {
  d <- get_panel(panels, tr)
  for (y in outcomes) {
    f <- try_est(run_fect(prep_outcome(d, y), y, "fe"), paste("fect", tr, "fe", y))
    if (!is.null(f)) { record(f, tr, "all", "fe", y); save_all() }
  }
}
save_all()
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("fect")
lab_m <- c(fe = "FE", ife = "IFE", cfe = "CFE")
for (tr in c("jobloss", "jobloss_onoff", "reemp_off")) {
  x <- res$tidy[treatment == tr & sample == "all"][, spec := factor(lab_m[spec], lab_m)]
  if (nrow(x)) ggsave(path_fig(paste0("fect_", tr)),
                      plot_es(x, color = "spec", xlab = if (tr == "reemp_off") "Waves relative to reemployment" else "Waves relative to job loss"),
                      width = 9, height = 2.5 + 2 * ceiling(uniqueN(x$outcome) / 2))
}
gen <- res$tidy[treatment %in% c("jobloss", "jobloss_onoff") & sample %in% c("men", "women")]
gen[, group := paste(fifelse(treatment == "jobloss", "Absorbing", "On/off"), fifelse(sample == "men", "men", "women"), sep = ", ")]
if (nrow(gen)) ggsave(path_fig("fect_gender"), plot_es(gen, color = "group", xlab = "Waves relative to job loss"),
                      width = 9, height = 2.5 + 2 * ceiling(uniqueN(gen$outcome) / 2))
inc <- res$tidy[treatment %in% c("hh_inc", "inc")]
inc[, treatment := factor(treatment, c("hh_inc", "inc"), c("Household income drop", "Personal income drop"))]
if (nrow(inc)) ggsave(path_fig("fect_income"), plot_es(inc, color = "treatment", xlab = "Waves relative to income drop"),
                      width = 9, height = 2.5 + 2 * ceiling(uniqueN(inc$outcome) / 2))

}

### END
