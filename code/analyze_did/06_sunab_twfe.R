### New DiD analysis: Sun & Abraham and TWFE event studies (fixest)
# Event dummies -5..8 relative to the wave before treatment (-1), endpoints binned (<= -6, >= 9),
# person and wave fixed effects, clustered by person. Sun-Abraham uses the never treated as the
# clean control cohort. Overall effects: Sun-Abraham aggregated over event times 0..8;
# static TWFE (D | id + t).
# Waves are numbered per outcome over the waves in which it was fielded (prep_outcome), so every
# cohort has a reference wave: e.g. a job loss in wave 16 is referenced to wave 14 for the politics
# outcomes, which were not fielded in wave 15. Without this, that cohort's effects are not
# identified relative to -1 (standard errors in the thousands).

rm(list = ls())
here::i_am("code/analyze_did/06_sunab_twfe.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fixest)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()
lo_bin <- event_window[1] - 1L
hi_bin <- event_window[2] + 1L

run_fixest <- function(d, y, type) {
  d <- prep_outcome(d, y)
  d[, G_sa := fifelse(G > 0, G, 10000L)]                # never treated: cohort outside the panel
  d[, rel  := fifelse(G > 0, t - G, NA_integer_)]
  # safeguard: a cohort nobody of which is observed in its reference wave is not identified
  no_ref <- setdiff(d[G > 1, unique(G)], d[rel == -1, unique(G)])
  if (length(no_ref)) {
    message("  dropping cohorts without data in their reference wave: ", paste(no_ref, collapse = ", "))
    d <- d[!G %in% no_ref]
  }
  if (type == "sunab") {
    rhs <- sunab_rhs(d)
  } else {
    d[, rel_b := fifelse(is.na(rel), -1000L, pmin(pmax(rel, lo_bin), hi_bin))]
    rhs <- "i(rel_b, ref = c(-1, -1000))"
  }
  m  <- feols(as.formula(paste(y, "~", rhs, "| id + t")), data = d, cluster = ~id)
  ct <- coeftable(m)
  es <- data.table(term = as.numeric(sub(".*::", "", rownames(ct))), estimate = ct[, 1], se = ct[, 2])
  es <- rbind(es[term >= event_window[1] & term <= event_window[2]],
              data.table(term = -1, estimate = 0, se = NA_real_))[order(term)]
  # overall: Sun-Abraham aggregated over event times 0..8 only (fixest's "ATT" would include the
  # binned >= 9 cells, some of which are barely identified); TWFE: static D | id + t
  ov <- if (type == "sunab") {
    stopifnot(event_window[2] <= 9)                     # single-digit event times in the pattern
    aggregate(m, c(ATT = sprintf("t::[0-%d]:cohort", event_window[2])))["ATT", 1:2]
  } else coeftable(feols(as.formula(paste(y, "~ D | id + t")), data = d, cluster = ~id))["D", 1:2]
  list(es = es, ov = ov, n_units = uniqueN(d$id), n_treated = uniqueN(d[G > 0, id]))
}

# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) check_sunab(run_fixest)

runs <- rbind(runs_absorbing, tibble(treatment = "reemp", sample = "all"))
labels <- c(sunab = "Sun-Abraham", twfe = "TWFE")
tidy <- list(); overall <- list()

if (do_estimate) {
for (i in seq_len(nrow(runs))) {
  tr <- runs$treatment[i]; sm <- runs$sample[i]
  if (!run_tr(tr)) next
  d <- get_panel(panels, tr, sm)
  for (type in names(labels)) for (y in outcomes) {
    key <- paste(type, tr, sm, y, sep = "|")
    r <- try_est(run_fixest(copy(d), y, type), key)
    if (is.null(r)) next
    tidy[[key]] <- as_tidy(r$es, labels[[type]], "main", tr, sm, y, r$n_units, r$n_treated)
    overall[[key]] <- data.table(estimator = labels[[type]], spec = "main", treatment = tr, sample = sm,
                                 outcome = y, att = r$ov[[1]], se = r$ov[[2]])
  }
}

save_est(list(tidy = rbindlist(tidy), overall = rbindlist(overall)), "sunab_twfe")
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("sunab_twfe")

for (tr in unique(runs$treatment)) {
  x <- res$tidy[treatment == tr & sample == "all"]
  if (nrow(x)) ggsave(path_fig(paste0("sunab_twfe_", tr)), plot_es(x, color = "estimator"),
                      width = 9, height = 2.5 + 2 * ceiling(uniqueN(x$outcome) / 2))
}

}

### END
