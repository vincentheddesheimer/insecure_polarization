### New DiD analysis: de Chaisemartin & D'Haultfoeuille (DIDmultiplegtDYN 2.4)
# On/off treatment (currently unemployed); effects of the first switch into unemployment whatever
# happens afterwards (not normalized). Comparison: units not yet switched (package default).
# Same switchers at every horizon (effects and placebos).
#
# Mapping to the common event time (0 = first treated wave, -1 = reference):
#   Effect_l -> l - 1,  Placebo_l -> -1 - l.  9 effects and 4 placebos cover the window -5..8.
# DIDmultiplegtDYN 2.4 computes with polars, a suggested (not imported) dependency that has to be
# attached; without it the estimation fails with "object 'pl' not found". The loader below fails
# early with a clear message, and check_dcdh() (checks.R) verifies on simulated data with known
# effects that run_dcdh() recovers them before anything is estimated.

rm(list = ls())
here::i_am("code/analyze_did/05_dcdh.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(DIDmultiplegtDYN)
if (!requireNamespace("polars", quietly = TRUE))
  stop("DIDmultiplegtDYN needs the 'polars' package (from r-universe; pinned in renv.lock): run renv::restore()")
library(polars)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()
n_eff <- event_window[2] + 1
n_pl  <- -event_window[1] - 1

run_dcdh <- function(d, y) {
  d <- prep_outcome(d, y, absorbing = FALSE)
  d <- data.frame(id = d$id, t = d$t, D = d$D, y = d[[y]])
  r <- did_multiplegt_dyn(df = d, outcome = "y", group = "id", time = "t", treatment = "D",
                          effects = n_eff, placebo = n_pl, switchers = "in",
                          same_switchers = TRUE, same_switchers_pl = TRUE, graph_off = TRUE)$results
  eff <- as.data.table(r$Effects, keep.rownames = "k")[, term := as.numeric(sub("Effect_", "", k)) - 1]
  pl  <- as.data.table(r$Placebos, keep.rownames = "k")[, term := -1 - as.numeric(sub("Placebo_", "", k))]
  es  <- rbind(eff, pl, data.table(term = -1, Estimate = 0, SE = NA_real_), fill = TRUE)[order(term)]
  list(es = es[, .(term, estimate = Estimate, se = SE)],
       ate = r$ATE[1, c("Estimate", "SE")], p_placebo = r$p_jointplacebo,
       n_units = length(unique(d$id)), n_switchers = max(r$Effects[, "Switchers"]))
}

# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) check_dcdh(run_dcdh)

tidy <- list(); overall <- list()
if (do_estimate) {
for (tr in c("jobloss_onoff", "pinc25")) for (sm in c("all", "men", "women")) {
  if (!run_tr(tr)) next
  d <- get_panel(panels, tr, sm)
  for (y in outcomes) {
    key <- paste(tr, sm, y, sep = "|")
    r <- try_est(run_dcdh(d, y), key)
    if (is.null(r)) next
    tidy[[key]] <- as_tidy(r$es, "dCDH", "main", tr, sm, y, r$n_units, r$n_switchers)
    overall[[key]] <- data.table(estimator = "dCDH", spec = "main", treatment = tr, sample = sm,
                                 outcome = y, att = r$ate[["Estimate"]], se = r$ate[["SE"]],
                                 p_joint_placebo = r$p_placebo)
  }
}

save_est(list(tidy = rbindlist(tidy), overall = rbindlist(overall)), "dcdh")
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("dcdh")

x <- res$tidy[sample == "all"]
ggsave(path_fig("dcdh_jobloss"), plot_es(x, xlab = "Waves relative to first job loss"),
       width = 8, height = 2 + 2 * ceiling(uniqueN(x$outcome) / 2))
gen <- res$tidy[sample %in% c("men", "women")][, sample := factor(sample, c("men", "women"), c("Men", "Women"))]
ggsave(path_fig("dcdh_gender"), plot_es(gen, color = "sample", xlab = "Waves relative to first job loss"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(gen$outcome) / 2))

}

### END
