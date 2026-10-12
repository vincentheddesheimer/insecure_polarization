### New DiD analysis: synthetic difference-in-differences (synthdid), per cohort
# synthdid needs a balanced block with one adoption date, so it is run separately for each
# job-loss cohort g and each post-treatment wave h (0..8): pre-period = the k waves before g,
# post = wave g+h, sample = job losers of cohort g and never-unemployed people observed in all of
# these waves. Waves are numbered over the waves in which the outcome was fielded (prep_outcome).
# Main: k = 3 pre-waves; robustness: k = 5. (For distance and job loss at h = 0, k = 3 keeps 278
# of 698 job losers, k = 5 keeps 173; requiring the whole -5..+8 window would keep far fewer.)
# Effects at h are averaged over cohorts with weights = number of treated people; standard errors
# from synthdid's placebo method, combined across cohorts assuming independence (approximate:
# cohorts share never-unemployed controls).
# Overall effect: per cohort, the mean over its horizons, with a conservative SE that assumes
# perfect correlation across horizons (same people and pre-period); then averaged over cohorts
# (weights = treated people), cohorts treated as independent.

rm(list = ls())
here::i_am("code/analyze_did/07_synthdid.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(synthdid)
source(here("code", "analyze_did", "checks.R"))

panels <- load_panels()
reps <- if (test_mode) 20 else 100
RNGkind("L'Ecuyer-CMRG")       # reproducible streams across mclapply workers
set.seed(20261007)

pre_waves <- c(main = 3L, pre5 = 5L)

sdid_cell <- function(d, y, g, h, k) {
  pre  <- seq(g - k, g - 1)
  post <- g + h
  if (min(pre) < 1 || post > max(d$t)) return(NULL)
  win <- c(pre, post)
  dd  <- d[t %in% win & (G == g | G == 0)]
  dd  <- dd[id %in% dd[, .N, by = id][N == length(win), id]]
  n1  <- uniqueN(dd[G == g, id]); n0 <- uniqueN(dd[G == 0, id])
  if (n1 < 1 || n0 <= n1 + 1) return(NULL)
  dd[, W := as.integer(G == g & t == post)]
  s   <- panel.matrices(data.frame(id = dd$id, t = dd$t, y = dd[[y]], W = dd$W), unit = "id", time = "t",
                        outcome = "y", treatment = "W")
  est <- synthdid_estimate(s$Y, s$N0, s$T0)
  data.table(cohort = g, term = h, estimate = as.numeric(est),
             se = sqrt(as.numeric(vcov(est, method = "placebo", replications = reps))), n1 = n1, n0 = n0)
}

run_sdid <- function(d, y, k) {
  d <- prep_outcome(d, y)
  cells <- CJ(g = sort(unique(d[G > 0, G])), h = 0:event_window[2])
  res <- rbindlist(parallel::mclapply(seq_len(nrow(cells)), function(i)
    tryCatch(sdid_cell(d, y, cells$g[i], cells$h[i], k), error = function(e) NULL),
    mc.cores = n_cores))
  if (!nrow(res)) stop("no estimable cohort/wave cells")
  es <- res[, .(estimate = weighted.mean(estimate, n1), se = sqrt(sum((n1 / sum(n1))^2 * se^2)),
                n_treated = sum(n1)), by = term][order(term)]
  by_g <- res[, .(est = mean(estimate), se = mean(se), n1 = mean(n1)), by = cohort]   # se: perfect-correlation bound
  ov <- by_g[, .(att = weighted.mean(est, n1), se = sqrt(sum((n1 / sum(n1))^2 * se^2)))]
  list(es = es, ov = ov, cells = res, n_treated = uniqueN(d[G > 0 & G %in% res$cohort, id]))
}

# Known-answer check (checks.R; stops the script if it fails) ----------------

if (run_checks) check_synthdid(sdid_cell, pre_waves)

tidy <- list(); overall <- list(); cells <- list()
if (do_estimate) {
for (tr in c("jobloss", "pinc25")) for (sm in c("all", "men", "women")) {
  if (!run_tr(tr)) next
  d <- get_panel(panels, tr, sm)
  specs <- if (sm == "all" && tr == "jobloss") names(pre_waves) else "main"
  for (sp in specs) for (y in outcomes) {
    key <- paste(tr, sm, sp, y, sep = "|")
    r <- try_est(run_sdid(d, y, pre_waves[[sp]]), key)
    if (is.null(r)) next
    tidy[[key]] <- as_tidy(r$es[, .(term, estimate, se)], "synthdid", sp, tr, sm, y,
                           NA_integer_, r$n_treated)
    overall[[key]] <- data.table(estimator = "synthdid", spec = sp, treatment = tr, sample = sm,
                                 outcome = y, att = r$ov$att, se = r$ov$se)
    cells[[key]] <- r$cells[, `:=`(treatment = tr, sample = sm, spec = sp, outcome = y)]
  }
}
save_est(list(tidy = rbindlist(tidy), overall = rbindlist(overall), cells = rbindlist(cells)), "synthdid")
}


# Figures -------------------------------------------------------------------

if (do_figures) {
res <- load_est("synthdid")
x <- res$tidy[sample == "all"][, spec := factor(spec, names(pre_waves), c("3 pre-waves (main)", "5 pre-waves"))]
ggsave(path_fig("synthdid_jobloss"), plot_es(x, color = "spec", xlab = "Waves relative to job loss"),
       width = 8, height = 2 + 2 * ceiling(uniqueN(x$outcome) / 2))
gen <- res$tidy[sample %in% c("men", "women") & spec == "main"][, sample := factor(sample, c("men", "women"), c("Men", "Women"))]
ggsave(path_fig("synthdid_gender"), plot_es(gen, color = "sample", xlab = "Waves relative to job loss"),
       width = 9, height = 2.5 + 2 * ceiling(uniqueN(gen$outcome) / 2))

}

### END
