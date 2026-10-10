### New DiD analysis: known-answer checks
# Every estimator script runs its check before estimating anything; Sys.setenv(DID_CHECK_ONLY = "1")
# runs only the check, and 00_checks.R runs all of them that way. A check simulates a panel with
# known effects, runs the script's OWN estimation function on it (passed in as an argument, so the
# check cannot drift from the code it checks) and compares the estimates with the truth. The
# simulated panel has person and wave effects, effects that grow with time since treatment (an
# off-by-one event time is off by 0.5), a wave in which the outcome was not fielded (one cohort's
# reference wave falls into it, as wave 15 does for the politics outcomes) and almost no noise.
# This catches errors in event-time mapping, reference waves, wave renumbering, aggregation and
# the package workarounds, including behaviour changes after a package update. A failed check
# stops the script. Requires 00_setup.R.

sim_effect <- function(e) 1 + 0.5 * pmin(e, event_window[2] + 1)   # constant from the upper bin on
max_se <- 0.25                                                     # larger SEs mean something broke

check_result <- function(check, pass, detail, strict) {
  r <- data.table(check = check, pass = isTRUE(pass), detail = detail)
  message(sprintf("  [%s] %s: %s", if (r$pass) "PASS" else "FAIL", check, detail))
  if (strict && !r$pass) stop("check failed: ", check, " (", detail, ")")
  invisible(r)
}

# estimates within 0.02 + 4 SE of the truth, all finite, no SE above max_se
ok_est <- function(est, truth, se = 0) {
  se[is.na(se)] <- 0
  length(est) > 0 && length(est) == length(truth) && all(is.finite(est)) &&
    all(abs(est - truth) <= 0.02 + 4 * se) && all(se < max_se)
}
fmt_dev <- function(est, truth, se = 0)
  sprintf("max |estimate - truth| = %.4f, max SE = %.4f, %d terms", max(abs(est - truth)), max(c(0, se), na.rm = TRUE), length(est))

# Simulated panel: id, t, G (calendar wave of first treatment, 0 = never), D, y, tau (true effect),
# e (event time over the waves with outcome data, i.e. as estimated after prep_outcome) and the
# baseline covariates. The outcome is missing in skip_wave. onoff = TRUE: treatment lasts 2-4
# waves and then switches off; effects depend on time since the first switch and persist.
sim_panel <- function(N = 1000, T = 16, cohorts = 4:12, skip_wave = 8L, onoff = FALSE,
                      noise = 0.001, seed = 1) {
  set.seed(seed)
  d <- CJ(id = 1:N, t = 1:T)
  g <- sample(c(cohorts, rep(0L, length(cohorts))), N, replace = TRUE)   # about half never treated
  dur <- sample(2:4, N, replace = TRUE)
  d[, G := g[id]]
  if (onoff) d[, D := as.integer(G > 0 & t >= G & t < G + dur[id])] else d[, D := as.integer(G > 0 & t >= G)]
  fielded <- setdiff(1:T, skip_wave)
  k_g <- vapply(g, function(x) if (x > 0) which(fielded >= x)[1] else NA_integer_, integer(1))
  d[, e := match(t, fielded) - k_g[id]]
  d[, tau := fifelse(!is.na(e) & e >= 0, sim_effect(e), 0)]
  a <- rnorm(N); b <- rnorm(T)
  d[, y := a[id] + b[t] + tau + rnorm(.N, sd = noise)]
  d[t %in% skip_wave, y := NA_real_]
  d[, `:=`(agegrp_base = factor(id %% 3), male_base = as.integer(id %% 2 == 0),
           educ_base = factor((id %/% 2) %% 3), hhsize_base = 1 + id %% 4, inccat_base = 1 + id %% 5)]
  d[]
}


# Shared helpers ------------------------------------------------------------

# prep_outcome(): waves without the outcome are skipped and G moves to the next fielded wave
check_prep_outcome <- function(strict = TRUE) {
  d <- CJ(id = 1:3, t = 1:6)[, G := c(0L, 3L, 4L)[id]][, D := as.integer(G > 0 & t >= G)]
  d[, y := fifelse(t == 3, NA_real_, 1)]                  # column named like the argument on purpose
  p <- prep_outcome(d, "y")
  ok <- identical(sort(unique(p$t)), 1:5) && identical(p[, unique(G), by = id]$V1, c(0L, 3L, 3L)) &&
    identical(p[t_cal == 4, t], rep(3L, 3)) && identical(p$D, as.integer(p$G > 0 & p$t >= p$G))
  s <- prep_outcome(sim_panel(), "y")
  ok <- ok && s[G > 0, all(t - G == e)]
  check_result("prep_outcome", ok, "waves renumbered over fielded waves; events in and after a skipped wave start at the next fielded wave", strict)
}

# Callaway-Sant'Anna balanced panel: balance_panel() + att_gt() must give exactly what did's own
# balancing gives, also when did's balancing removes a whole cohort (did 2.5.1 stops with an
# internal error there; then the comparison is with the cohort removed beforehand)
check_cs_balancing <- function(strict = TRUE) {
  fit <- function(x) {
    a <- suppressWarnings(did::att_gt("y", "t", "id", "G", data = as.data.frame(x), allow_unbalanced_panel = FALSE,
                                      bstrap = FALSE, base_period = "universal", control_group = "nevertreated",
                                      est_method = "dr"))
    data.table(g = a$group, t = a$t, att = a$att, se = a$se)
  }
  d <- sim_panel(N = 800, T = 8, cohorts = 3:6, skip_wave = integer(0), noise = 0.5)
  set.seed(2); d <- d[runif(.N) > 0.05]                       # unbalanced: 5% of person-waves missing
  same_a <- isTRUE(all.equal(fit(d), fit(balance_panel(d)), tolerance = 1e-10))
  dB <- d[!(G == 5 & t == 2)]                                 # balancing removes cohort 5
  b_did  <- tryCatch(fit(dB), error = function(e) conditionMessage(e))
  b_ours <- fit(balance_panel(dB))
  same_b <- isTRUE(all.equal(b_ours, fit(dB[G != 5]), tolerance = 1e-10))
  bug <- is.character(b_did)
  if (!bug) same_b <- same_b && isTRUE(all.equal(b_did, b_ours, tolerance = 1e-10))
  check_result("CS balanced panel", same_a && same_b,
               sprintf("identical to did's balancing: %s; cohort-removal case: %s; did %s internal error: %s",
                       same_a, same_b, as.character(packageVersion("did")),
                       if (bug) paste0("present (", substr(b_did, 1, 60), ")") else "not present"), strict)
}


# Estimator checks (called from the scripts with their own functions) ---------

# 02_cs.R: every spec on an unbalanced panel in which balancing removes a whole cohort;
# event study -5..8 and the overall (average over 0..8) against the truth
check_cs <- function(fit, specs, strict = TRUE) {
  d <- sim_panel()
  set.seed(2); d <- d[runif(.N) > 0.03]
  d <- d[!(G == 5 & t == 2)]
  res <- rbindlist(lapply(names(specs), function(s) {
    es <- fit(copy(d), "y", specs[[s]])$es
    rbind(data.table(spec = s, e = es$egt, est = es$att.egt, se = es$se.egt)[e >= event_window[1] & e <= event_window[2]],
          data.table(spec = s, e = NA, est = es$overall.att, se = es$overall.se))
  }))
  res[, truth := fifelse(is.na(e), mean(sim_effect(0:event_window[2])), fifelse(e >= 0, sim_effect(e), 0))]
  check_result("CS (run_cs)", ok_est(res$est, res$truth, res$se),
               paste0(fmt_dev(res$est, res$truth, res$se), "; specs: ", paste(names(specs), collapse = ", ")), strict)
}

# 03_fect.R: FE fit through run_fect() and fect_tidy() (fect's event time 1 = first treated wave)
check_fect <- function(fit, tidy_fun, strict = TRUE) {
  d <- prep_outcome(sim_panel(N = 600), "y")
  f <- fit(d, "y", "fe")
  x <- tidy_fun(f$est.att, "ATT")[term >= event_window[1] & term <= event_window[2]]
  x[, truth := fifelse(term >= 0, sim_effect(term), 0)]
  est <- c(x$estimate, f$est.avg[1, "ATT.avg"]); truth <- c(x$truth, d[D == 1, mean(tau)])
  se <- c(x$se, f$est.avg[1, "S.E."])
  check_result("fect (run_fect + fect_tidy)", ok_est(est, truth, se), fmt_dev(est, truth, se), strict)
}

# 04_imputation.R: both first stages; horizons, pre-trend coefficients and the overall effect
check_bjs <- function(fit, stages, strict = TRUE) {
  d <- sim_panel()
  p <- prep_outcome(d, "y")
  ov_truth <- p[D == 1 & t - G <= event_window[2], mean(tau)]
  res <- rbindlist(lapply(names(stages), function(s) {
    r <- fit(copy(d), "y", stages[[s]])
    rbind(r$es[, .(term = as.numeric(term), est = estimate, se = std.error)],
          data.table(term = NA, est = r$ov$estimate[1], se = r$ov$std.error[1]))
  }))
  res[, truth := fifelse(is.na(term), ov_truth, fifelse(term >= 0, sim_effect(term), 0))]
  check_result("BJS imputation (run_bjs)", ok_est(res$est, res$truth, res$se), fmt_dev(res$est, res$truth, res$se), strict)
}

# 05_dcdh.R: on/off treatment; effects of the first switch (Effect_l -> l - 1), placebos (-> -1 - l)
check_dcdh <- function(fit, strict = TRUE) {
  d <- sim_panel(T = 20, cohorts = 6:10, skip_wave = 9L, onoff = TRUE)
  x <- fit(d, "y")$es[!is.na(se)]
  x[, truth := fifelse(term >= 0, sim_effect(term), 0)]
  ok <- ok_est(x$estimate, x$truth, x$se) && setequal(x$term, setdiff(event_window[1]:event_window[2], -1))
  check_result("DIDmultiplegtDYN (run_dcdh)", ok, fmt_dev(x$estimate, x$truth, x$se), strict)
}

# 06_sunab_twfe.R: Sun-Abraham and TWFE event studies with binned ends, Sun-Abraham overall
# (fixest's aggregation over 0..8), and no cohort dropped (the skipped wave is cohort 9's
# calendar reference wave)
check_sunab <- function(fit, strict = TRUE) {
  d <- sim_panel()
  p <- prep_outcome(d, "y")
  res <- rbindlist(lapply(c("sunab", "twfe"), function(type) {
    r <- fit(copy(d), "y", type)
    if (r$n_treated != uniqueN(p[G > 0, id])) return(data.table(term = NA, est = NA_real_, se = NA_real_))
    x <- r$es[term != -1, .(term, est = estimate, se)]
    if (type == "sunab") x <- rbind(x, data.table(term = NA, est = r$ov[[1]], se = r$ov[[2]]))
    x
  }))
  ov_truth <- p[G > 0 & t - G >= 0 & t - G <= event_window[2], mean(tau)]
  res[, truth := fifelse(is.na(term), ov_truth, fifelse(term >= 0, sim_effect(term), 0))]
  check_result("Sun-Abraham + TWFE (run_fixest)", ok_est(res$est, res$truth, res$se), fmt_dev(res$est, res$truth, res$se), strict)
}

# 07_synthdid.R: cells for every cohort, two horizons and every number of pre-waves
check_synthdid <- function(cell_fun, pre_waves, strict = TRUE) {
  p <- prep_outcome(sim_panel(N = 600, cohorts = 6:10), "y")
  cells <- CJ(g = sort(unique(p[G > 0, G])), h = c(0L, 3L), k = unname(pre_waves))
  res <- rbindlist(lapply(seq_len(nrow(cells)), function(i) cell_fun(p, "y", cells$g[i], cells$h[i], cells$k[i])))
  ok <- nrow(res) == nrow(cells) && ok_est(res$estimate, sim_effect(res$term), res$se)
  check_result("synthdid (sdid_cell)", ok,
               sprintf("%d of %d cells; %s", nrow(res), nrow(cells), fmt_dev(res$estimate, sim_effect(res$term), res$se)), strict)
}

# 08_panelmatch.R: on/off treatment, controls that switch later are allowed (PanelMatch's ATT is
# the effect of switching at t, without conditioning on later treatment). For each lead the
# estimate must (a) equal the matched-set contrast of Y_{t+h} - Y_{t-1} computed by hand from the
# matched sets and weights (lead/reference mapping, weighting), (b) match the same contrast of
# the true effects (the estimand), and (c) the reported share of matched controls treated at t+h
# must match the share computed by hand
check_panelmatch <- function(prep, formula_fun, match_fun, est_fun, strict = TRUE) {
  d  <- sim_panel(N = 800, T = 14, cohorts = 6:10, onoff = TRUE, noise = 0.05)
  p  <- prep(d, "y")
  pd <- PanelMatch::PanelData(p$d, "id", "t", "D", "y")
  f  <- formula_fun("y", p$covs)
  v  <- prep_outcome(d, "y", absorbing = FALSE)
  set_contrast <- function(pm, val, h, diff = TRUE) {
    ms <- pm[["att"]]; ms <- ms[lengths(ms) > 0]
    V  <- setNames(v[[val]], paste(v$id, v$t))
    it <- do.call(rbind, lapply(strsplit(names(ms), ".", fixed = TRUE), as.integer))
    at <- function(j, t0) if (diff) V[paste(j, t0 + h)] - V[paste(j, t0 - 1)] else V[paste(j, t0 + h)]
    mean(vapply(seq_along(ms), function(k) {
      w <- attr(ms[[k]], "weights")[as.character(ms[[k]])]; w <- w / sum(w)
      if (diff) at(it[k, 1], it[k, 2]) - sum(w * at(ms[[k]], it[k, 2])) else sum(w * at(ms[[k]], it[k, 2]))
    }, numeric(1)), na.rm = TRUE)
  }
  res <- rbindlist(lapply(0:3, function(h) {
    pm <- match_fun(pd, f, h); r <- est_fun(pd, f, h)
    data.table(h = h, est = r$estimate, se = r$se, by_hand = set_contrast(pm, "y", h),
               truth = set_contrast(pm, "tau", h), share = r$ctrl_treated, share_hand = set_contrast(pm, "D", h, FALSE))
  }))
  ok <- all(abs(res$est - res$by_hand) < 1e-6) && ok_est(res$est, res$truth, res$se) &&
    all(abs(res$share - res$share_hand) < 1e-8)
  check_result("PanelMatch (est_lead)", ok,
               sprintf("max |estimate - by hand| = %.1e; %s; controls treated at t+h: %s; effect for treated alone: %s",
                       max(abs(res$est - res$by_hand)), fmt_dev(res$est, res$truth, res$se),
                       paste(round(res$share, 3), collapse = "/"), paste(sim_effect(0:3), collapse = "/")), strict)
}
