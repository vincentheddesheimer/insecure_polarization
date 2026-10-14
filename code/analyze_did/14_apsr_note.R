### APSR research note: figures, tables and in-text numbers (October 2026)
# Writes to the Overleaf project (Dropbox: Apps/Overleaf/HB_insecure_polarization/apsr/):
# figures/*.pdf, tables/*.tex and numbers.tex, which defines every number in the text as a macro.
#
# Design (the note's research design section): Callaway-Sant'Anna, never-treated comparison group,
# wave -1 as baseline, no covariates. One robustness check per identifying assumption:
#   comparison group           -> not-yet-treated people as controls (CS)
#   selection on observables   -> baseline covariates, doubly robust (CS); and the main specification
#                                  on the same complete-case sample, to separate sample and adjustment
#   no anticipation            -> waves -5 to -3 as the baseline (CS contrasts)
#   no compositional change    -> within-person estimator: the same people in the baseline wave and
#                                  in each later wave, one employment rule for treated and controls
#                                  (with an unbalanced panel, did compares group means across waves)
#   no spillovers              -> controls without household members of treated people; SEs
#                                  clustered by household
#   parallel trends, all waves -> imputation estimator (Borusyak, Jaravel & Spiess), more efficient
#                                  if trends are parallel in every pre-treatment wave
#   size of violations         -> Rambachan-Roth bounds (HonestDiD): relative magnitudes and smoothness
# Mechanism outcomes (00_setup.R mechanism_labels): confidence in institutions, satisfaction.
# Generalized trust and heterophily are fielded between politics interviews and use wave -2 as the
# reference (shift_between() in 00_setup.R).
# Run order: 01_sample.R, then 10, 12, 13, 15, 16, 17, 18 and 19 (their saved results are read here), then this
# script. CS is fitted with analytic (influence-function) standard errors, so that averages over waves
# have their full covariance; BJS window averages use didimputation's weights. Fits are cached in
# results/estimates/apsr/refits.rds with a tag of the panels and definitions (cache_tag() in
# 00_setup.R); a cache with another tag, or DID_APSR_REFIT=1, is recomputed (about an hour).
# Treatments: large personal income loss (pinc25) and job loss (jobloss).

rm(list = ls())
here::i_am("code/analyze_did/14_apsr_note.R")
Sys.setenv(DID_NO_FIGURES = "")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fixest, did, didimputation, HonestDiD)

out_dir <- normalizePath(here("..", "..", "Apps", "Overleaf", "HB_insecure_polarization", "apsr"), mustWork = FALSE)
for (s in c("figures", "tables")) dir.create(file.path(out_dir, s), recursive = TRUE, showWarnings = FALSE)
fig_path <- function(name) file.path(out_dir, "figures", paste0(name, ".pdf"))
tab_path <- function(name) file.path(out_dir, "tables", paste0(name, ".tex"))

tr_lab <- c(pinc25 = "Large income loss", jobloss = "Job loss")
tr_key <- c(pinc25 = "Inc", jobloss = "Job")
y_key  <- c(distance = "Dist", like_max = "Max", like_min = "Min", spread = "Spread",
            generalized_trust = "Trust", red_overall_mean_distance = "Het",
            confidence_index = "Conf", confidence_gov = "ConfGov", satisfaction_gov = "SatGov",
            satisfaction_dem = "SatDem")
mech <- names(mechanism_labels)
sm_key <- c(all = "", men = "Men", women = "Women")


# Saved results of 10, 12, 13, 15, 16 and 17 (rerun on the current data) ---------------------

fu  <- readRDS(here("results", "estimates", "followups", "followups.rds"))
alt <- readRDS(here("results", "estimates", "alt", "alt.rds"))
dip <- readRDS(here("results", "estimates", "dip", "dip.rds"))
imp <- readRDS(here("results", "estimates", "apsr", "imputed_check.rds"))
ch  <- readRDS(here("results", "estimates", "mechanisms", "channels_summary.rds"))
hr  <- readRDS(here("results", "estimates", "mechanisms", "horse_summary.rds"))
md  <- readRDS(here("results", "estimates", "mechanisms", "mediation_summary.rds"))
mfit <- function(tr, y) ch$fits[[paste(tr, y, sep = "|")]]
# 17_extensions.R: measurement, blame, treatments, inference (summaries only)
ex_m <- readRDS(here("results", "estimates", "extensions", "measurement_summary.rds"))$summary
ex_bl <- readRDS(here("results", "estimates", "extensions", "blame_summary.rds")); ex_b <- ex_bl$summary
ex_t <- readRDS(here("results", "estimates", "extensions", "treatments_summary.rds"))
ex_i <- readRDS(here("results", "estimates", "extensions", "inference_summary.rds"))
ex_s <- readRDS(here("results", "estimates", "extensions", "selection_summary.rds"))


# Refits ----------------------------------------------------------------------

# imputation estimator, average over the treated observations in waves h after the loss
bjs_window <- function(d, y, h) {
  d <- as.data.frame(prep_outcome(d, y))
  w <- as.numeric(d$D == 1 & (d$t - d$G) %in% h); d$w_win <- w / sum(w)
  ov <- didimputation::did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id",
                                      wtr = "w_win", cluster_var = "id")
  list(estimate = ov$estimate[1], se = ov$std.error[1], n_treated = length(unique(d$id[d$G > 1])))
}
# imputation event study, waves -5..8 (pre-treatment estimates from its pre-trend regression)
bjs_es <- function(d, y) {
  d <- as.data.frame(prep_outcome(d, y))
  es <- didimputation::did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id",
                                      horizon = 0:event_window[2], pretrends = event_window[1]:-1, cluster_var = "id")
  data.table(term = as.integer(es$term), estimate = es$estimate, se = es$std.error)
}
# Within-person estimator (no compositional change). For every cohort g and wave t, it compares the
# change in the outcome from wave g - 1 to wave t between people who suffer the loss in g and people
# who never do, among those observed in both waves and employed in g - 1 (before the loss also in t);
# after the loss everyone's waves are kept, whatever their employment status, for treated and controls
# alike (d: an unrestricted panel from 01_sample.R, *_all). Cells are averaged over cohorts by the
# number of treated people observed; standard errors from the influence function, clustered by person.
wp_fit <- function(d, y) {
  d <- prep_outcome(d, y)
  base <- d[, .(id, t_b = t, y_b = get(y), emp_b = emp)]
  rows <- rbindlist(lapply(sort(unique(d[G > 1, G])), function(g) {
    b <- base[t_b == g - 1L & emp_b %in% TRUE]
    x <- d[G %in% c(0L, g) & t != g - 1L & t - g >= event_window[1] & t - g <= event_window[2]]
    x <- x[t >= g | emp %in% TRUE][b, on = "id", nomatch = 0]
    x[, .(id, g = g, e = t - g, treated = G == g, dy = get(y) - y_b)]
  }))
  rows[, `:=`(n_t = sum(treated), n_c = sum(!treated), m_t = mean(dy[treated]), m_c = mean(dy[!treated])), by = .(g, e)]
  rows <- rows[n_t > 0 & n_c > 0]
  cells <- unique(rows[, .(g, e, n_t, n_c, m_t, m_c)])[, w := n_t / sum(n_t), by = e]
  rows[cells, on = c("g", "e"), w := i.w]
  rows[, psi := fifelse(treated, w * (dy - m_t) / n_t, -w * (dy - m_c) / n_c)]
  inf <- dcast(rows[, .(psi = sum(psi)), by = .(id, e)], id ~ e, value.var = "psi", fill = 0)
  b <- cells[order(e), .(b = sum(w * (m_t - m_c))), by = e]
  b <- setNames(b$b, b$e); M <- as.matrix(inf[, as.character(names(b)), with = FALSE])
  V <- crossprod(M); dimnames(V) <- list(names(b), names(b))
  list(b = b, V = V, n_treated = uniqueN(rows[treated == TRUE, id]), n_units = uniqueN(rows$id))
}
# controls without household members of treated people (no spillovers within households)
drop_hh_controls <- function(d) { h <- d[G > 0, unique(hh)]; d[!(G == 0 & hh %in% h)] }
# covariate check sample: people with all baseline covariates
complete_covs <- function(d) d[complete.cases(d[, ..baseline_covs])]

jobs <- rbind(
  CJ(kind = "cs_main", treatment = names(tr_lab), sample = c("all", "men", "women"), outcome = "distance"),
  CJ(kind = "cs_main", treatment = names(tr_lab), sample = "all",
     outcome = c("like_max", "like_min", "spread", "generalized_trust", "red_overall_mean_distance")),
  CJ(kind = c("cs_notyet", "cs_covars", "cs_cc", "cs_nohh", "wp"), treatment = names(tr_lab), sample = "all", outcome = "distance"),
  CJ(kind = c("bjs_avg", "bjs_mid"), treatment = names(tr_lab), sample = "all", outcome = "distance"),
  CJ(kind = "bjs_avg", treatment = names(tr_lab), sample = c("men", "women"), outcome = "distance"),
  CJ(kind = "bjs_avg", treatment = names(tr_lab), sample = "all", outcome = c("generalized_trust", "red_overall_mean_distance", mech)),
  CJ(kind = "cs_main", treatment = names(tr_lab), sample = "all", outcome = mech),
  CJ(kind = "bjs_es", treatment = names(tr_lab), sample = "all", outcome = "distance"))
jobs[, key := paste(kind, treatment, sample, outcome, sep = "|")]

path_cache <- path_est("apsr", "refits")
cache <- if (file.exists(path_cache) && Sys.getenv("DID_APSR_REFIT") != "1") readRDS(path_cache) else list()
if (!identical(cache$tag, cache_tag())) { if (length(cache)) message("refits: cache from other panels or definitions, recomputing"); cache <- list(tag = cache_tag()) }
todo <- jobs[!key %in% names(cache)]
if (nrow(todo) || is.null(cache$desc) || is.null(cache$overlap)) {
  panels <- load_panels()
  for (i in seq_len(nrow(todo))) {
    j <- todo[i]
    d <- get_panel(panels, if (j$kind == "wp") paste0(j$treatment, "_all") else j$treatment, j$sample)
    if (j$outcome %in% between_outcomes) d <- shift_between(d)
    message("fit ", j$key)
    cache[[j$key]] <- switch(j$kind,
      cs_main   = cs_fit(d, j$outcome, return_inf = j$outcome == "distance" && j$sample == "all"),
      cs_notyet = cs_fit(d, j$outcome, control = "notyettreated"),
      cs_covars = cs_fit(d, j$outcome, xformla = xformla_base),
      cs_cc     = cs_fit(complete_covs(d), j$outcome),
      cs_nohh   = cs_fit(drop_hh_controls(d), j$outcome),
      wp        = wp_fit(d, j$outcome),
      bjs_avg   = bjs_window(d, j$outcome, 0:8),
      bjs_mid   = bjs_window(d, j$outcome, 2:4),
      bjs_es    = bjs_es(d, j$outcome))
    saveRDS(cache, path_cache)
  }
  # estimation samples (distance)
  cache$desc <- rbindlist(lapply(names(tr_lab), function(tr) {
    d <- prep_outcome(panels[[tr]], "distance")
    data.table(treatment = tr, persons = uniqueN(d$id), person_waves = nrow(d), waves = uniqueN(d$t_cal),
               treated = uniqueN(d[G > 1, id]), treated_women = uniqueN(d[G > 1 & male_base == 0, id]),
               cohorts = uniqueN(d[G > 1, G]), mean_d = mean(d$distance), sd_d = sd(d$distance))
  }))
  # outcome means and SDs among the never unemployed (job-loss panel), as in 11_power.R
  cache$sds <- rbindlist(lapply(c(names(y_key)), function(y) {
    d <- prep_outcome(panels$jobloss, y)[G == 0]
    d[, dev := get(y) - mean(get(y)), by = id]
    data.table(outcome = y, sd_pw = sd(d[[y]]), sd_within = sd(d[, if (.N > 1) .(dev = dev), by = id]$dev), mean = mean(d[[y]]))
  }))
  # overlap of the two treatments (calendar wave of the first loss). An income loss requires an
  # employed previous wave, so it cannot fall in the wave after a job loss.
  gj <- unique(panels$jobloss[G > 0, .(id, Gj = G)]); gi <- unique(panels$pinc25[G > 0, .(id, Gi = G)])
  both <- merge(gj, gi, by = "id")
  cache$overlap <- data.table(job_n = nrow(gj), inc_n = nrow(gi), both_n = nrow(both), same = both[Gi == Gj, .N])
  # households of the treated, for household-clustered standard errors
  cache$hh <- rbindlist(lapply(names(tr_lab), function(tr) unique(panels[[tr]][, .(treatment = tr, id, hh)])))
  saveRDS(cache, path_cache)
}
desc <- cache$desc
sd_y <- setNames(cache$sds$sd_pw, cache$sds$outcome)
sd_d <- sd_y[["distance"]]
fit <- function(kind, tr, y = "distance", sm = "all") cache[[paste(kind, tr, sm, y, sep = "|")]]
# window average with standard errors clustered by household (from the person-level influence function)
lc_hh <- function(tr, nm) {
  f <- fit("cs_main", tr); hh <- cache$hh[treatment == tr][match(f$ids, id), hh]
  V <- crossprod(rowsum(f$inf, hh)) / nrow(f$inf)^2; dimnames(V) <- dimnames(f$V)
  lc(list(b = f$b, V = V), nm)
}
es_dt <- function(f) data.table(term = as.integer(names(f$b)), estimate = f$b, se = sqrt(diag(f$V)))

# Rambachan-Roth relative-magnitude bounds for the averages over waves 0-8 and 2-4
if (is.null(cache$honest)) { cache$honest <- rbindlist(lapply(names(tr_lab), function(tr) {
  f <- fit("cs_main", tr); e <- as.integer(names(f$b)); npre <- sum(e < 0); npost <- sum(e >= 0)
  targets <- list(Avg = rep(1 / npost, npost), Mid = as.numeric(e[e >= 0] %in% 2:4) / 3)
  rbindlist(lapply(names(targets), function(nm) {
    l <- targets[[nm]]
    o <- HonestDiD::constructOriginalCS(betahat = f$b, sigma = f$V, numPrePeriods = npre, numPostPeriods = npost, l_vec = l)
    r <- HonestDiD::createSensitivityResults_relativeMagnitudes(betahat = f$b, sigma = f$V, numPrePeriods = npre,
                                                                numPostPeriods = npost, Mbarvec = c(seq(0, 0.3, 0.05), 0.5, 1, 1.5, 2), l_vec = l)
    rbind(as.data.table(o)[, Mbar := NA_real_], as.data.table(r), fill = TRUE)[, `:=`(target = nm, treatment = tr)]
  }))
})); saveRDS(cache, path_cache) }
honest <- cache$honest
breakdown <- honest[!is.na(Mbar), .(Mbar = if (any(lb > 0)) max(Mbar[lb > 0]) else NA_real_), by = .(treatment, target)]
# smoothness bounds: the slope of the difference in trends may change by at most M per wave
if (is.null(cache$honest_sd)) { cache$honest_sd <- rbindlist(lapply(names(tr_lab), function(tr) {
  f <- fit("cs_main", tr); e <- as.integer(names(f$b)); npre <- sum(e < 0); npost <- sum(e >= 0)
  targets <- list(Avg = rep(1 / npost, npost), Mid = as.numeric(e[e >= 0] %in% 2:4) / 3)
  rbindlist(lapply(names(targets), function(nm) {
    r <- HonestDiD::createSensitivityResults(betahat = f$b, sigma = f$V, numPrePeriods = npre, numPostPeriods = npost,
                                             method = "FLCI", Mvec = c(0, 0.005, 0.01, 0.02, 0.03, 0.05, 0.1), l_vec = targets[[nm]])
    as.data.table(r)[, `:=`(target = nm, treatment = tr)]
  }))
})); saveRDS(cache, path_cache) }
honest_sd <- cache$honest_sd
breakdown_sd <- honest_sd[, .(M = if (any(lb > 0)) max(M[lb > 0]) else NA_real_), by = .(treatment, target)]


# Helpers -------------------------------------------------------------------------

stars <- function(e, s) fifelse(abs(e / s) > 2.576, "***", fifelse(abs(e / s) > 1.96, "**", fifelse(abs(e / s) > 1.645, "*", "")))
f2 <- function(x) sub("^-(0\\.00)$", "\\1", sprintf("%.2f", x))
f3 <- function(x) sub("^-(0\\.000)$", "\\1", sprintf("%.3f", x))
neg <- function(x) sub("^-", "$-$", x)
cell <- function(e, s) sprintf("%s%s", neg(f2(e)), fifelse(is.na(s) | stars(e, s) == "", "", paste0("$^{", stars(e, s), "}$")))
cell_se <- function(e, s) sprintf("%s (%s)", cell(e, s), f2(s))
big <- function(n) format(n, big.mark = ",", trim = TRUE)
pval <- function(p) if (p < 0.001) "$<$0.001" else sprintf("%.2f", p)
words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
n_word <- function(n) if (n < 10) words[n + 1] else big(n)   # numbers below ten as words

nums <- list()
num <- function(name, value) { stopifnot(grepl("^[A-Za-z]+$", name)); nums[[name]] <<- as.character(value) }
num_est <- function(name, e, s, sd = sd_d) {
  num(name, neg(f2(e))); num(paste0(name, "SE"), f2(s)); num(paste0(name, "SD"), neg(f2(e / sd)))
  num(paste0(name, "Lo"), neg(f2(e - 1.96 * s))); num(paste0(name, "Hi"), neg(f2(e + 1.96 * s)))
}


# Numbers: samples, outcome, benchmarks ----------------------------------------------

for (tr in names(tr_lab)) {
  k <- tr_key[[tr]]; x <- desc[treatment == tr]
  num(paste0(k, "Persons"), big(x$persons)); num(paste0(k, "PersonWaves"), big(x$person_waves))
  num(paste0(k, "Treated"), big(x$treated)); num(paste0(k, "TreatedWomenPct"), sprintf("%.0f", 100 * x$treated_women / x$treated))
  num(paste0(k, "Cohorts"), x$cohorts)
}
num("Waves", desc[treatment == "jobloss", waves])
num("DistMean", f2(cache$sds[outcome == "distance", mean])); num("DistSD", f2(sd_d))
num("DistSDWithin", f2(cache$sds[outcome == "distance", sd_within]))
info <- fu$inc_info
num("IncTreatedAll", big(info[variant == "drop >= 25% (main)", treated]))
num("IncUnempAtEventPct", sprintf("%.0f", 100 * info[variant == "drop >= 25% (main)", unemployed]))
# what people with a large income loss do in the wave of the loss
x <- info[variant == "drop >= 25% (main)"]
num("IncEmpAtEventPct", sprintf("%.0f", 100 * x$employed)); num("IncOtherAtEventPct", sprintf("%.0f", 100 * (1 - x$employed - x$unemployed)))
num("IncInactiveAtEventPct", sprintf("%.0f", 100 * x$pensioner_student_homemaker)); num("IncZeroAtEventPct", sprintf("%.0f", 100 * x$to_zero))
num("IncSuspectPct", sprintf("%.0f", 100 * x$suspect))
num("IncVolPct", sprintf("%.0f", 100 * (1 - info[variant == "drop >= 25%, without voluntary-looking drops", treated] /
                                         info[variant == "drop >= 25% (main)", treated])))
num("IncNoVolTreated", big(info[variant == "drop >= 25%, without voluntary-looking drops", treated]))
# income losses involving an imputed income value (event wave or the wave before) and the estimates without them
num("IncImpEvents", imp$n_imp); num("IncImpEventsAll", big(imp$n_events))
num("IncImpEventsPct", sprintf("%.1f", 100 * imp$n_imp / imp$n_events))
num("IncImpMid", neg(f2(imp$noimp["estimate", "mid"]))); num("IncImpAvg", neg(f2(imp$noimp["estimate", "avg"])))
# composition of income losses by gender: births and unemployment in the wave of the loss
for (g in c("men", "women")) {
  x <- info[variant == paste0("drop >= 25%, ", g)]; G <- c(men = "Men", women = "Women")[[g]]
  num(paste0("IncNewChild", G, "Pct"), sprintf("%.0f", 100 * x$new_child))
  num(paste0("IncUnemp", G, "Pct"), sprintf("%.0f", 100 * x$unemployed))
}
num("IncShareEverPct", sprintf("%.0f", 100 * alt$info[treat == "p25", share_treated]))
num("JobShareEverPct", sprintf("%.0f", 100 * alt$info[treat == "jobloss", share_treated]))
num("JobTreatedAll", big(alt$info[treat == "jobloss", treated]))

liss_d <- fread(here("data", "liss_to26.csv"), select = c("wave", "distance"))[!is.na(distance)]
trend <- liss_d[, .(mean = mean(distance)), by = wave][order(wave)]
yr <- function(w) sprintf("%d/%02d", 1999 + w, (2000 + w) %% 100)
num("TrendFirst", neg(f2(trend[1, mean]))); num("TrendLast", neg(f2(trend[.N, mean])))
num("TrendFirstLabel", yr(trend[1, wave])); num("TrendLastLabel", yr(trend[.N, wave]))
trend_change <- trend[.N, mean] - trend[1, mean]
num("TrendChange", f2(trend_change))


# Numbers: CS estimates (main and checks), BJS, Rambachan-Roth ----------------------------

for (tr in names(tr_lab)) for (y in names(y_key)) for (sm in names(sm_key)) {
  f <- fit("cs_main", tr, y, sm); if (is.null(f)) next
  for (nm in names(windows)) {
    x <- lc(f, nm); num_est(paste0(tr_key[[tr]], y_key[[y]], "CS", sm_key[[sm]], nm), x$estimate, x$se, sd_y[[y]])
  }
  if (y %in% c("distance", mech)) num(paste0(tr_key[[tr]], y_key[[y]], "CS", sm_key[[sm]], "PreP"), pval(pre_p(f)))
}
for (tr in names(tr_lab)) {
  k <- tr_key[[tr]]
  for (kind in c("cs_notyet", "cs_covars", "cs_cc", "cs_nohh", "wp")) {
    f <- fit(kind, tr); s <- c(cs_notyet = "NY", cs_covars = "Cov", cs_cc = "CC", cs_nohh = "NoHH", wp = "WP")[[kind]]
    for (nm in c("Avg", "Mid")) { x <- lc(f, nm); num_est(paste0(k, "Dist", s, nm), x$estimate, x$se) }
    num(paste0(k, "Dist", s, "PreP"), pval(pre_p(f)))
    num(paste0(k, "Dist", s, "Treated"), big(f$n_treated))
  }
  # main estimates with standard errors clustered by household
  for (nm in c("Avg", "Mid")) num(paste0(k, "DistHHSE", nm), f2(lc_hh(tr, nm)$se))
  for (kind in c("bjs_avg", "bjs_mid")) {
    x <- fit(kind, tr); num_est(paste0(k, "DistBJS", c(bjs_avg = "Avg", bjs_mid = "Mid")[[kind]]), x$estimate, x$se)
  }
  for (nm in c("Avg", "Mid")) {
    b <- breakdown[treatment == tr & target == nm, Mbar]
    num(paste0(k, "HonestBreak", nm), if (is.na(b)) "none" else sprintf("%.2f", b))   # none: interval includes zero at Mbar = 0
  }
  num(paste0(k, "DistCSWaveMinusTwo"), neg(f2(fit("cs_main", tr)$b[["-2"]])))
  num(paste0(k, "DistCSWaveMinusTwoSE"), f2(sqrt(fit("cs_main", tr)$V["-2", "-2"])))
  # overall effect (waves 0-8) relative to the national trend and to within-person variation
  num(paste0(k, "AvgTrendPct"), sprintf("%.0f", 100 * lc(fit("cs_main", tr), "Avg")$estimate / trend_change))
  num(paste0(k, "AvgWithinShare"), sprintf("%.0f", 100 * lc(fit("cs_main", tr), "Avg")$estimate / cache$sds[outcome == "distance", sd_within]))
  # event-study estimates for every wave after the loss (where the effect is strongest)
  for (h in 0:8) num(paste0(k, "DistCSWave", c("Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")[h + 1]),
                     neg(f2(fit("cs_main", tr)$b[[as.character(h)]])))
  # power: minimum detectable effect (80% power, 5% two-sided)
  for (nm in c("Avg", "Mid")) num(paste0(k, "MDE", nm), f2(2.8 * lc(fit("cs_main", tr), nm)$se))
  # gender difference (independent samples), waves 2-4 and 0-8
  for (nm in c("Mid", "Avg")) {
    m <- lc(fit("cs_main", tr, sm = "men"), nm); w <- lc(fit("cs_main", tr, sm = "women"), nm)
    num_est(paste0(k, "GenderDiff", nm), m$estimate - w$estimate, sqrt(m$se^2 + w$se^2))
  }
  for (sm in c("men", "women")) { x <- fit("bjs_avg", tr, sm = sm); num_est(paste0(k, "DistBJS", sm_key[[sm]], "Avg"), x$estimate, x$se) }
  for (y in c("generalized_trust", "red_overall_mean_distance", mech)) {
    x <- fit("bjs_avg", tr, y); num_est(paste0(k, y_key[[y]], "BJSAvg"), x$estimate, x$se, sd_y[[y]])
  }
}


# Numbers: anticipation, insecurity, alternative definitions ---------------------------------

plj <- dip$pr_lose[model == "change from <= t-5 (person and wave FE)"]
num("PrLoseTwo", sprintf("%.0f", plj[term == -2, estimate])); num("PrLoseOne", sprintf("%.0f", plj[term == -1, estimate]))
num("PrLoseMeanNever", sprintf("%.0f", dip$pr_lose_mean[group == "never unemployed", mean]))
num("PrLoseMeanLosers", sprintf("%.0f", dip$pr_lose_mean[group == "job losers, pre-treatment", mean]))
fear <- alt$cont[outcome == "distance"]
num("FearTen", neg(f2(10 * fear$estimate))); num("FearTenSE", f2(10 * fear$se))
num("FearTenHi", neg(f2(10 * (fear$estimate + 1.96 * fear$se))))
num("FearPersons", big(fear$n_persons))
ae <- function(k, e) alt$est[treat == k & outcome == "distance" & estimator == e &
                               contrast == if (e == "BJS") "waves 0-8 (imputation)" else "waves 0-8 vs -1"]
ie <- function(v, e) fu$inc_est[variant == v & outcome == "distance" & estimator == e &
                                   contrast == if (e == "BJS") "waves 0-8 (imputation)" else "waves 0-8 vs -1"]
range_num <- function(name, x) { num(paste0(name, "Min"), neg(f2(min(x$estimate)))); num(paste0(name, "Max"), neg(f2(max(x$estimate)))) }
for (k in c("insec50", "insec50_keep", "p10", "hardship", "endsmeet", "p25_emp", "hh10"))
  range_num(c(insec50 = "Insec", insec50_keep = "InsecKeep", p10 = "PTen", hardship = "Hardship", endsmeet = "EndsMeet",
              p25_emp = "IncKept", hh10 = "HhTen")[[k]], rbind(ae(k, "CS"), ae(k, "BJS")))
for (v in c("drop >= 20%", "drop >= 30%", "drop >= 50%", "drop >= 25%, without voluntary-looking drops", "drop >= 25%, without drops that may be measurement"))
  range_num(c(`drop >= 20%` = "IncTwenty", `drop >= 30%` = "IncThirty", `drop >= 50%` = "IncFifty",
              `drop >= 25%, without voluntary-looking drops` = "IncNoVol",
              `drop >= 25%, without drops that may be measurement` = "IncNoSuspect")[[v]], rbind(ie(v, "CS"), ie(v, "BJS")))
# Callaway-Sant'Anna estimate with SE (the text reports the main estimator; imputation in SI Table C1)
alt_key <- c(insec50 = "Insec", insec50_keep = "InsecKeep", p10 = "PTen", hardship = "Hardship", endsmeet = "EndsMeet",
             p25_emp = "IncKept", hh10 = "HhTen")
for (k in names(alt_key)) { x <- ae(k, "CS"); num_est(paste0(alt_key[[k]], "CS"), x$estimate, x$se) }
inc_key <- c(`drop >= 20%` = "IncTwenty", `drop >= 30%` = "IncThirty", `drop >= 50%` = "IncFifty",
             `drop >= 25%, without voluntary-looking drops` = "IncNoVol", `drop >= 25%, without drops that may be measurement` = "IncNoSuspect")
for (v in names(inc_key)) { x <- ie(v, "CS"); num_est(paste0(inc_key[[v]], "CS"), x$estimate, x$se) }
num("EndsMeetTreated", big(alt$info[treat == "endsmeet", treated]))
num("InsecTreated", big(alt$info[treat == "insec50", treated]))


# Numbers: periods, PVV, on/off status --------------------------------------------------

grp <- copy(fu$by_period[type == "group" & outcome == "distance"])
num("JobCohortEarlyMin", neg(f2(grp[treatment == "jobloss" & wave %in% 10:12, min(estimate)])))
num("JobCohortEarlyMax", neg(f2(grp[treatment == "jobloss" & wave %in% 10:12, max(estimate)])))
num("JobCohortEarlyN", grp[treatment == "jobloss" & wave %in% 10:12, sum(n_treated)])
num("JobCohortLaterMean", neg(f2(grp[treatment == "jobloss" & wave >= 13, weighted.mean(estimate, n_treated)])))
num("IncCohortPositive", grp[treatment == "pinc25", sum(estimate > 0)]); num("IncCohortN", grp[treatment == "pinc25", .N])
num("IncCohortTwentyThree", f2(grp[treatment == "pinc25" & wave == 23, estimate]))
pvv <- fu$pvv[outcome == "thermo_PVV" & sample == "all" & estimator %in% c("CS", "BJS") &
              contrast %in% c("waves 0-8 vs -1", "waves 0-8 (imputation)")]
for (tr in names(tr_lab)) range_num(paste0("Pvv", tr_key[[tr]]), pvv[treatment == tr])
# overlap of the two treatments
ov <- cache$overlap
num("OverlapJobInc", sprintf("%.0f", 100 * ov$both_n / ov$job_n)); num("OverlapJobIncSame", sprintf("%.0f", 100 * ov$same / ov$job_n))
num("OverlapIncJob", sprintf("%.0f", 100 * ov$both_n / ov$inc_n)); num("OverlapIncJobSame", sprintf("%.0f", 100 * ov$same / ov$inc_n))
num("OverlapBothN", big(ov$both_n))
# smoothness bounds: breakdown M (largest M at which the robust interval excludes zero) and the
# robust interval at M = 0 (linear difference in trends allowed)
for (tr in names(tr_lab)) for (nm in c("Avg", "Mid")) {
  b <- breakdown_sd[treatment == tr & target == nm, M]
  num(paste0(tr_key[[tr]], "HonestSDBreak", nm), if (is.na(b)) "none" else sprintf("%.3f", b))
  x <- honest_sd[treatment == tr & target == nm & M == 0]
  num(paste0(tr_key[[tr]], "HonestSDLo", nm), neg(f2(x$lb))); num(paste0(tr_key[[tr]], "HonestSDHi", nm), neg(f2(x$ub)))
}
# confidence in the government: first and last post-treatment waves with a significant decline
for (tr in names(tr_lab)) {
  f <- fit("cs_main", tr, "confidence_gov"); e <- es_dt(f)[term >= 0]
  sig <- e[estimate / se < -1.96, term]
  num(paste0(tr_key[[tr]], "ConfGovSigFirst"), if (length(sig)) min(sig) else "none")
  num(paste0(tr_key[[tr]], "ConfGovSigLast"), if (length(sig)) max(sig) else "none")
}


# Numbers: mechanisms (15_mechanisms.R) -------------------------------------------------------

# channels: Callaway-Sant'Anna averages over waves 0-1, 2-4 and 0-8 (SD = SD among the never treated)
ch_key <- c(fin_sat = "FinSat", hh_finsit = "HhFin", econ_sat = "EconSat", happy = "Happy", life_sat = "LifeSat",
            pol_interest = "Interest", news_int = "NewsInt", news_online = "NewsOnline", lr_extreme = "Extreme",
            adherent = "Adherent", alienated = "Alienated", no_influence = "NoInfluence", foreigners = "Foreigners",
            immig_adapt = "Adapt", redist = "Redist", connected = "Connected", gov_mean = "GovRating",
            main_gov = "MainGov", rate_SP = "RateSP", rate_PVV = "RatePVV", least_gov = "LeastGov",
            dist_opp = "DistOpp", dist2 = "DistSq", dist2_gov = "DistSqGov", dist2_opp = "DistSqOpp")
ch_sd <- unique(ch$summary[, .(outcome, sd)])
for (tr in names(tr_lab)) for (y in names(ch_key)) {
  f <- mfit(tr, y); sdy <- ch_sd[outcome == y, sd]
  for (nm in c("Early", "Mid", "Avg")) { x <- lc(f, nm); num_est(paste0(tr_key[[tr]], ch_key[[y]], nm), x$estimate, x$se, sdy) }
  num(paste0(tr_key[[tr]], ch_key[[y]], "PreP"), pval(pre_p(f)))
}
# share of the increase in squared distance that comes from the governing parties (delta-method SE
# from the person-level influence functions, 15_mechanisms.R), and benchmarks: their share of squared
# distance among the never treated (DistSqGovBasePct) and among the treated in the wave before the loss,
# and their first-order share sum(weight x gap) among the treated in the wave before the loss (the share
# of the increase if feelings toward all other parties cooled by the same amount)
for (tr in names(tr_lab)) {
  k <- tr_key[[tr]]
  for (nm in c("Avg", "Mid")) {
    x <- ch$gov_share[treatment == tr & window == nm]
    num(paste0(k, "DistSqGovShare", nm), sprintf("%.0f", 100 * x$share)); num(paste0(k, "DistSqGovShare", nm, "SE"), sprintf("%.0f", 100 * x$se))
  }
  x <- ch$bench[treatment == tr]
  num(paste0(k, "DistSqGovTreatedPct"), sprintf("%.0f", 100 * x$sq_treated)); num(paste0(k, "GapGovTreatedPct"), sprintf("%.0f", 100 * x$first_order))
}
num("DistSqGovBasePct", sprintf("%.0f", 100 * ch$gov_share_base))

# channels: sharpened FDR q-values (Benjamini, Krieger & Yekutieli 2006, as in Anderson 2008) within
# each loss for the averages over waves 2-4 of the outcomes in SI Figure G.1
sharpened_q <- function(p) {
  m <- length(p); o <- order(p); ps <- p[o]; qv <- rep(1, m)
  for (q in seq(1, 0.001, by = -0.001)) {
    q1 <- q / (1 + q)
    r1 <- max(c(0, which(ps <= seq_len(m) * q1 / m)))
    q2 <- q1 * m / (m - r1)
    r2 <- max(c(0, which(ps <= seq_len(m) * q2 / m)))
    if (r2 > 0) qv[o[seq_len(r2)]] <- q
  }
  qv
}

# horse race: effects of attitude-change events on distance (all 18-65), in SD of distance
hr_key <- c(extreme = "Extreme", adherent = "Adherent", interest = "Interest", online = "Online", distrust = "Distrust",
            alienated = "Alienated", foreigners = "Foreigners", finsat = "FinSat", econ = "Econ", happy = "Happy",
            isolated = "Isolated", pinc25 = "Inc", jobloss = "Job")
hs <- hr$summary
for (e in names(hr_key)) {
  x <- hs[event == e]; k <- paste0("Hr", hr_key[[e]])
  for (nm in c("Mid", "Avg")) num(paste0(k, nm, "SD"), neg(f2(x[window == nm, est_sd])))
  num(paste0(k, "SharePct"), sprintf("%.0f", 100 * x[window == "Avg", share_treated]))
  num(paste0(k, "PreP"), pval(x[window == "Avg", pre_p]))
}
num("HrFailPreN", n_word(hs[window == "Avg" & !event %in% names(tr_lab) & pre_p < 0.05, .N]))
num("HrEventsN", n_word(hs[window == "Avg" & !event %in% names(tr_lab), .N]))

# mediation: indirect effects (distance points) and their share of the total effect (with SE)
md_key <- c(confidence_index = "Conf", satisfaction_gov = "SatGov", alienated = "Alienated", fin_sat = "FinSat",
            econ_sat = "EconSat", happy = "Happy", foreigners = "Foreigners", redist = "Redist", lr_extreme = "Extreme",
            pol_interest = "Interest", connected = "Connected", adherent = "Adherent", joint = "Joint")
tm_key <- c(A = "A", B = "B", A2 = "Atwo")
ms <- md$summary
pct <- function(x) { v <- round(100 * x); neg(sprintf("%.0f", fifelse(v == 0, 0, v))) }   # no "-0"
for (r in split(ms[effect == "indirect"], by = c("treatment", "set", "timing", "ml"))) {
  sh <- ms[effect == "share" & treatment == r$treatment & set == r$set & timing == r$timing & ml == r$ml]
  k <- paste0("Med", tr_key[[r$treatment]], md_key[[r$set]], tm_key[[r$timing]], if (r$ml == "lasso") "Lasso" else "")
  num(k, neg(f3(r$estimate))); num(paste0(k, "SE"), f3(r$se))
  num(paste0(k, "Pct"), pct(sh$estimate)); num(paste0(k, "PctSE"), sprintf("%.0f", 100 * sh$se))
}
for (tr in names(tr_lab)) {
  x <- ms[effect == "total" & ml == "parametric" & timing %in% c("A", "B") & treatment == tr]
  num(paste0("Med", tr_key[[tr]], "TotalMin"), neg(f2(min(x$estimate)))); num(paste0("Med", tr_key[[tr]], "TotalMax"), neg(f2(max(x$estimate))))
  num(paste0("Med", tr_key[[tr]], "JointTreated"), big(x[set == "joint", min(n_treated)]))
}


# Figures ------------------------------------------------------------------------------

theme_paper <- function() theme_hanno(fontsize = 10) +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 8), panel.grid.minor = element_blank())
in_window <- function(x) x[term >= event_window[1] & term <= event_window[2]]
add_ref <- function(x) rbind(x, data.table(term = -1L, estimate = 0, se = NA_real_), fill = TRUE)
es_panel <- function(kind, sm = "all") rbindlist(lapply(names(tr_lab), function(tr)
  add_ref(es_dt(fit(kind, tr, sm = sm)))[, treatment_label := factor(tr_lab[[tr]], levels = tr_lab)]))
base_es <- function(p) p +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_vline(xintercept = -0.5, linetype = "dotted", linewidth = 0.3)

# critical value of a 95% sup-t band over the event times shown (simulated from the estimated covariance)
supt_crit <- function(V, draws = 1e5) {
  set.seed(14); R <- cov2cor(V); z <- matrix(rnorm(draws * ncol(R)), draws) %*% chol(R)
  unname(quantile(apply(abs(z), 1, max), 0.95))
}
# Figure 1: main event studies, with pointwise 95% and 90% intervals and the 95% sup-t band (grey)
f1 <- in_window(es_panel("cs_main"))
f1[, crit := supt_crit(fit("cs_main", names(tr_lab)[match(as.character(treatment_label), tr_lab)][1])$V), by = treatment_label]
p1 <- base_es(ggplot(f1, aes(term, estimate))) +
  geom_linerange(aes(ymin = estimate - crit * se, ymax = estimate + crit * se), color = "grey80", linewidth = 2.4, na.rm = TRUE) +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.35, na.rm = TRUE) +
  geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.8) + facet_wrap(~ treatment_label, ncol = 2) +
  scale_x_continuous(breaks = seq(event_window[1], event_window[2])) +
  labs(x = "Waves relative to the loss", y = "Effect on distance") + theme_paper()
ggsave(fig_path("fig1_event_study"), p1, width = 6.5, height = 2.8)

# Figure 2: men and women
f2d <- in_window(rbind(es_panel("cs_main", "men")[, sample := "Men"], es_panel("cs_main", "women")[, sample := "Women"]))
pos <- position_dodge(width = 0.5)
p2 <- base_es(ggplot(f2d, aes(term, estimate, shape = sample, color = sample))) +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.35, position = pos, na.rm = TRUE) +
  geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), linewidth = 0.9, position = pos, na.rm = TRUE) +
  geom_point(size = 1.8, position = pos, fill = "white") +
  scale_shape_manual(values = c(Men = 16, Women = 21)) + scale_color_manual(values = c(Men = "black", Women = "grey45")) +
  facet_wrap(~ treatment_label, ncol = 2) + scale_x_continuous(breaks = seq(event_window[1], event_window[2])) +
  labs(x = "Waves relative to the loss", y = "Effect on distance") + theme_paper()
ggsave(fig_path("fig2_gender"), p2, width = 6.5, height = 3.1)

# Figure 3: confidence in institutions (index)
es_panel_y <- function(y) rbindlist(lapply(names(tr_lab), function(tr)
  add_ref(es_dt(fit("cs_main", tr, y)))[, treatment_label := factor(tr_lab[[tr]], levels = tr_lab)]))
f3d <- in_window(es_panel_y("confidence_index"))
p3 <- base_es(ggplot(f3d, aes(term, estimate))) +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.35, na.rm = TRUE) +
  geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.8) + facet_wrap(~ treatment_label, ncol = 2) +
  scale_x_continuous(breaks = seq(event_window[1], event_window[2])) +
  labs(x = "Waves relative to the loss", y = "Effect on confidence\n(index, 0-10)") + theme_paper()
ggsave(fig_path("fig3_confidence"), p3, width = 6.5, height = 2.8)

# Figure B1: event studies under the alternative assumptions
bjs_es <- rbindlist(lapply(names(tr_lab), function(tr) fit("bjs_es", tr)[, treatment := tr]))
fb1 <- in_window(rbind(
  es_panel("cs_main")[, spec := "Never-treated comparison (main)"],
  es_panel("cs_notyet")[, spec := "Not-yet-treated comparison"],
  es_panel("cs_covars")[, spec := "Baseline covariates"],
  bjs_es[, treatment_label := factor(tr_lab[treatment], levels = tr_lab)][, spec := "Imputation (all pre-periods)"],
  fill = TRUE))
fb1[, spec := factor(spec, c("Never-treated comparison (main)", "Not-yet-treated comparison", "Baseline covariates", "Imputation (all pre-periods)"))]
pos4 <- position_dodge(width = 0.7)
pb1 <- base_es(ggplot(fb1, aes(term, estimate, shape = spec, color = spec))) +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.3, position = pos4, na.rm = TRUE) +
  geom_point(size = 1.4, position = pos4, fill = "white", na.rm = TRUE) +
  scale_shape_manual(values = c(16, 21, 17, 15)) + scale_color_manual(values = c("black", "grey30", "grey45", "grey60")) +
  facet_wrap(~ treatment_label, ncol = 1) + scale_x_continuous(breaks = seq(event_window[1], event_window[2])) +
  guides(shape = guide_legend(nrow = 2), color = guide_legend(nrow = 2)) +
  labs(x = "Waves relative to the loss", y = "Effect on distance") + theme_paper()
ggsave(fig_path("figB1_assumptions"), pb1, width = 6.5, height = 6)

# Figure B2: Rambachan-Roth bounds
fb2 <- copy(honest)[target == "Avg"][, treatment_label := factor(tr_lab[treatment], levels = tr_lab)]
pb2 <- ggplot(fb2[!is.na(Mbar)], aes(Mbar, ymin = lb, ymax = ub)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(linewidth = 0.6) +
  geom_linerange(data = fb2[is.na(Mbar)], aes(x = -0.15), color = "grey50", linewidth = 0.9) +
  facet_wrap(~ treatment_label) +
  labs(x = expression(bar(M) ~ "(post-treatment violations relative to the largest pre-treatment violation)"),
       y = "95% robust confidence interval, waves 0-8") + theme_paper()
ggsave(fig_path("figB2_honestdid"), pb2, width = 6.5, height = 2.8)

# Figure C1: anticipation of job loss
# the income module of wave w is fielded between the politics interviews of waves w and w + 1, so its
# event times -2 and -1 lie at -1.5 and -0.5 on the scale of the politics interviews
fc1 <- rbind(plj[, .(term = term + 0.5, estimate, se, panel = "Expected chance of losing one's job\n(percentage points, change within person)")],
             in_window(add_ref(es_dt(fit("cs_main", "jobloss"))))[term %in% -5:2,
               .(term, estimate, se, panel = "Affective polarization\n(distance)")])
fc1[, panel := factor(panel, unique(panel))]
pc1 <- ggplot(fc1, aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) + geom_vline(xintercept = -0.5, linetype = "dotted", linewidth = 0.3) +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.35, na.rm = TRUE) +
  geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.8) + facet_wrap(~ panel, ncol = 2, scales = "free_y") + scale_x_continuous(breaks = -5:2) +
  labs(x = "Waves relative to job loss", y = NULL) + theme_paper()
ggsave(fig_path("figC1_anticipation"), pc1, width = 6.5, height = 3)

# Figure F1: effects by the wave of the loss
grp[, `:=`(treatment_label = factor(tr_lab[treatment], levels = tr_lab), year = yr(wave))]
pf1 <- ggplot(grp, aes(year, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.35) +
  geom_point(aes(size = n_treated), shape = 21, fill = "grey35", color = "black") +
  scale_size_area(max_size = 3.5, name = "People treated") +
  facet_wrap(~ treatment_label, ncol = 1, scales = "free_x") +
  labs(x = "Wave of the loss (fieldwork December to March)", y = "Average effect on distance") +
  theme_paper() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_text())
ggsave(fig_path("figF1_cohorts"), pf1, width = 6.5, height = 5.2)

# Figure G1: channels (effects in SD of each outcome, average over waves 0-8). Party labels: family (name).
g1_lab <- c(fin_sat = "Satisfaction with own finances (0-10)", hh_finsit = "Household finances (1-5)",
            econ_sat = "Satisfaction with the economy (0-10)", happy = "Happiness (0-10)", life_sat = "Life satisfaction (0-10)",
            pol_interest = "Political interest (0-2)", news_int = "Interest in news (0-2)", news_online = "Follows news online (0/1)",
            lr_extreme = "Ideological extremity (0-5)", adherent = "Party adherent (0/1)",
            confidence_index = "Confidence in institutions (0-10)", alienated = "Parties only want my vote (0/1)",
            no_influence = "No influence on government (0/1)", generalized_trust = "Generalized trust (0-10)",
            foreigners = "Too many foreigners (1-5)", immig_adapt = "Immigrants should adapt (1-5)",
            redist = "Income differences should shrink (1-5)", connected = "Feels connected to others (1-7)",
            gov_mean = "Governing parties (mean rating)", rate_SP = "Radical-left (SP)", rate_PVV = "Far-right (PVV)",
            least_gov = "Least-liked party governs (0/1)")
g1 <- ch$summary[outcome %in% names(g1_lab) & window == "Avg"]
g1[, `:=`(label = factor(g1_lab[outcome], levels = rev(g1_lab)), group = factor(group, levels = unique(ch$spec$group)),
          treatment_label = factor(tr_lab[treatment], levels = tr_lab))]
posg <- position_dodge(width = 0.6)
pg1 <- ggplot(g1, aes(est_sd, label)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(aes(xmin = est_sd - 1.96 * se_sd, xmax = est_sd + 1.96 * se_sd), linewidth = 0.35) +
  geom_linerange(aes(xmin = est_sd - 1.645 * se_sd, xmax = est_sd + 1.645 * se_sd), linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_grid(group ~ treatment_label, scales = "free_y", space = "free_y", labeller = labeller(group = label_wrap_gen(14))) +
  labs(x = "Effect in standard deviations of the outcome", y = NULL) +
  theme_paper() + theme(strip.text.y = element_text(angle = 0, face = "bold"))
ggsave(fig_path("figG1_channels"), pg1, width = 6.5, height = 8)

# Figure G2: horse race (dagger: pre-treatment estimates jointly different from zero, p < 0.05)
g2 <- copy(hs)[window == "Avg"]
g2[, label := fifelse(!is.na(pre_p) & pre_p < 0.05, paste0(label, " †"), label)]
g2[, `:=`(label = factor(label, levels = g2[order(est_sd), label]),
          kind = factor(fifelse(event %in% names(tr_lab), "Economic loss (labor-force samples)", "Change in attitude or engagement (all aged 18-65)"),
                        c("Economic loss (labor-force samples)", "Change in attitude or engagement (all aged 18-65)")))]
pg2 <- ggplot(g2, aes(est_sd, label)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(aes(xmin = est_sd - 1.96 * se_sd, xmax = est_sd + 1.96 * se_sd), linewidth = 0.35) +
  geom_linerange(aes(xmin = est_sd - 1.645 * se_sd, xmax = est_sd + 1.645 * se_sd), linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_grid(kind ~ ., scales = "free_y", space = "free_y", labeller = labeller(kind = label_wrap_gen(16))) +
  labs(x = "Effect on affective polarization (distance), waves 0-8, standard deviations", y = NULL) +
  theme_paper() + theme(strip.text.y = element_text(angle = 0, face = "bold"))
ggsave(fig_path("figG2_horse"), pg2, width = 6.5, height = 4.6, device = cairo_pdf)   # cairo renders the dagger

# Figure G3: mediation, indirect effects on distance (points) with the mediator in waves 0-1 or 2-4
g3_lab <- c(satisfaction_gov = "Government blame\n(satisfaction with the government)",
            confidence_index = "Institutional distrust\n(confidence in institutions)",
            alienated = "Political alienation\n(parties only want my vote)", fin_sat = "Financial strain\n(satisfaction with own finances)",
            econ_sat = "Economic pessimism\n(satisfaction with the economy)", happy = "Emotional distress\n(happiness)",
            foreigners = "Outgroup threat\n(too many foreigners)", redist = "Redistribution demand\n(income differences should shrink)",
            lr_extreme = "Ideological extremity\n(distance from the center)", pol_interest = "Political engagement\n(political interest)",
            connected = "Social isolation\n(feels connected to others)", adherent = "Partisan identity\n(party adherent)",
            joint = "All jointly\n(except partisan identity)")
g3 <- ms[effect == "indirect" & ml == "parametric" & timing %in% c("A", "B")]
g3[, `:=`(label = factor(g3_lab[set], levels = rev(g3_lab)), treatment_label = factor(tr_lab[treatment], levels = tr_lab),
          timing = factor(timing, c("A", "B"), c("Mediator in waves 0-1", "Mediator in waves 2-4")))]
pg3 <- ggplot(g3, aes(estimate, label, shape = timing, color = timing)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), linewidth = 0.35, position = posg) +
  geom_linerange(aes(xmin = estimate - 1.645 * se, xmax = estimate + 1.645 * se), linewidth = 0.9, position = posg) +
  geom_point(size = 1.6, position = posg, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_color_manual(values = c("black", "grey45")) +
  facet_wrap(~ treatment_label) +
  labs(x = "Indirect effect on distance (points, 0-10 scale)", y = NULL) +
  theme_paper() + theme(axis.text.y = element_text(size = 7, lineheight = 0.85))
ggsave(fig_path("figG3_mediation"), pg3, width = 6.5, height = 6.5)


# Table 1: where the change in distance comes from ------------------------------------------

# panel A: distance and its components (this script's fits); panel B: governing and other parties
# (15_mechanisms.R; party ratings in waves 2007/08 to 2024/25, cabinet at the time of fieldwork)
t1_row <- function(y, label, get = function(tr, y) fit("cs_main", tr, y)) {
  cells <- lapply(names(tr_lab), function(tr) lc(get(tr, y), "Avg"))
  c(sprintf("%s & %s \\\\", label, paste(vapply(cells, function(x) cell(x$estimate, x$se), ""), collapse = " & ")),
    sprintf(" & %s \\\\", paste(vapply(cells, function(x) sprintf("(%s)", f2(x$se)), ""), collapse = " & ")))
}
t1_mech <- function(y, label) t1_row(y, label, get = mfit)
writeLines(c("\\begin{tabular}{lcc}", "\\toprule",
  "& Large income loss & Job loss \\\\", "\\midrule",
  "\\multicolumn{3}{l}{\\emph{A. Distance and its components}} \\\\",
  t1_row("distance", "Affective polarization (distance)"), t1_row("like_max", "Rating of most-liked party"),
  t1_row("like_min", "Rating of least-liked party"), t1_row("spread", "Spread of party ratings"),
  "\\midrule", "\\multicolumn{3}{l}{\\emph{B. Governing parties and other parties}} \\\\",
  t1_mech("gov_mean", "Mean rating of governing parties"), t1_mech("rate_SP", "Rating of Radical-left (SP)"),
  t1_mech("rate_PVV", "Rating of Far-right (PVV)"), t1_mech("least_gov", "Least-liked party governs (share)"),
  t1_mech("dist2", "Squared distance"), t1_mech("dist2_gov", "\\quad from governing parties"),
  t1_mech("dist2_opp", "\\quad from other parties"), t1_mech("dist_opp", "Distance among parties not in government"),
  "\\midrule",
  sprintf("People treated & %s & %s \\\\", big(fit("cs_main", "pinc25")$n_treated), big(fit("cs_main", "jobloss")$n_treated)),
  sprintf("People & %s & %s \\\\", big(fit("cs_main", "pinc25")$n_units), big(fit("cs_main", "jobloss")$n_units)),
  "\\bottomrule", "\\end{tabular}"), tab_path("tab1_components"))


# Table A1: samples -----------------------------------------------------------------------

dd <- function(tr, v) desc[treatment == tr][[v]]
writeLines(c("\\begin{tabular}{lcc}", "\\toprule", "& Large income loss & Job loss \\\\", "\\midrule",
  sprintf("People & %s & %s \\\\", big(dd("pinc25", "persons")), big(dd("jobloss", "persons"))),
  sprintf("Person-waves & %s & %s \\\\", big(dd("pinc25", "person_waves")), big(dd("jobloss", "person_waves"))),
  sprintf("Waves with the outcome & %d & %d \\\\", dd("pinc25", "waves"), dd("jobloss", "waves")),
  sprintf("People treated & %s & %s \\\\", big(dd("pinc25", "treated")), big(dd("jobloss", "treated"))),
  sprintf("\\quad of whom women (\\%%) & %.0f & %.0f \\\\", 100 * dd("pinc25", "treated_women") / dd("pinc25", "treated"),
          100 * dd("jobloss", "treated_women") / dd("jobloss", "treated")),
  sprintf("Waves in which losses occur & %d & %d \\\\", dd("pinc25", "cohorts"), dd("jobloss", "cohorts")),
  sprintf("Distance: mean (SD) & %s (%s) & %s (%s) \\\\", f2(dd("pinc25", "mean_d")), f2(dd("pinc25", "sd_d")),
          f2(dd("jobloss", "mean_d")), f2(dd("jobloss", "sd_d"))),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabA1_samples"))


# Table B1: identification checks, one per assumption ------------------------------------------

b_cells <- function(get) paste(vapply(names(tr_lab), function(tr) { x <- get(tr, "Avg"); sprintf("%s (%s)", cell(x$estimate, x$se), f2(x$se)) }, ""), collapse = " & ")
b_row <- function(label, get) sprintf("%s & %s \\\\", label, b_cells(get))
b_two <- function(label, f) sprintf("%s & %s \\\\", label, paste(vapply(names(tr_lab), f, ""), collapse = " & "))
writeLines(c("\\begin{tabular}{lcc}", "\\toprule",
  "Check (assumption it addresses) & Large income loss & Job loss \\\\", "\\midrule",
  b_row("Main specification", function(tr, nm) lc(fit("cs_main", tr), nm)),
  b_row("Not-yet-treated controls (comparison group)", function(tr, nm) lc(fit("cs_notyet", tr), nm)),
  b_row("Baseline covariates (selection on observables)", function(tr, nm) lc(fit("cs_covars", tr), nm)),
  b_row("\\quad Same sample without covariates", function(tr, nm) lc(fit("cs_cc", tr), nm)),
  b_row("Within sector and occupation (selection on observables)", function(tr, nm) ex_s$summary[treatment == tr & version == "matched" & window == nm, .(estimate, se)]),
  b_row("\\quad same people, not matched", function(tr, nm) ex_s$summary[treatment == tr & version == "unmatched" & window == nm, .(estimate, se)]),
  b_row("Waves $-5$ to $-3$ as baseline (no anticipation)", function(tr, nm) lc(fit("cs_main", tr), paste0("Clean", nm))),
  b_row("Same people before and after (no compositional change)", function(tr, nm) lc(fit("wp", tr), nm)),
  b_row("No household members of the treated (no spillovers)", function(tr, nm) lc(fit("cs_nohh", tr), nm)),
  b_row("Same people, weighted for attrition (selective exit)", function(tr, nm) ex_i$attrition[treatment == tr & version == "weighted" & window == nm, .(estimate, se)]),
  b_row("Imputation estimator (parallel trends in all waves)", function(tr, nm) fit("bjs_avg", tr)),
  "\\midrule",
  b_two("Joint pre-trend test, waves $-5$ to $-2$ ($p$)", function(tr) pval(pre_p(fit("cs_main", tr)))),
  b_two("Smallest pre-trend the test can exclude, waves $-5$ to $-2$", function(tr) f2(ex_i$equivalence[treatment == tr, delta])),
  b_two("Breakdown $\\bar{M}$, Rambachan--Roth (size of violations)", function(tr) {
    b <- breakdown[treatment == tr & target == "Avg", Mbar]; if (is.na(b)) "--" else sprintf("%.2f", b) }),
  b_two("Robust 95\\% interval, linear trend difference ($M = 0$)", function(tr) {
    x <- honest_sd[treatment == tr & target == "Avg" & M == 0]; sprintf("[%s, %s]", neg(f2(x$lb)), neg(f2(x$ub))) }),
  b_two("Breakdown $M$, smoothness (size of violations)", function(tr) {
    b <- breakdown_sd[treatment == tr & target == "Avg", M]; if (is.na(b)) "--" else sprintf("%.3f", b) }),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabB1_checks"))


# Table C1: alternative definitions of the loss and measures of insecurity -----------------------

two <- function(cs, bjs) sprintf("%s & %s", cell_se(cs$estimate, cs$se), cell_se(bjs$estimate, bjs$se))
inc_rows <- c(`drop >= 25% (main)` = "Personal income $-$25\\% or more (main)",
              `drop >= 25%, without voluntary-looking drops` = "\\quad excluding more children and exits from the labor force",
              `drop >= 25%, without drops that may be measurement` = "\\quad excluding possible measurement errors",
              `drop >= 20%` = "Personal income $-$20\\% or more", `drop >= 30%` = "Personal income $-$30\\% or more",
              `drop >= 50%` = "Personal income $-$50\\% or more",
              `self-reported wage drop >= 25%` = "Self-reported net wage $-$25\\% or more")
# upper panel: personal income losses (13_followups.R, and 12_alt_treatments.R for the last two rows);
# lower panel: household income, insecurity and hardship (12_alt_treatments.R)
inc_rows_alt <- c(p10 = "Personal income $-$10\\% or more", p25_emp = "Personal income $-$25\\% or more, job kept")
alt_rows <- c(hh10 = "Household income $-$10\\% or more", hh25 = "Household income $-$25\\% or more",
              insec50 = "Expected job-loss chance rises to 50\\% or more",
              insec50_keep = "\\quad and job kept for two waves",
              hardship = "Household starts using savings or debt", endsmeet = "First report of trouble making ends meet")
writeLines(c("\\begin{tabular}{lrcc}", "\\toprule", "Treatment & Treated & Callaway-Sant'Anna & Imputation \\\\", "\\midrule",
  vapply(names(inc_rows), function(v) sprintf("%s & %s & %s \\\\", inc_rows[[v]], big(info[variant == v, treated]), two(ie(v, "CS"), ie(v, "BJS"))), ""),
  vapply(names(inc_rows_alt), function(k) sprintf("%s & %s & %s \\\\", inc_rows_alt[[k]], big(alt$info[treat == k, treated]), two(ae(k, "CS"), ae(k, "BJS"))), ""),
  "\\midrule",
  vapply(names(alt_rows), function(k) sprintf("%s & %s & %s \\\\", alt_rows[[k]], big(alt$info[treat == k, treated]), two(ae(k, "CS"), ae(k, "BJS"))), ""),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabC1_treatments"))


# Table D1: men and women ------------------------------------------------------------------

d_row <- function(tr, sm) {
  f <- fit("cs_main", tr, sm = sm); bj <- fit("bjs_avg", tr, sm = sm)
  sprintf("\\quad %s & %s & %s \\\\", c(men = "Men", women = "Women")[[sm]],
          paste(vapply(c("Early", "Mid", "Late", "Avg"), function(nm) { x <- lc(f, nm); cell_se(x$estimate, x$se) }, ""), collapse = " & "),
          cell_se(bj$estimate, bj$se))
}
d_diff <- function(tr) {
  cells <- vapply(c("Early", "Mid", "Late", "Avg"), function(nm) {
    m <- lc(fit("cs_main", tr, sm = "men"), nm); w <- lc(fit("cs_main", tr, sm = "women"), nm)
    cell_se(m$estimate - w$estimate, sqrt(m$se^2 + w$se^2)) }, "")
  bm <- fit("bjs_avg", tr, sm = "men"); bw <- fit("bjs_avg", tr, sm = "women")
  sprintf("\\quad Difference & %s & %s \\\\", paste(cells, collapse = " & "), cell_se(bm$estimate - bw$estimate, sqrt(bm$se^2 + bw$se^2)))
}
writeLines(c("\\begin{tabular}{lccccc}", "\\toprule",
  "& \\multicolumn{4}{c}{Callaway-Sant'Anna} & Imputation \\\\", "\\cmidrule(lr){2-5} \\cmidrule(lr){6-6}",
  "& Waves 0--1 & Waves 2--4 & Waves 5--8 & Waves 0--8 & Waves 0--8 \\\\", "\\midrule",
  "Large income loss & & & & & \\\\", d_row("pinc25", "men"), d_row("pinc25", "women"), d_diff("pinc25"),
  "Job loss & & & & & \\\\", d_row("jobloss", "men"), d_row("jobloss", "women"), d_diff("jobloss"),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabD1_gender"))


# Table E1: outcomes that do not change ------------------------------------------------------

# three decimals for outcomes on small scales (heterophily, 0-1)
cell_se3 <- function(e, s) sprintf("%s%s (%s)", neg(f3(e)), fifelse(stars(e, s) == "", "", paste0("$^{", stars(e, s), "}$")), f3(s))
e_row <- function(label, get_cs, get_bjs, digits = 2) sprintf("%s & %s \\\\", label, paste(unlist(lapply(names(tr_lab), function(tr) {
  cs <- get_cs(tr); bj <- get_bjs(tr); fn <- if (digits == 3) cell_se3 else cell_se
  c(fn(cs$estimate, cs$se), fn(bj$estimate, bj$se)) })), collapse = " & "))
pvv_get <- function(tr, e) { x <- pvv[treatment == tr & estimator == e]; list(estimate = x$estimate, se = x$se) }
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{Large income loss} & \\multicolumn{2}{c}{Job loss} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "& Callaway-Sant'Anna & Imputation & Callaway-Sant'Anna & Imputation \\\\", "\\midrule",
  e_row("Generalized trust (0--10)", function(tr) lc(fit("cs_main", tr, "generalized_trust"), "Avg"), function(tr) fit("bjs_avg", tr, "generalized_trust")),
  e_row("Heterophily of close contacts (0--1)", function(tr) lc(fit("cs_main", tr, "red_overall_mean_distance"), "Avg"), function(tr) fit("bjs_avg", tr, "red_overall_mean_distance"), digits = 3),
  e_row("Rating of Far-right (PVV) (0--10)", function(tr) pvv_get(tr, "CS"), function(tr) pvv_get(tr, "BJS")),
  "\\midrule",
  "\\multicolumn{5}{l}{\\emph{Confidence and satisfaction (0--10)}} \\\\",
  e_row("\\quad Confidence index", function(tr) lc(fit("cs_main", tr, "confidence_index"), "Avg"), function(tr) fit("bjs_avg", tr, "confidence_index")),
  e_row("\\quad Confidence in the government", function(tr) lc(fit("cs_main", tr, "confidence_gov"), "Avg"), function(tr) fit("bjs_avg", tr, "confidence_gov")),
  e_row("\\quad Satisfaction with the government", function(tr) lc(fit("cs_main", tr, "satisfaction_gov"), "Avg"), function(tr) fit("bjs_avg", tr, "satisfaction_gov")),
  e_row("\\quad Satisfaction with democracy", function(tr) lc(fit("cs_main", tr, "satisfaction_dem"), "Avg"), function(tr) fit("bjs_avg", tr, "satisfaction_dem")),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabE1_nulls"))
for (tr in names(tr_lab)) for (y in c("generalized_trust", "red_overall_mean_distance", mech)) {
  x <- lc(fit("cs_main", tr, y), "Avg")
  num(paste0(tr_key[[tr]], y_key[[y]], "AvgSDabs"), f2(abs(x$estimate) / sd_y[[y]]))
  num(paste0(tr_key[[tr]], y_key[[y]], "HiSD"), f2(max(abs(x$estimate + c(-1, 1) * 1.96 * x$se)) / sd_y[[y]]))
}


# Table G2: mediation (indirect effects and their share of the total effect) --------------------------

# cells: indirect effect with stars; (SE) and share of the total effect in the same sample [share (SE)]
g_cell <- function(r, sh) {
  if (!nrow(r)) return(c("", ""))
  c(sprintf("%s%s", neg(f3(r$estimate)), fifelse(stars(r$estimate, r$se) == "", "", paste0("$^{", stars(r$estimate, r$se), "}$"))),
    sprintf("(%s) [%s\\%% (%s)]", f3(r$se), pct(sh$estimate), sprintf("%.0f", 100 * sh$se)))
}
g_row <- function(st, label, learner = "parametric", timings = c("A", "B")) {
  cells <- unlist(lapply(names(tr_lab), function(tr) lapply(timings, function(tm) {
    sel <- function(e) ms[effect == e & treatment == tr & set == st & timing == tm & ml == learner]
    g_cell(sel("indirect"), sel("share"))
  })), recursive = FALSE)
  vapply(1:2, function(i) sprintf("%s & %s \\\\", if (i == 1) label else "", paste(vapply(cells, `[`, "", i), collapse = " & ")), "")
}
g_lab <- c(satisfaction_gov = "Government blame", confidence_index = "Institutional distrust", alienated = "Political alienation",
           fin_sat = "Financial strain", econ_sat = "Economic pessimism", happy = "Emotional distress", foreigners = "Outgroup threat",
           redist = "Redistribution demand", lr_extreme = "Ideological extremity", pol_interest = "Political engagement",
           connected = "Social isolation", adherent = "Partisan identity")   # measures: SI table note
tot_range <- function(tr, tm) { x <- ms[effect == "total" & treatment == tr & timing == tm & ml == "parametric"]; sprintf("%s--%s", neg(f2(min(x$estimate))), neg(f2(max(x$estimate)))) }
between_meds <- md$spec[module == "between", mediator]
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{Large income loss} & \\multicolumn{2}{c}{Job loss} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Mechanism (mediator) & Waves 0--1 & Waves 2--4 & Waves 0--1 & Waves 2--4 \\\\", "\\midrule",
  g_row("joint", "All jointly, except partisan identity"), g_row("joint", "\\quad with lasso nuisance models", learner = "lasso"),
  "\\midrule",
  unlist(lapply(names(g_lab), function(m) g_row(m, g_lab[[m]]))),
  "\\midrule",
  "\\multicolumn{5}{l}{\\emph{Items fielded between politics interviews, modules around the loss (waves 0--1 only)}} \\\\",
  unlist(lapply(c("joint", between_meds), function(m) {
    rows <- g_row(m, if (m == "joint") "All jointly, except partisan identity" else g_lab[[m]], timings = "A2")
    # one timing per loss: put the cells in the waves 0-1 columns
    sub("^(.*?) & (.*?) & (.*?) \\\\\\\\$", "\\1 & \\2 & & \\3 & \\\\\\\\", rows)
  })),
  "\\midrule",
  sprintf("Total effect (range across mediator samples) & %s \\\\", paste(c(tot_range("pinc25", "A"), tot_range("pinc25", "B"), tot_range("jobloss", "A"), tot_range("jobloss", "B")), collapse = " & ")),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabG2_mediation"))

# Table G1: channels, averages over waves 0-8 in SD of each outcome, with pre-trend tests and sharpened
# q-values within each loss
g2t <- ch$summary[outcome %in% names(g1_lab) & window == "Avg"]
g2t[, q := sharpened_q(2 * pnorm(-abs(estimate / se))), by = treatment]
g2_cell <- function(tr, y) { x <- g2t[treatment == tr & outcome == y]
  sprintf("%s & %s & %s", cell_se(x$est_sd, x$se_sd), pval(x$pre_p), sprintf("%.2f", x$q)) }
writeLines(c("\\begin{tabular}{lcccccc}", "\\toprule",
  "& \\multicolumn{3}{c}{Large income loss} & \\multicolumn{3}{c}{Job loss} \\\\", "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Outcome & Effect (SE) & Pre-trend $p$ & $q$ & Effect (SE) & Pre-trend $p$ & $q$ \\\\", "\\midrule",
  vapply(names(g1_lab), function(y) sprintf("%s & %s & %s \\\\", gsub("([0-9])-([0-9])", "\\1--\\2", g1_lab[[y]]), g2_cell("pinc25", y), g2_cell("jobloss", y)), ""),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabG1_channels"))
num("ChannelsQSigInc", g2t[treatment == "pinc25" & q < 0.05, .N]); num("ChannelsQSigJob", g2t[treatment == "jobloss" & q < 0.05, .N])
num("ChannelsN", uniqueN(g2t$outcome))


# Extensions (17_extensions.R): Tables C2, G3 and H1, and their numbers -------------------------
# All estimates are Callaway-Sant'Anna averages over waves 2-4 (and 0-8 in C2), except the stacked
# within-person cells of 17 (party ratings by cabinet status, groups by intended vote in wave -1).
ex_cell <- function(x, scale = 1) if (!nrow(x)) "& " else sprintf("%s & %s", cell_se(scale * x$estimate, scale * x$se), pval(x$pre_p))
ex_cell_sd <- function(x) if (!nrow(x)) "& " else sprintf("%s & %s", cell_se(x$est_sd, x$se_sd), pval(x$pre_p))
two_cols <- function(get) paste(get("pinc25"), get("jobloss"), sep = " & ")
ex_head <- function(unit) c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{Large income loss} & \\multicolumn{2}{c}{Job loss} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  sprintf("%s & Effect (SE) & Pre-trend $p$ & Effect (SE) & Pre-trend $p$ \\\\", unit), "\\midrule")
panel <- function(title) sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", title)

# Table H1: alternative measures and scale use (SD of each measure, waves 2-4)
h1_lab <- list(
  "Affective polarization" = c(distance = "Distance (main measure)", dist_const_eq = "Distance, same six parties in every wave",
    dist_const_fix = "\\quad with fixed vote-share weights", spread_const_eq = "Spread, same six parties",
    lead_dist = "Distance between party leaders", lead_spread = "Spread of party-leader ratings",
    partisan_affect = "Affective polarization index, partisans only"),
  "Party ratings and use of the scale" = c(max_rating = "Rating of the most-liked party", min_rating = "Rating of the least-liked party",
    mean_rating = "Mean party rating", n_low = "Number of parties rated 2 or lower", n_rated = "Number of parties rated"),
  "Confidence in six non-political institutions (0--10)" = c(anchor_max = "Highest-rated institution", anchor_min = "Lowest-rated institution",
    anchor_mean = "Mean", anchor_sd = "Spread (standard deviation across the six)", inst_media = "The media", inst_military = "The military",
    inst_police = "The police", inst_science = "Science", inst_healthcare = "Health care", inst_edusys = "The education system"))
writeLines(c(ex_head("Measure"),
  unlist(lapply(names(h1_lab), function(pn) c(if (pn != names(h1_lab)[1]) "\\midrule", panel(pn),
    vapply(names(h1_lab[[pn]]), function(y) sprintf("%s & %s \\\\", h1_lab[[pn]][[y]],
      two_cols(function(tr) ex_cell_sd(ex_m[treatment == tr & outcome == y & window == "Avg"]))), "")))),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabH1_measures"))

# Table C2: size of the income drop, reason for the job loss, unemployment benefit, partner's loss
c2_lab <- list(
  "Size of the first drop in personal income of 10\\% or more" = c(dose_10_25 = "10 to 25\\%", dose_25_50 = "25 to 50\\%", dose_50 = "50\\% or more"),
  "Job loss, by the reason for leaving the job (Work and Schooling)" = c(reason_closure = "Company closure or reorganization",
    reason_dismissal = "End of contract or other dismissal", reason_other = "Other reason or none recorded"),
  "Job loss, by unemployment benefit (WW) in the year of the loss or the next" = c(ww_yes = "Receives WW", ww_no = "Does not receive WW"),
  "The partner's loss (own job not lost in the same wave)" = c(partner_job = "Partner loses job", partner_inc = "Partner's income $-$25\\% or more"))
c2_cell <- function(k) { x <- ex_t$summary[treatment == k]; ce <- function(nm) cell_se(x[window == nm, estimate], x[window == nm, se])
  sprintf("%s & %s & %s & %s", big(x$n_treated[1]), ce("Avg"), ce("CleanAvg"), pval(x$pre_p[1])) }
writeLines(c("\\begin{tabular}{lrccc}", "\\toprule", "& & \\multicolumn{2}{c}{Average effect, waves 0--8} & \\\\",
  "\\cmidrule(lr){3-4}", "Treatment & Treated & Baseline wave $-1$ & Baseline waves $-5$ to $-3$ & Pre-trend $p$ \\\\", "\\midrule",
  unlist(lapply(names(c2_lab), function(pn) c(if (pn != names(c2_lab)[1]) "\\midrule", sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", pn),
    vapply(names(c2_lab[[pn]]), function(k) sprintf("%s & %s \\\\", c2_lab[[pn]][[k]], c2_cell(k)), "")))),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabC2_treatments"))

# Table G3: incumbent blame (waves 2-4)
gb <- function(tr, an, y, grp, nm = "Avg") ex_b[treatment == tr & analysis == an & outcome == y & group == grp & window == nm]
gm <- function(tr, y) ex_m[treatment == tr & outcome == y & window == "Avg"]
vg <- c(governing = "a governing party", `other party` = "another party", `none named` = "no party named")
vote_rows <- function(y, label, groups = vg) c(sprintf("%s: & & & & \\\\", label), vapply(names(groups), function(g)
  sprintf("\\quad %s & %s \\\\", groups[[g]], two_cols(function(tr) ex_cell(gb(tr, "by vote in wave -1", y, g)))), ""))
cs_rows <- function(lab, scale = 1, sd = FALSE) vapply(names(lab), function(y) sprintf("%s & %s \\\\", lab[[y]], two_cols(function(tr) {
  x <- ex_b[treatment == tr & analysis == "CS" & outcome == y & window == "Avg"]; if (sd) ex_cell_sd(x) else ex_cell(x, scale) })), "")
g3_rows <- c(
  panel("Party ratings by whether the party governs (0--10)"),
  sprintf("Parties while they govern & %s \\\\", two_cols(function(tr) ex_cell(gb(tr, "party rating by status, all", "party rating", "governing")))),
  sprintf("\\quad the same parties while not governing & %s \\\\", two_cols(function(tr) ex_cell(gb(tr, "party rating by status, switchers", "party rating", "not governing")))),
  sprintf("All parties while not governing & %s \\\\", two_cols(function(tr) ex_cell(gb(tr, "party rating by status, all", "party rating", "not governing")))),
  sprintf("Prime minister & %s \\\\", two_cols(function(tr) ex_cell(gm(tr, "pm_rating")))),
  sprintf("Prime minister minus opposition leaders & %s \\\\", two_cols(function(tr) ex_cell(gm(tr, "pm_minus_opp")))),
  "\\midrule", panel("By the party the person intended to vote for in wave $-1$"),
  vote_rows("distance", "Distance"), vote_rows("gov_mean", "Mean rating of the governing parties"),
  vote_rows("own_party", "Rating of that party", groups = vg[1:2]),
  "\\midrule", panel("Vote intention and protest (percentage points)"),
  cs_rows(c(intend_gov = "Would vote for a governing party", intend_PVV = "Would vote Far-right (PVV)", intend_SP = "Would vote Radical-left (SP)",
            intend_none = "Names no party", protest = "Took part in a protest action, march or demonstration"), scale = 100),
  "\\midrule", panel("Emotions and self-regard (standard deviations)"),
  cs_rows(c(anger = "Anger (hostile, irritable)", anxiety = "Anxiety (afraid, nervous)", self_esteem = "Self-esteem", loneliness = "Loneliness"), sd = TRUE))
writeLines(c(ex_head("Outcome"), g3_rows, "\\bottomrule", "\\end{tabular}"), tab_path("tabG3_blame"))

# numbers for the text (num_est: distance outcomes, with SD of distance; num_pe: other outcomes, in points)
num_pe <- function(name, e, s) { num(name, neg(f2(e))); num(paste0(name, "SE"), f2(s)) }
inst_key <- c(anchor_min = "InstMin", anchor_max = "InstMax", inst_media = "InstMedia", anchor_sd = "InstSpread", pm_rating = "PM")
status_key <- c(governing = "Gov", `not governing` = "Opp")
pp_key <- c(intend_gov = "IntendGov", protest = "Protest")
for (tr in names(tr_lab)) {
  k <- tr_key[[tr]]
  mx <- function(y) ex_m[treatment == tr & outcome == y & window == "Avg"]
  num_est(paste0(k, "ConstAvg"), mx("dist_const_eq")$estimate, mx("dist_const_eq")$se)
  for (y in names(inst_key)) num_pe(paste0(k, inst_key[[y]], "Avg"), mx(y)$estimate, mx(y)$se)
  num(paste0(k, "InstSpreadAvgSD"), neg(f2(mx("anchor_sd")$est_sd))); num(paste0(k, "InstMinPreP"), pval(mx("anchor_min")$pre_p))
  for (st in names(status_key)) {
    x <- gb(tr, "party rating by status, switchers", "party rating", st); num_pe(paste0(k, "Switch", status_key[[st]], "Avg"), x$estimate, x$se)
  }
  for (y in names(pp_key)) {      # percentage points
    x <- ex_b[treatment == tr & analysis == "CS" & outcome == y & window == "Avg"]; nm <- paste0(k, pp_key[[y]])
    num(paste0(nm, "Avg"), neg(sprintf("%.1f", 100 * x$estimate))); num(paste0(nm, "AvgSE"), sprintf("%.1f", 100 * x$se)); num(paste0(nm, "PreP"), pval(x$pre_p))
  }
  x <- ex_i$attrition[treatment == tr & version == "weighted" & window == "Avg"]; num_est(paste0(k, "IPWAvg"), x$estimate, x$se)
  x <- ex_i$equivalence[treatment == tr]; num(paste0(k, "EquivDelta"), f2(x$delta)); num(paste0(k, "EquivDeltaSD"), f2(x$delta_sd))
}
trt_key <- c(dose_10_25 = "DoseLow", dose_25_50 = "DoseMid", dose_50 = "DoseHigh", reason_closure = "Closure", reason_dismissal = "Dismissal",
             reason_other = "ReasonOther", ww_yes = "WWYes", ww_no = "WWNo", partner_job = "PartnerJob", partner_inc = "PartnerInc")
for (key in names(trt_key)) {
  x <- ex_t$summary[treatment == key]; k <- trt_key[[key]]
  for (nm in c("Avg", "CleanAvg")) num_est(paste0(k, nm), x[window == nm, estimate], x[window == nm, se])
  num(paste0(k, "Treated"), big(x$n_treated[1])); num(paste0(k, "PreP"), pval(x$pre_p[1]))
}
num("WWSharePct", sprintf("%.0f", 100 * ex_t$ww_share))
num("SwitchN", n_word(length(ex_bl$switchers)))
# range of the overall effect across the checks that keep the main comparison (not-yet-treated
# controls, same people, within sector and occupation, no household members, attrition weights,
# imputation estimator)
for (tr in names(tr_lab)) {
  x <- c(lc(fit("cs_notyet", tr), "Avg")$estimate, lc(fit("wp", tr), "Avg")$estimate, lc(fit("cs_nohh", tr), "Avg")$estimate,
         ex_s$summary[treatment == tr & version == "matched" & window == "Avg", estimate],
         ex_i$attrition[treatment == tr & version == "weighted" & window == "Avg", estimate], fit("bjs_avg", tr)$estimate)
  num(paste0(tr_key[[tr]], "ChecksMin"), neg(f2(min(x)))); num(paste0(tr_key[[tr]], "ChecksMax"), neg(f2(max(x))))
}
# effect of currently being unemployed (on/off coding, fect; 19_onoff.R)
oo <- readRDS(here("results", "estimates", "apsr", "onoff.rds"))
num_est("OnOffAtt", oo$att$estimate, oo$att$se); num("OnOffPreP", pval(oo$pre_p)); num("OnOffTreated", big(oo$n_treated))
num_est("OnOffExitOne", oo$off[term == 1, estimate], oo$off[term == 1, se])
num_est("OnOffEnterZero", oo$es[term == 0, estimate], oo$es[term == 0, se])
sel_key <- c(matched = "Sector", unmatched = "SectorUnmatched")
for (tr in names(tr_lab)) {    # selection on sector and occupation (within-person, matched cells)
  k <- tr_key[[tr]]
  for (v in names(sel_key)) for (nm in "Avg") {
    x <- ex_s$summary[treatment == tr & version == v & window == nm]; num_est(paste0(k, sel_key[[v]], nm), x$estimate, x$se)
  }
  cv <- ex_s$coverage[treatment == tr]; num(paste0(k, "SectorCoveragePct"), sprintf("%.0f", 100 * cv$treated_with_cell / cv$treated_all))
}
rs <- ex_t$reasons[, .(N = sum(N)), by = reason_grp]
num("ReasonClosureN", big(rs[reason_grp == "closure", N])); num("ReasonDismissalN", big(rs[reason_grp == "dismissal", N]))
num("ReasonAnyPct", sprintf("%.0f", 100 * ex_t$reasons[!is.na(reason), sum(N)] / sum(rs$N)))


# Table I1: validation with LISS study 233, "Affective polarization" (18_ap_validation.R) ----------
va <- readRDS(here("results", "estimates", "ap_study", "validation.rds"))
ci_cell <- function(r, lo, hi) sprintf("%s [%s, %s]", neg(f2(r)), neg(f2(lo)), neg(f2(hi)))
va_cell <- function(xv, yv, yr) { z <- va$cross[x == xv & y == yv & year == yr]
  if (!nrow(z)) "-- & " else sprintf("%s & %s", ci_cell(z$r, z$lo, z$hi), big(z$n)) }
va_row <- function(xv, yv, label) sprintf("%s & %s & %s \\\\", label, va_cell(xv, yv, "2019"), va_cell(xv, yv, "2021"))
ch_row <- function(xv, yv, label) { z <- va$changes[x == xv & y == yv]
  sprintf("%s & \\multicolumn{4}{c}{%s, $n$ = %s} \\\\", label, ci_cell(z$r, z$lo, z$hi), big(z$n)) }
vs_ch <- va$slopes[model == "change: voter (eight parties) on the note's distance"]
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{August 2019} & \\multicolumn{2}{c}{July 2021} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Correlation of our distance with & $r$ [95\\% CI] & $n$ & $r$ [95\\% CI] & $n$ \\\\", "\\midrule",
  "\\multicolumn{5}{l}{\\emph{Feelings toward voters and groups}} \\\\",
  va_row("p_dist", "v_dist", "Distance computed from feelings toward each party's voters"),
  va_row("p_dist_same", "v_dist", "\\quad our distance over the same parties"),
  va_row("p_dist", "v_min", "Feelings toward the least-liked party's voters"),
  va_row("p_dist", "issue_ap", "Gap between opposing issue and ideological groups"),
  va_row("p_dist", "social_spread", "Spread of feelings toward social groups"),
  "\\midrule", "\\multicolumn{5}{l}{\\emph{Social distance and hostility}} \\\\",
  va_row("p_dist", "social_distance", "Would mind a child marrying a voter of the least-liked party"),
  va_row("p_dist", "hate", "Has come to hate people for their opinions"),
  va_row("p_dist", "intolerance", "Intolerance of the least-liked party's followers"),
  "\\midrule", "\\multicolumn{5}{l}{\\emph{Benchmarks}} \\\\",
  va_row("p_dist_before", "p_dist_after", "Our distance in the politics waves before and after"),
  va_row("v_dist", "social_distance", "Voter-based distance and minding the marriage"),
  va_row("v_dist", "hate", "Voter-based distance and hatred"),
  "\\midrule", "\\multicolumn{5}{l}{\\emph{Changes from 2019 to 2021, people in both waves}} \\\\",
  ch_row("dp", "dv", "Change in voter-based distance (eight parties)"),
  sprintf("\\quad slope (SE); corrected for unreliability & \\multicolumn{4}{c}{%s (%s); %s} \\\\", f2(vs_ch$b), f2(vs_ch$se), f2(vs_ch$b_corrected)),
  ch_row("dp", "dsd", "Change in minding the marriage"),
  ch_row("dp", "dhate", "Change in hatred"),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabI1_validation"))
# numbers
vx <- function(xv, yv) va$cross[x == xv & y == yv, r]
for (k in list(c("ValVoter", "p_dist", "v_dist"), c("ValRetest", "p_dist_before", "p_dist_after"), c("ValSocial", "p_dist", "social_distance"),
               c("ValHate", "p_dist", "hate"), c("ValVoterSocial", "v_dist", "social_distance"), c("ValVoterHate", "v_dist", "hate"))) {
  num(paste0(k[1], "Min"), f2(min(vx(k[2], k[3])))); num(paste0(k[1], "Max"), f2(max(vx(k[2], k[3]))))
}
num("ValIntolerance", neg(f2(vx("p_dist", "intolerance")))); num("ValSocialSpread", f2(vx("p_dist", "social_spread")))
num("ValPartialRaw", f2(va$partial$r_raw)); num("ValPartial", f2(va$partial$r_partial))
num("ValNOne", big(va$n[year == "2019", study])); num("ValNTwo", big(va$n[year == "2021", study])); num("ValInBoth", big(va$n$in_both[1]))
num("ValChangeR", f2(va$changes[x == "dp" & y == "dv", r])); num("ValChangeN", big(va$changes[x == "dp" & y == "dv", n]))
num("ValChangeSlope", f2(vs_ch$b)); num("ValChangeSlopeCorr", f2(vs_ch$b_corrected)); num("ValRelChange", f2(va$reliability$rel_change_avg))
num("ValLevelSlope", f2(va$slopes[model == "levels: voter on the note's distance (both years)", b]))
inc_avg <- lc(fit("cs_main", "pinc25"), "Avg")$estimate    # overall effect, waves 0-8
num("ValImpliedLo", f2(inc_avg * vs_ch$b)); num("ValImpliedHi", f2(inc_avg * vs_ch$b_corrected))
num("ValLossInc", n_word(va$losses[treatment == "pinc25", between])); num("ValLossJob", n_word(va$losses[treatment == "jobloss", between]))


# numbers.tex ----------------------------------------------------------------------------------

writeLines(c("% Generated by code/analyze_did/14_apsr_note.R in the code repository; do not edit by hand.",
             sprintf("\\newcommand{\\%s}{%s\\xspace}", names(nums), unlist(nums))),
           file.path(out_dir, "numbers.tex"))
message("Wrote ", length(nums), " numbers, figures and tables to ", out_dir)

### END
