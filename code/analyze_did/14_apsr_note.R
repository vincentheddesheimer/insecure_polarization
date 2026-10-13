### APSR research note: figures, tables and in-text numbers (October 2026)
# Writes to the Overleaf project (Dropbox: Apps/Overleaf/HB_insecure_polarization/apsr/):
# figures/*.pdf, tables/*.tex and numbers.tex, which defines every number in the text as a macro.
#
# Design (the note's research design section): Callaway-Sant'Anna, never-treated comparison group,
# wave -1 as baseline, no covariates. One robustness check per identifying assumption:
#   comparison group           -> not-yet-treated people as controls (CS)
#   selection on observables   -> baseline covariates, doubly robust (CS)
#   no anticipation            -> waves -5 to -3 as the baseline (CS contrasts)
#   parallel trends, all waves -> imputation estimator (Borusyak, Jaravel & Spiess), more efficient
#                                  if trends are parallel in every pre-treatment wave
#   size of violations         -> Rambachan-Roth bounds (HonestDiD): relative magnitudes and smoothness
# Mechanism outcomes (00_setup.R mechanism_labels): confidence in institutions, satisfaction.
# Everything here is computed from the current panels, except the results of 10_dip.R,
# 12_alt_treatments.R and 13_followups.R, which must be rerun on the same data first.
# CS is refitted here with analytic (influence-function) standard errors, so that averages over
# waves have their full covariance; BJS window averages use didimputation's weights. Refits are
# cached in results/estimates/apsr/ (only missing jobs run); DID_APSR_REFIT=1 redoes all of them.
# Treatments: large personal income loss (pinc25) and job loss (jobloss), on equal footing.

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


# Saved results of 10, 12 and 13 (rerun on the current data) -------------------------

fu  <- readRDS(here("results", "estimates", "followups", "followups.rds"))
alt <- readRDS(here("results", "estimates", "alt", "alt.rds"))
dip <- readRDS(here("results", "estimates", "dip", "dip.rds"))


# Refits ----------------------------------------------------------------------

cs_fit2 <- function(d, y, control = "nevertreated", xformla = NULL) {
  d <- as.data.frame(prep_outcome(d, y))
  if (!is.null(xformla)) d <- d[complete.cases(d[, baseline_covs]), ]
  a <- did::att_gt(yname = y, tname = "t", idname = "id", gname = "G", data = d, xformla = xformla,
                   control_group = control, base_period = "universal", allow_unbalanced_panel = TRUE,
                   est_method = "dr", clustervars = "id", bstrap = FALSE)
  es <- did::aggte(a, type = "dynamic", min_e = event_window[1], max_e = event_window[2], na.rm = TRUE, bstrap = FALSE)
  keep <- es$egt != -1
  inf <- es$inf.function$dynamic.inf.func.e[, keep, drop = FALSE]
  V <- crossprod(inf) / nrow(inf)^2
  b <- setNames(es$att.egt[keep], es$egt[keep]); dimnames(V) <- list(names(b), names(b))
  stopifnot(all(abs(sqrt(diag(V)) / es$se.egt[keep] - 1) < 0.01))
  list(b = b, V = V, n_treated = length(unique(d$id[d$G > 0])), n_units = length(unique(d$id)))
}
# imputation estimator, average over the treated observations in waves h after the loss
bjs_window <- function(d, y, h) {
  d <- as.data.frame(prep_outcome(d, y))
  w <- as.numeric(d$D == 1 & (d$t - d$G) %in% h); d$w_win <- w / sum(w)
  ov <- didimputation::did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id",
                                      wtr = "w_win", cluster_var = "id")
  list(estimate = ov$estimate[1], se = ov$std.error[1], n_treated = length(unique(d$id[d$G > 0])))
}
# imputation event study, waves -5..8 (pre-treatment estimates from its pre-trend regression)
bjs_es <- function(d, y) {
  d <- as.data.frame(prep_outcome(d, y))
  es <- didimputation::did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id",
                                      horizon = 0:event_window[2], pretrends = event_window[1]:-1, cluster_var = "id")
  data.table(term = as.integer(es$term), estimate = es$estimate, se = es$std.error)
}

jobs <- rbind(
  CJ(kind = "cs_main", treatment = names(tr_lab), sample = c("all", "men", "women"), outcome = "distance"),
  CJ(kind = "cs_main", treatment = names(tr_lab), sample = "all",
     outcome = c("like_max", "like_min", "spread", "generalized_trust", "red_overall_mean_distance")),
  CJ(kind = c("cs_notyet", "cs_covars"), treatment = names(tr_lab), sample = "all", outcome = "distance"),
  CJ(kind = c("bjs_avg", "bjs_mid"), treatment = names(tr_lab), sample = "all", outcome = "distance"),
  CJ(kind = "bjs_avg", treatment = names(tr_lab), sample = c("men", "women"), outcome = "distance"),
  CJ(kind = "bjs_avg", treatment = names(tr_lab), sample = "all", outcome = c("generalized_trust", "red_overall_mean_distance", mech)),
  CJ(kind = "cs_main", treatment = names(tr_lab), sample = "all", outcome = mech),
  CJ(kind = "bjs_es", treatment = names(tr_lab), sample = "all", outcome = "distance"))
jobs[, key := paste(kind, treatment, sample, outcome, sep = "|")]

path_cache <- path_est("apsr", "refits")
cache <- if (file.exists(path_cache) && Sys.getenv("DID_APSR_REFIT") != "1") readRDS(path_cache) else list()
todo <- jobs[!key %in% names(cache)]
if (nrow(todo) || is.null(cache$desc) || is.null(cache$overlap)) {
  panels <- load_panels()
  for (i in seq_len(nrow(todo))) {
    j <- todo[i]; d <- get_panel(panels, j$treatment, j$sample)
    message("fit ", j$key)
    cache[[j$key]] <- switch(j$kind,
      cs_main   = cs_fit2(d, j$outcome),
      cs_notyet = cs_fit2(d, j$outcome, control = "notyettreated"),
      cs_covars = cs_fit2(d, j$outcome, xformla = xformla_base),
      bjs_avg   = bjs_window(d, j$outcome, 0:8),
      bjs_mid   = bjs_window(d, j$outcome, 2:4),
      bjs_es    = bjs_es(d, j$outcome))
    saveRDS(cache, path_cache)
  }
  # estimation samples (distance)
  cache$desc <- rbindlist(lapply(names(tr_lab), function(tr) {
    d <- prep_outcome(panels[[tr]], "distance")
    data.table(treatment = tr, persons = uniqueN(d$id), person_waves = nrow(d), waves = uniqueN(d$t_cal),
               treated = uniqueN(d[G > 0, id]), treated_women = uniqueN(d[G > 0 & male_base == 0, id]),
               cohorts = uniqueN(d[G > 0, G]), mean_d = mean(d$distance), sd_d = sd(d$distance))
  }))
  # outcome means and SDs among the never unemployed (job-loss panel), as in 11_power.R
  cache$sds <- rbindlist(lapply(c(names(y_key)), function(y) {
    d <- prep_outcome(panels$jobloss, y)[G == 0]
    d[, dev := get(y) - mean(get(y)), by = id]
    data.table(outcome = y, sd_pw = sd(d[[y]]), sd_within = sd(d[, if (.N > 1) .(dev = dev), by = id]$dev), mean = mean(d[[y]]))
  }))
  # overlap of the two treatments (calendar wave of the first loss)
  gj <- unique(panels$jobloss[G > 0, .(id, Gj = G)]); gi <- unique(panels$pinc25[G > 0, .(id, Gi = G)])
  both <- merge(gj, gi, by = "id")
  cache$overlap <- data.table(job_n = nrow(gj), inc_n = nrow(gi), both_n = nrow(both),
                              job_inc_near = both[(Gi - Gj) %in% 0:1, .N], inc_job_same = both[Gi == Gj, .N])
  saveRDS(cache, path_cache)
}
desc <- cache$desc
sd_y <- setNames(cache$sds$sd_pw, cache$sds$outcome)
sd_d <- sd_y[["distance"]]
fit <- function(kind, tr, y = "distance", sm = "all") cache[[paste(kind, tr, sm, y, sep = "|")]]


# averages over windows of event times; each window averages the event times the fit has (the
# covariate-adjusted job-loss fit has no estimate for wave 8, so its 0-8 average covers 0-7)
windows <- list(Avg = list(0:8), Early = list(0:1), Mid = list(2:4), Late = list(5:8),
                CleanAvg = list(0:8, -5:-3), CleanMid = list(2:4, -5:-3), MidLate = list(2:4, 5:8))
lc <- function(f, nm) {
  avail <- function(h) intersect(as.character(h), names(f$b))
  p <- avail(windows[[nm]][[1]]); w <- ev(p, 1 / length(p))
  if (length(windows[[nm]]) > 1) { n <- avail(windows[[nm]][[2]]); w <- c(w, ev(n, -1 / length(n))) }
  l <- lincom(f$b, f$V, w); list(estimate = l[["estimate"]], se = l[["se"]])
}
pre_p <- function(f) {
  k <- intersect(as.character(-5:-2), names(f$b))
  w <- drop(t(f$b[k]) %*% solve(f$V[k, k]) %*% f$b[k]); pchisq(w, length(k), lower.tail = FALSE)
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
                                                                numPostPeriods = npost, Mbarvec = seq(0, 2, 0.25), l_vec = l)
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
num("IncVolPct", sprintf("%.0f", 100 * (1 - info[variant == "drop >= 25%, without voluntary-looking drops", treated] /
                                         info[variant == "drop >= 25% (main)", treated])))
num("IncNoVolTreated", big(info[variant == "drop >= 25%, without voluntary-looking drops", treated]))
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
num("TrendFirst", f2(trend[1, mean])); num("TrendLast", f2(trend[.N, mean]))
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
  for (kind in c("cs_notyet", "cs_covars")) {
    f <- fit(kind, tr); s <- c(cs_notyet = "NY", cs_covars = "Cov")[[kind]]
    for (nm in c("Avg", "Mid")) { x <- lc(f, nm); num_est(paste0(k, "Dist", s, nm), x$estimate, x$se) }
    num(paste0(k, "Dist", s, "PreP"), pval(pre_p(f)))
  }
  for (kind in c("bjs_avg", "bjs_mid")) {
    x <- fit(kind, tr); num_est(paste0(k, "DistBJS", c(bjs_avg = "Avg", bjs_mid = "Mid")[[kind]]), x$estimate, x$se)
  }
  for (nm in c("Avg", "Mid")) {
    b <- breakdown[treatment == tr & target == nm, Mbar]
    num(paste0(k, "HonestBreak", nm), if (is.na(b)) "0" else sprintf("%.2f", b))
  }
  for (h in 2:4) num(paste0(k, "DistCSWave", c("Two", "Three", "Four")[h - 1]), neg(f2(fit("cs_main", tr)$b[[as.character(h)]])))
  for (h in -2) num(paste0(k, "DistCSWaveMinusTwo"), neg(f2(fit("cs_main", tr)$b[["-2"]])))
  # medium-run effect relative to the national trend and to within-person variation
  num(paste0(k, "MidTrendPct"), sprintf("%.0f", 100 * lc(fit("cs_main", tr), "Mid")$estimate / trend_change))
  num(paste0(k, "MidWithinShare"), sprintf("%.0f", 100 * lc(fit("cs_main", tr), "Mid")$estimate / cache$sds[outcome == "distance", sd_within]))
  # power: minimum detectable effect (80% power, 5% two-sided)
  for (nm in c("Avg", "Mid")) num(paste0(k, "MDE", nm), f2(2.8 * lc(fit("cs_main", tr), nm)$se))
  # gender difference in the medium run (independent samples)
  m <- lc(fit("cs_main", tr, sm = "men"), "Mid"); w <- lc(fit("cs_main", tr, sm = "women"), "Mid")
  num_est(paste0(k, "GenderDiffMid"), m$estimate - w$estimate, sqrt(m$se^2 + w$se^2))
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
num("FearTen", neg(f3(10 * fear$estimate))); num("FearTenSE", f3(10 * fear$se))
num("FearTenHi", f3(10 * (fear$estimate + 1.96 * fear$se)))
num("FearPersons", big(fear$n_persons))
ae <- function(k, e) alt$est[treat == k & outcome == "distance" & estimator == e &
                               contrast == if (e == "BJS") "waves 0-8 (imputation)" else "waves 0-8 vs -1"]
ie <- function(v, e) fu$inc_est[variant == v & outcome == "distance" & estimator == e &
                                   contrast == if (e == "BJS") "waves 0-8 (imputation)" else "waves 0-8 vs -1"]
range_num <- function(name, x) { num(paste0(name, "Min"), neg(f2(min(x$estimate)))); num(paste0(name, "Max"), neg(f2(max(x$estimate)))) }
for (k in c("insec50", "insec50_keep", "p10", "hardship", "endsmeet", "p25_emp", "hh10"))
  range_num(c(insec50 = "Insec", insec50_keep = "InsecKeep", p10 = "PTen", hardship = "Hardship", endsmeet = "EndsMeet",
              p25_emp = "IncKept", hh10 = "HhTen")[[k]], rbind(ae(k, "CS"), ae(k, "BJS")))
for (v in c("drop >= 20%", "drop >= 30%", "drop >= 50%", "drop >= 25%, without voluntary-looking drops"))
  range_num(c(`drop >= 20%` = "IncTwenty", `drop >= 30%` = "IncThirty", `drop >= 50%` = "IncFifty",
              `drop >= 25%, without voluntary-looking drops` = "IncNoVol")[[v]], rbind(ie(v, "CS"), ie(v, "BJS")))
num("EndsMeetTreated", big(alt$info[treat == "endsmeet", treated]))
num("InsecTreated", big(alt$info[treat == "insec50", treated]))


# Numbers: periods, PVV, on/off status --------------------------------------------------

grp <- copy(fu$by_period[type == "group" & outcome == "distance"])
num("JobCohortEarlyMin", f2(grp[treatment == "jobloss" & wave %in% 10:12, min(estimate)]))
num("JobCohortEarlyMax", f2(grp[treatment == "jobloss" & wave %in% 10:12, max(estimate)]))
num("JobCohortEarlyN", grp[treatment == "jobloss" & wave %in% 10:12, sum(n_treated)])
num("JobCohortLaterMean", neg(f2(grp[treatment == "jobloss" & wave >= 13, weighted.mean(estimate, n_treated)])))
num("IncCohortPositive", grp[treatment == "pinc25", sum(estimate > 0)]); num("IncCohortN", grp[treatment == "pinc25", .N])
num("IncCohortTwentyThree", f2(grp[treatment == "pinc25" & wave == 23, estimate]))
pvv <- fu$pvv[outcome == "thermo_PVV" & sample == "all" & estimator %in% c("CS", "BJS") &
              contrast %in% c("waves 0-8 vs -1", "waves 0-8 (imputation)")]
for (tr in names(tr_lab)) range_num(paste0("Pvv", tr_key[[tr]]), pvv[treatment == tr])
# overlap of the two treatments
ov <- cache$overlap
num("OverlapJobInc", sprintf("%.0f", 100 * ov$both_n / ov$job_n)); num("OverlapJobIncNear", sprintf("%.0f", 100 * ov$job_inc_near / ov$job_n))
num("OverlapIncJob", sprintf("%.0f", 100 * ov$both_n / ov$inc_n)); num("OverlapIncJobSame", sprintf("%.0f", 100 * ov$inc_job_same / ov$inc_n))
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


# Figures ------------------------------------------------------------------------------

theme_paper <- function() theme_hanno(fontsize = 10) +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 8), panel.grid.minor = element_blank())
in_window <- function(x) x[term >= event_window[1] & term <= event_window[2]]
add_ref <- function(x) rbind(x, data.table(term = -1L, estimate = 0, se = NA_real_), fill = TRUE)
es_panel <- function(kind, sm = "all") rbindlist(lapply(names(tr_lab), function(tr)
  add_ref(es_dt(fit(kind, tr, sm = sm)))[, treatment_label := factor(tr_lab[[tr]], levels = tr_lab)]))
base_es <- function(p) p +
  annotate("rect", xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_vline(xintercept = -0.5, linetype = "dotted", linewidth = 0.3)

# Figure 1: main event studies
f1 <- in_window(es_panel("cs_main"))
p1 <- base_es(ggplot(f1, aes(term, estimate))) +
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
fb2 <- copy(honest)[, `:=`(treatment_label = factor(tr_lab[treatment], levels = tr_lab),
                          target_label = factor(c(Avg = "Average, waves 0-8", Mid = "Average, waves 2-4")[target],
                                                levels = c("Average, waves 2-4", "Average, waves 0-8")))]
pb2 <- ggplot(fb2[!is.na(Mbar)], aes(Mbar, ymin = lb, ymax = ub)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(linewidth = 0.6) +
  geom_linerange(data = fb2[is.na(Mbar)], aes(x = -0.15), color = "grey50", linewidth = 0.9) +
  facet_grid(target_label ~ treatment_label) +
  labs(x = expression(bar(M) ~ "(post-treatment violations relative to the largest pre-treatment violation)"),
       y = "95% robust confidence interval") + theme_paper()
ggsave(fig_path("figB2_honestdid"), pb2, width = 6.5, height = 4.2)

# Figure C1: anticipation of job loss
fc1 <- rbind(plj[, .(term, estimate, se, panel = "Expected chance of losing one's job\n(percentage points, change within person)")],
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


# Table 1: where the change in distance comes from ------------------------------------------

t1_row <- function(y, label) {
  cells <- unlist(lapply(names(tr_lab), function(tr) lapply(c("Avg", "Mid"), function(nm) lc(fit("cs_main", tr, y), nm))), recursive = FALSE)
  c(sprintf("%s & %s \\\\", label, paste(vapply(cells, function(x) cell(x$estimate, x$se), ""), collapse = " & ")),
    sprintf(" & %s \\\\", paste(vapply(cells, function(x) sprintf("(%s)", f2(x$se)), ""), collapse = " & ")))
}
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{Large income loss} & \\multicolumn{2}{c}{Job loss} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "& Waves 0--8 & Waves 2--4 & Waves 0--8 & Waves 2--4 \\\\", "\\midrule",
  t1_row("distance", "Affective polarization (distance)"), t1_row("like_max", "Rating of most-liked party"),
  t1_row("like_min", "Rating of least-liked party"), t1_row("spread", "Spread of party ratings"), "\\midrule",
  sprintf("People treated & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\", big(fit("cs_main", "pinc25")$n_treated), big(fit("cs_main", "jobloss")$n_treated)),
  sprintf("People & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\", big(fit("cs_main", "pinc25")$n_units), big(fit("cs_main", "jobloss")$n_units)),
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

b_cells <- function(get) paste(unlist(lapply(names(tr_lab), function(tr) lapply(c("Avg", "Mid"), function(nm) {
  x <- get(tr, nm); sprintf("%s (%s)", cell(x$estimate, x$se), f2(x$se)) }))), collapse = " & ")
b_row <- function(label, get) sprintf("%s & %s \\\\", label, b_cells(get))
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{Large income loss} & \\multicolumn{2}{c}{Job loss} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Check (assumption it addresses) & Waves 0--8 & Waves 2--4 & Waves 0--8 & Waves 2--4 \\\\", "\\midrule",
  b_row("Main specification", function(tr, nm) lc(fit("cs_main", tr), nm)),
  b_row("Not-yet-treated controls (comparison group)", function(tr, nm) lc(fit("cs_notyet", tr), nm)),
  b_row("Baseline covariates (selection on observables)", function(tr, nm) lc(fit("cs_covars", tr), nm)),
  b_row("Waves $-5$ to $-3$ as baseline (no anticipation)", function(tr, nm) lc(fit("cs_main", tr), paste0("Clean", nm))),
  b_row("Imputation estimator (parallel trends in all waves)", function(tr, nm) fit(c(Avg = "bjs_avg", Mid = "bjs_mid")[[nm]], tr)),
  "\\midrule",
  sprintf("Joint pre-trend test, waves $-5$ to $-2$ ($p$) & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
          pval(pre_p(fit("cs_main", "pinc25"))), pval(pre_p(fit("cs_main", "jobloss")))),
  sprintf("Breakdown $\\bar{M}$, Rambachan--Roth (size of violations) & %s \\\\",
          paste(unlist(lapply(names(tr_lab), function(tr) lapply(c("Avg", "Mid"), function(nm) {
            b <- breakdown[treatment == tr & target == nm, Mbar]; if (is.na(b)) "--" else sprintf("%.2f", b) }))), collapse = " & ")),
  sprintf("Breakdown $M$, smoothness (size of violations) & %s \\\\",
          paste(unlist(lapply(names(tr_lab), function(tr) lapply(c("Avg", "Mid"), function(nm) {
            b <- breakdown_sd[treatment == tr & target == nm, M]; if (is.na(b)) "--" else sprintf("%.3f", b) }))), collapse = " & ")),
  "\\bottomrule", "\\end{tabular}"), tab_path("tabB1_checks"))


# Table C1: alternative definitions of the loss and measures of insecurity -----------------------

two <- function(cs, bjs) sprintf("%s & %s", cell_se(cs$estimate, cs$se), cell_se(bjs$estimate, bjs$se))
inc_rows <- c(`drop >= 25% (main)` = "Personal income $-$25\\% or more (main)",
              `drop >= 25%, without voluntary-looking drops` = "\\quad excluding births and exits from the labor force",
              `drop >= 20%` = "Personal income $-$20\\% or more", `drop >= 30%` = "Personal income $-$30\\% or more",
              `drop >= 50%` = "Personal income $-$50\\% or more",
              `self-reported wage drop >= 25%` = "Self-reported net wage $-$25\\% or more")
alt_rows <- c(p10 = "Personal income $-$10\\% or more", p25_emp = "Personal income $-$25\\% or more, job kept",
              hh10 = "Household income $-$10\\% or more", hh25 = "Household income $-$25\\% or more",
              insec50 = "Expected job-loss chance rises to 50\\% or more",
              insec50_keep = "\\quad and job kept for two waves",
              hardship = "Household starts using savings or debt", endsmeet = "First report of trouble making ends meet")
writeLines(c("\\begin{tabular}{lrcc}", "\\toprule", "Treatment & Treated & Callaway-Sant'Anna & Imputation \\\\", "\\midrule",
  vapply(names(inc_rows), function(v) sprintf("%s & %s & %s \\\\", inc_rows[[v]], big(info[variant == v, treated]), two(ie(v, "CS"), ie(v, "BJS"))), ""),
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

e_row <- function(label, get_cs, get_bjs) sprintf("%s & %s \\\\", label, paste(unlist(lapply(names(tr_lab), function(tr) {
  cs <- get_cs(tr); bj <- get_bjs(tr); c(cell_se(cs$estimate, cs$se), cell_se(bj$estimate, bj$se)) })), collapse = " & "))
pvv_get <- function(tr, e) { x <- pvv[treatment == tr & estimator == e]; list(estimate = x$estimate, se = x$se) }
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
  "& \\multicolumn{2}{c}{Large income loss} & \\multicolumn{2}{c}{Job loss} \\\\", "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "& Callaway-Sant'Anna & Imputation & Callaway-Sant'Anna & Imputation \\\\", "\\midrule",
  e_row("Generalized trust (0--10)", function(tr) lc(fit("cs_main", tr, "generalized_trust"), "Avg"), function(tr) fit("bjs_avg", tr, "generalized_trust")),
  e_row("Heterophily of close contacts (0--1)", function(tr) lc(fit("cs_main", tr, "red_overall_mean_distance"), "Avg"), function(tr) fit("bjs_avg", tr, "red_overall_mean_distance")),
  e_row("Rating of the PVV (0--10)", function(tr) pvv_get(tr, "CS"), function(tr) pvv_get(tr, "BJS")),
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


# numbers.tex ----------------------------------------------------------------------------------

writeLines(c("% Generated by code/analyze_did/14_apsr_note.R in the code repository; do not edit by hand.",
             sprintf("\\newcommand{\\%s}{%s\\xspace}", names(nums), unlist(nums))),
           file.path(out_dir, "numbers.tex"))
message("Wrote ", length(nums), " numbers, figures and tables to ", out_dir)

### END
