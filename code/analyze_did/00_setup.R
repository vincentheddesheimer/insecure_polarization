### New DiD analysis: shared setup
# Sourced at the top of every script in code/analyze_did/. Design: SPEC.md.
#
# Test mode (one outcome, few bootstrap draws): Sys.setenv(DID_TEST = "1")
# Restrict outcomes:                            Sys.setenv(DID_OUTCOMES = "distance,spread")
# Estimation only (e.g. cluster array jobs):    Sys.setenv(DID_NO_FIGURES = "1")
# Figures only, from saved estimates:           Sys.setenv(DID_FIGURES_ONLY = "1")
# Known-answer check only (checks.R):           Sys.setenv(DID_CHECK_ONLY = "1")
# Restrict treatments (files tagged):          Sys.setenv(DID_TREATMENTS = "pinc25")

pacman::p_load(tidyverse, data.table, here)
here::i_am("code/analyze_did/00_setup.R")

test_mode    <- Sys.getenv("DID_TEST") == "1"
do_estimate  <- Sys.getenv("DID_FIGURES_ONLY") != "1"
do_figures   <- Sys.getenv("DID_NO_FIGURES") != "1"
check_only   <- Sys.getenv("DID_CHECK_ONLY") == "1"
if (check_only) do_estimate <- do_figures <- FALSE
run_checks   <- do_estimate || check_only                # every estimation run starts with its check
# haschaR (theme_hanno) only for figures: it needs ImageMagick, which Della lacks; on Della only
# estimates are computed (DID_NO_FIGURES = 1) and figures are drawn locally from them
if (do_figures) pacman::p_load(haschaR)
n_cores      <- if (Sys.getenv("SLURM_CPUS_PER_TASK") != "") as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) else
  max(1, min(6, parallel::detectCores() - 2))
# keep multithreaded packages within the allocated cores (on a shared cluster node they would
# otherwise start one thread per core of the node); polars: POLARS_MAX_THREADS in the job script
setDTthreads(n_cores)
if (requireNamespace("fixest", quietly = TRUE)) fixest::setFixest_nthreads(n_cores)


# Design constants --------------------------------------------------------

event_window <- c(-5, 8)

outcome_labels <- c(
  distance                        = "Affective polarization (distance)",
  spread                          = "Affective polarization (spread)",
  partisan_affect                 = "Partisan affect",
  like_max                        = "Ingroup affinity (like max)",
  like_min                        = "Outgroup aversion (like min)",
  generalized_trust               = "Generalized trust",
  red_overall_mean_distance       = "Heterophily (overall)",
  red_distance_age_rel_mean       = "Heterophily: age",
  red_distance_gender_mean        = "Heterophily: gender",
  red_distance_education_rel_mean = "Heterophily: education",
  red_distance_ethnicity_rel_mean = "Heterophily: ethnicity"
)
outcomes <- names(outcome_labels)
# mechanism outcomes for the APSR note (14_apsr_note.R): confidence in institutions and satisfaction,
# 0-10 (politics module). Kept in the panels but not in the estimator scripts' outcome loops.
mechanism_labels <- c(
  confidence_index = "Confidence in government, parliament, politicians and parties (mean)",
  confidence_gov   = "Confidence in the government",
  satisfaction_gov = "Satisfaction with the government",
  satisfaction_dem = "Satisfaction with democracy"
)
if (Sys.getenv("DID_OUTCOMES") != "") outcomes <- intersect(strsplit(Sys.getenv("DID_OUTCOMES"), ",")[[1]], outcomes)
if (test_mode) outcomes <- "distance"

# treatments to run (all if unset); results of a restricted run are saved in files tagged with the
# treatments, so they never overwrite the results of other runs
treatments_run <- if (Sys.getenv("DID_TREATMENTS") != "") strsplit(Sys.getenv("DID_TREATMENTS"), ",")[[1]] else NULL
run_tr  <- function(tr) is.null(treatments_run) || tr %in% treatments_run
run_tag <- if (is.null(treatments_run)) "" else paste(treatments_run, collapse = "-")

# baseline covariates (pre-treatment values, constant within person)
baseline_covs <- c("agegrp_base", "male_base", "educ_base", "hhsize_base", "inccat_base")
xformla_base  <- ~ agegrp_base + male_base + educ_base + hhsize_base + inccat_base


# Paths ---------------------------------------------------------------------

path_panels <- here("data", "analysis", "panels.rds")
path_est <- function(estimator, name) {
  d <- here("results", "estimates", estimator); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, paste0(name, if (test_mode) "_TEST", ".rds"))
}
path_fig <- function(name) {
  d <- here("results", "figures"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, paste0(name, if (test_mode) "_TEST", ".pdf"))
}
path_tab <- function(name) {
  d <- here("results", "tables"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, paste0(name, if (test_mode) "_TEST", ".tex"))
}

# Estimates are saved as one file per outcome (results/estimates/<est>/<est>__<outcome>[__<tag>].rds),
# so runs for different outcomes (or, with a tag, different parts of one outcome), e.g. parallel
# cluster jobs, never overwrite each other. load_est() stops if two files hold the same estimates.
save_est <- function(res, estimator, tag = run_tag) {
  for (y in outcomes) {
    part <- lapply(res, function(x) if (is.data.frame(x) && "outcome" %in% names(x)) x[outcome == y] else x)
    # skip outcomes without results (e.g. checkpoints before an outcome has run) so that earlier
    # complete results are never overwritten by empty ones
    if (!any(vapply(part, function(x) is.data.frame(x) && nrow(x) > 0, logical(1)))) next
    saveRDS(part, path_est(estimator, paste0(estimator, "__", y, if (nzchar(tag)) paste0("__", tag))))
  }
}
load_est <- function(estimator) {
  f <- list.files(here("results", "estimates", estimator), full.names = TRUE,
                  pattern = paste0("^", estimator, "__.*", if (test_mode) "_TEST", "\\.rds$"))
  if (!test_mode) f <- f[!grepl("_TEST\\.rds$", f)]
  if (!length(f)) return(NULL)
  parts <- lapply(f, readRDS)
  keys <- unique(unlist(lapply(parts, names)))
  out <- setNames(lapply(keys, function(k) rbindlist(lapply(parts, `[[`, k), fill = TRUE)), keys)
  if (!is.null(out$tidy) && anyDuplicated(out$tidy, by = c("estimator", "spec", "treatment", "sample", "outcome", "term")))
    stop("load_est: the same estimates appear in more than one file in results/estimates/", estimator,
         " (e.g. a run with and without DID_FECT_METHODS); remove the stale files")
  out
}


# Panels ------------------------------------------------------------------
# Built by 01_sample.R. Each panel: id, t (= wave - 7), outcomes, baseline covariates and
#   G : first treated period (0 = never treated)   [absorbing panels]
#   D : current treatment status                    [all panels]

load_panels <- function() {
  if (check_only) return(NULL)
  if (!file.exists(path_panels)) stop("Run code/analyze_did/01_sample.R first.")
  readRDS(path_panels)
}

# Outcome-specific panel. Drops person-waves without the outcome and numbers the waves in which
# the outcome was fielded consecutively, so event time counts fielded waves. Example: the politics
# module was not fielded in wave 15, so for the politics outcomes wave 16 directly follows wave 14
# and a job loss in wave 16 has wave 14 as its reference wave (-1). G moves to the first fielded
# wave at or after the event; events after the last fielded wave are untreated within this
# outcome's data (G = 0). t_cal keeps the calendar index (wave - 7). D is unchanged; for absorbing
# panels t_cal >= G_cal holds exactly when t >= G after renumbering, which is checked.
prep_outcome <- function(d, y, absorbing = TRUE) {
  keep <- !is.na(d[[y]])            # outside [ ]: a column named like y must not mask it
  d <- copy(d)[keep]
  waves <- sort(unique(d$t))
  d[, t_cal := t][, t := match(t_cal, waves)]
  g_old <- sort(unique(d[G > 0, G]))
  g_new <- vapply(g_old, function(g) { w <- which(waves >= g); if (length(w)) w[1] else 0L }, integer(1))
  d[G > 0, G := g_new[match(G, g_old)]]
  if (absorbing && !identical(d$D, as.integer(d$G > 0 & d$t >= d$G)))
    stop("prep_outcome: treatment indicator inconsistent after renumbering waves")
  d[]
}

# Balanced panel, done before calling att_gt(): keep people observed in every wave that has data,
# which is what did's own balancing does (BMisc::makeBalancedPanel after dropping missing outcomes).
# did 2.5.1's own balancing stops with "Internal error: treated group <g> not found in cohort_vec"
# when it removes an entire cohort. check_cs_balancing() (checks.R) verifies that pre-balancing gives identical
# estimates to did's balancing whenever the latter works.
balance_panel <- function(d) {
  n_waves <- uniqueN(d$t)
  d[, if (uniqueN(t) == n_waves) .SD, by = id]
}

# Sun-Abraham term with the event-time ends binned (<= event_window[1] - 1, >= event_window[2] + 1);
# d needs G_sa (cohort; never treated = 10000) and rel (t - G; NA for never treated). Bins list
# the event times present in d, as fixest requires. Used by 06_sunab_twfe.R and 10_dip.R.
sunab_rhs <- function(d) {
  lo <- event_window[1] - 1L; hi <- event_window[2] + 1L
  r <- sort(unique(na.omit(d$rel)))
  bins <- c(if (any(r <= lo)) sprintf('"%d" = c(%s)', lo, paste(r[r <= lo], collapse = ",")),
            if (any(r >= hi)) sprintf('"%d" = c(%s)', hi, paste(r[r >= hi], collapse = ",")))
  if (length(bins)) sprintf("sunab(G_sa, t, bin.rel = list(%s))", paste(bins, collapse = ", ")) else "sunab(G_sa, t)"
}

# linear combination w'b of event-study coefficients (names = event times; -1 is the reference, 0)
lincom <- function(b, V, w) {
  w <- w[names(w) != "-1"]
  c(estimate = sum(w * b[names(w)]), se = sqrt(drop(t(w) %*% V[names(w), names(w)] %*% w)))
}
ev <- function(e, x) setNames(rep(x, length(e)), e)

# Sun-Abraham fit as in 06_sunab_twfe.R; returns event-time coefficients and their vcov (10, 12)
sa_fit <- function(d, y) {
  d <- prep_outcome(d, y)
  d[, G_sa := fifelse(G > 0, G, 10000L)][, rel := fifelse(G > 0, t - G, NA_integer_)]
  m <- fixest::feols(as.formula(paste(y, "~", sunab_rhs(d), "| id + t")), data = d, cluster = ~id)
  s <- summary(m)
  # fixest reports event-time coefficients aggregated over cohorts (weights = observations per
  # cohort x event time), but vcov() is that of the cohort x event-time coefficients: rebuild the
  # aggregation matrix A, then b = A b_raw, V = A V_raw A' (checked against fixest's own output)
  raw <- names(m$coefficients); per <- sub(":cohort::.*", "", raw)
  n <- colSums(model.matrix(m, type = "rhs") != 0)[raw]
  A <- t(sapply(names(coef(s)), function(p) ifelse(per == p, n, 0) / sum(n[per == p])))
  b <- drop(A %*% m$coefficients); V <- A %*% vcov(m) %*% t(A)
  stopifnot(max(abs(b - coef(s))) < 1e-8, max(abs(sqrt(diag(V)) - fixest::se(s))) < 1e-8)
  names(b) <- sub(".*::", "", names(coef(s))); dimnames(V) <- list(names(b), names(b))
  list(b = b, V = V, n_treated = uniqueN(d[G > 0, id]))
}
# Callaway-Sant'Anna main specification (02_cs.R) with analytic SEs; vcov from the influence function (10, 12)
cs_fit <- function(d, y) {
  d <- as.data.frame(prep_outcome(d, y))
  a <- did::att_gt(yname = y, tname = "t", idname = "id", gname = "G", data = d, control_group = "nevertreated",
              base_period = "universal", allow_unbalanced_panel = TRUE, est_method = "dr",
              clustervars = "id", bstrap = FALSE)
  es <- did::aggte(a, type = "dynamic", min_e = event_window[1], max_e = event_window[2], na.rm = TRUE, bstrap = FALSE)
  keep <- es$egt != -1
  inf <- es$inf.function$dynamic.inf.func.e[, keep, drop = FALSE]
  V <- crossprod(inf) / nrow(inf)^2
  b <- setNames(es$att.egt[keep], es$egt[keep]); dimnames(V) <- list(names(b), names(b))
  stopifnot(all(abs(sqrt(diag(V)) / es$se.egt[keep] - 1) < 0.01))   # vcov matches did's analytic SEs
  list(b = b, V = V, n_treated = length(unique(d$id[d$G > 0])))
}

# which treatment/sample combinations each estimator runs (subset in test mode via outcomes only)
runs_absorbing <- tribble(
  ~treatment, ~sample,
  "jobloss",  "all",
  "jobloss",  "men",
  "jobloss",  "women",
  "hh_inc",   "all",
  "inc",      "all",
  "pinc25",   "all",
  "pinc25",   "men",
  "pinc25",   "women"
)

get_panel <- function(panels, treatment, sample = "all") {
  d <- copy(panels[[treatment]])
  if (sample == "men")   d <- d[male_base == 1]
  if (sample == "women") d <- d[male_base == 0]
  d
}


# Tidy output schema --------------------------------------------------------
# Every estimator script saves one table with these columns (09_compare.R stacks them).

# crit = critical value for uniform (simultaneous) bands, NA where the estimator has none
tidy_cols <- c("estimator", "spec", "treatment", "sample", "outcome", "term",
               "estimate", "se", "crit", "n_units", "n_treated")

as_tidy <- function(dt, estimator, spec, treatment, sample, outcome, n_units = NA_integer_, n_treated = NA_integer_) {
  dt <- as.data.table(dt)
  if (!"crit" %in% names(dt)) dt[, crit := NA_real_]
  dt[, `:=`(estimator = estimator, spec = spec, treatment = treatment, sample = sample, outcome = outcome,
            n_units = as.integer(n_units), n_treated = as.integer(n_treated))]
  dt[, ..tidy_cols]
}

# run one estimation and keep going if it fails (failures are logged and returned as NULL)
try_est <- function(expr, label) {
  t0 <- Sys.time()
  out <- tryCatch(expr, error = function(e) { message("  FAILED ", label, ": ", conditionMessage(e)); NULL })
  if (!is.null(out)) message("  ok ", label, " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s)")
  out
}


# Plotting ------------------------------------------------------------------

# categorical colors in fixed order (validated for color-vision deficiency; three of them are
# low-contrast on white, so every series also gets its own point shape and a legend)
series_pal    <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
series_shapes <- c(16, 17, 15, 18, 1, 2)

# event-study plot: 95% (thin) and 90% (thick) pointwise intervals, no end caps;
# uniform = TRUE adds the simultaneous band (grey) where the estimator provides one;
# dodge = share of each wave over which the series are spread side by side
plot_es <- function(dt, color = NULL, ncol = 2, uniform = FALSE, xlab = "Waves relative to treatment", dodge = 0.6) {
  dt <- as.data.table(dt)[term >= event_window[1] & term <= event_window[2]]
  waves <- seq(event_window[1], event_window[2])
  # every series gets a slot in every wave (empty where it has no estimate), so each series keeps
  # its position within the waves, e.g. when one estimator has no pre-period estimates
  if (!is.null(color)) {
    grid <- CJ(outcome = unique(dt$outcome), term = waves, series = unique(dt[[color]]))
    setnames(grid, "series", color)
    dt <- dt[grid, on = c("outcome", "term", color)]
  }
  dt[, outcome_label := factor(outcome_labels[outcome], levels = outcome_labels)]
  pos <- position_dodge(width = if (is.null(color)) 0 else dodge)
  aes_col <- if (is.null(color)) aes() else aes(color = .data[[color]], shape = .data[[color]])
  n_ser <- if (is.null(color)) 0 else uniqueN(dt[[color]])
  if (n_ser > length(series_pal)) stop("plot_es: more than ", length(series_pal), " series; split the figure")
  ggplot(dt, aes(term, estimate)) + aes_col +
    (if (n_ser) scale_color_manual(values = series_pal[seq_len(n_ser)])) +
    (if (n_ser) scale_shape_manual(values = series_shapes[seq_len(n_ser)])) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    (if (uniform) geom_linerange(aes(ymin = estimate - crit * se, ymax = estimate + crit * se),
                                 color = "grey75", linewidth = 2.2, position = pos, na.rm = TRUE)) +
    geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                   linewidth = 0.4, position = pos, na.rm = TRUE) +
    geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se),
                   linewidth = 1, position = pos, na.rm = TRUE) +
    geom_point(size = 2, position = pos, na.rm = TRUE) +
    facet_wrap(~ outcome_label, scales = "free_y", ncol = ncol, labeller = label_wrap_gen(width = 40)) +
    scale_x_continuous(breaks = waves) +
    theme_hanno() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(x = xlab, y = "ATT")
}
