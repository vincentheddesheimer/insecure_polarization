### New DiD analysis: the dip in the wave before job loss
# Callaway-Sant'Anna's effect of job loss on distance disappears when the reference wave is t-2
# instead of t-1 (anticipation = 1): relative to the never unemployed, job losers' distance is
# lower at t-1 than at t-2. This script asks
#   1. whether t-1 is unusually low or t-2 unusually high, relative to the earlier pre-waves
#      (contrasts of Sun-Abraham and Callaway-Sant'Anna event-study coefficients, with SEs);
#   2. how the average effect over waves 0-8 depends on the reference: t-1, t-2, mean of t-5..t-1;
#   3. whether single cohorts drive the dip, and whether it coincides with a change in the
#      vote-share weights of distance (the election used, election_before() in 00_setup.R, switches
#      at waves 11, 13, 18, 22, 24 and 26);
#   4. whether job losers expect the job loss at t-1: subjective chance of losing the job within
#      12 months (income module; asked of employed people only, so pre-treatment waves only) and
#      financial-situation items. The income module of wave w is fielded in June, between the
#      politics interviews of waves w and w + 1 (00_setup.R), so its "t-1" lies half a wave before
#      the interview that records the job loss, and people who have lost their job by then are not
#      asked; its "t-2" lies a wave and a half before. 14_apsr_note.R plots them at -0.5 and -1.5.
#   5. the unweighted components like max and like min, as a check on the vote-share weights.
# Job-loss panel, never-unemployed comparison group, waves numbered per outcome (prep_outcome).
# Needs data/liss_income_to26.csv (cleaned income module of the build; the items are not in
# liss_to26.csv). Saves aggregates only: results/estimates/dip/dip.rds.

rm(list = ls())
here::i_am("code/analyze_did/10_dip.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fixest, did)

dip_outcomes <- c("distance", "spread", "partisan_affect", "like_max", "like_min", "generalized_trust")

# income-module items; response scales changed across waves (e.g. items 243/244 -> 377/378), so
# the financial items are standardized within wave. Job-loss chance in percent (0-100);
# -8/-9 (quitting voluntarily / no job) are missing.
econ_labels <- c(pr_lose_job    = "Chance of losing job within 12 months (%)",
                 fin_sit_better = "Household finances better than a year ago (z)",
                 easy_live_inc  = "Easy to live off household income (z)",
                 hh_fin_sat     = "Household financial situation (z)")

contrast_defs <- list(
  `t-1 vs mean(t-5..t-2)` = ev(-5:-2, -1/4),
  `t-2 vs mean(t-5..t-3)` = c(ev(-2, 1), ev(-5:-3, -1/3)),
  `waves 0-8 vs t-1`      = ev(0:8, 1/9),
  `waves 0-8 vs t-2`      = c(ev(0:8, 1/9), ev(-2, -1)),
  `waves 0-8 vs mean(t-5..t-1)` = c(ev(0:8, 1/9), ev(-5:-2, -1/5)),
  `waves 0-8 vs mean(t-5..t-2)` = c(ev(0:8, 1/9), ev(-5:-2, -1/4)),
  # baseline before job-loss expectations rise (they are up at t-2 and t-1, part 4 below)
  `t-1 vs mean(t-5..t-3)`       = ev(-5:-3, -1/3),
  `waves 0-8 vs mean(t-5..t-3)` = c(ev(0:8, 1/9), ev(-5:-3, -1/3)))
# event study re-centred on another reference: b_e - mean(b_ref)
recentre <- function(b, V, ref) {
  rbindlist(lapply(as.character(event_window[1]:event_window[2]), function(e) {
    w <- c(ev(e, 1), ev(ref, -1 / length(ref)))
    w <- c(tapply(w, names(w), sum))
    if (all(abs(w) < 1e-12)) return(data.table(term = as.numeric(e), estimate = 0, se = NA_real_))
    r <- lincom(b, V, w); data.table(term = as.numeric(e), estimate = r[["estimate"]], se = r[["se"]])
  }))
}
references <- list(`t-1 (main)` = "-1", `t-2 (anticipation)` = "-2", `mean of t-5..t-3` = as.character(-5:-3))


# cohort-level change between consecutive pre-waves, job losers minus never unemployed:
# k = 1: t-2 -> t-1, k = 2: t-3 -> t-2, k = 3: t-4 -> t-3
cohort_steps <- function(d, y) {
  d <- prep_outcome(d, y)
  wide <- dcast(d[, .(id, t, G, y = get(y))], id + G ~ t, value.var = "y")
  waves <- d[, .(t_cal = t_cal[1]), by = t]
  rbindlist(lapply(sort(unique(d[G > 0, G])), function(g) rbindlist(lapply(1:3, function(k) {
    a <- g - k - 1L; b <- g - k
    if (a < 1) return(NULL)
    x <- wide[G %in% c(0L, g), .(treated = G == g, dy = get(as.character(b)) - get(as.character(a)))][!is.na(dy)]
    n1 <- x[treated == TRUE, .N]; n0 <- x[treated == FALSE, .N]
    if (n1 < 2 || n0 < 2) return(NULL)
    wa <- waves[t == a, t_cal] + 7L; wb <- waves[t == b, t_cal] + 7L
    data.table(outcome = y, cohort_wave = waves[t == g, t_cal] + 7L, step = sprintf("t-%d -> t-%d", k + 1, k),
               from_wave = wa, to_wave = wb, weights_change = election_before(wa) != election_before(wb),
               estimate = x[treated == TRUE, mean(dy)] - x[treated == FALSE, mean(dy)],
               se = sqrt(x[treated == TRUE, var(dy)] / n1 + x[treated == FALSE, var(dy)] / n0), n_treated = n1)
  }))))
}

if (do_estimate) {
panels <- load_panels()
jl <- get_panel(panels, "jobloss")

# 1, 2, 5: contrasts and re-centred event studies
contrasts <- list(); recentred <- list()
for (y in dip_outcomes) for (est in c("Sun-Abraham", "CS")) {
  f <- try_est(if (est == "CS") cs_fit(jl, y) else sa_fit(jl, y), paste(est, y))
  if (is.null(f)) next
  contrasts[[paste(est, y)]] <- rbindlist(lapply(names(contrast_defs), function(nm) {
    r <- lincom(f$b, f$V, contrast_defs[[nm]])
    data.table(estimator = est, outcome = y, contrast = nm, estimate = r[["estimate"]], se = r[["se"]],
               n_treated = f$n_treated)
  }))
  recentred[[paste(est, y)]] <- rbindlist(lapply(names(references), function(nm)
    recentre(f$b, f$V, references[[nm]])[, `:=`(estimator = est, outcome = y, reference = nm)]))
}

# 3: cohorts
steps <- rbindlist(lapply(dip_outcomes, function(y) try_est(cohort_steps(jl, y), paste("cohort steps", y))))

# 4: expectations and finances around job loss
inc <- fread(here("data", "liss_income_to26.csv"), select = c("nomem_encr", "wave", names(econ_labels)))
inc <- inc[!duplicated(inc[, .(nomem_encr, wave)])]
inc[!between(pr_lose_job, 0, 100), pr_lose_job := NA]
diag_items <- rbindlist(lapply(names(econ_labels), function(v)
  inc[, .(item = v, n = sum(!is.na(get(v))), mean = mean(get(v), na.rm = TRUE), sd = sd(get(v), na.rm = TRUE),
          min = suppressWarnings(min(get(v), na.rm = TRUE)), max = suppressWarnings(max(get(v), na.rm = TRUE))), by = wave]))
for (v in setdiff(names(econ_labels), "pr_lose_job"))
  inc[, (v) := (get(v) - mean(get(v), na.rm = TRUE)) / sd(get(v), na.rm = TRUE), by = wave]
jl_e <- merge(jl[, .(id, t, wave = t + 7L, G, D)], inc, by.x = c("id", "wave"), by.y = c("nomem_encr", "wave"), all.x = TRUE)

# job-loss chance (asked of employed people): pre-treatment waves only, event times binned at <= -5
#   levels: job losers minus never unemployed in the same wave; within: change from <= t-5 (person FE)
pre <- prep_outcome(jl_e, "pr_lose_job", absorbing = FALSE)[G == 0 | t < G]
pre[, rel_b := fifelse(G > 0, pmax(t - G, -5L), -1000L)]
pr_lose <- rbind(
  as.data.table(coeftable(feols(pr_lose_job ~ i(rel_b, ref = -1000) | t, pre, cluster = ~id)), keep.rownames = "term")[, model := "levels vs never unemployed (wave FE)"],
  as.data.table(coeftable(feols(pr_lose_job ~ i(rel_b, ref = c(-1000, -5)) | id + t, pre, cluster = ~id)), keep.rownames = "term")[, model := "change from <= t-5 (person and wave FE)"])
setnames(pr_lose, c("Estimate", "Std. Error"), c("estimate", "se"))
pr_lose <- pr_lose[, .(model, term = as.numeric(sub(".*::", "", term)), estimate, se)]
pr_lose_n <- pre[G > 0 & !is.na(pr_lose_job), .(n_obs = .N, n_persons = uniqueN(id)), by = .(rel_b)]
pr_lose_mean <- pre[!is.na(pr_lose_job), .(mean = mean(pr_lose_job), n = .N), by = .(group = fifelse(G > 0, "job losers, pre-treatment", "never unemployed"))]

# financial items: Sun-Abraham event studies over the whole window
econ_es <- rbindlist(lapply(setdiff(names(econ_labels), "pr_lose_job"), function(v) {
  f <- try_est(sa_fit(jl_e, v), paste("Sun-Abraham", v))
  if (is.null(f)) return(NULL)
  rbind(recentre(f$b, f$V, "-1")[, `:=`(outcome = v, contrast = "event study, reference t-1")],
        rbindlist(lapply(names(contrast_defs)[1:2], function(nm) {
          r <- lincom(f$b, f$V, contrast_defs[[nm]])
          data.table(term = NA_real_, estimate = r[["estimate"]], se = r[["se"]], outcome = v, contrast = nm)
        })))
}))

dir.create(here("results", "estimates", "dip"), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(contrasts = rbindlist(contrasts), recentred = rbindlist(recentred), steps = steps,
             pr_lose = pr_lose, pr_lose_n = pr_lose_n, pr_lose_mean = pr_lose_mean, econ_es = econ_es,
             diag_items = diag_items), path_est("dip", "dip"))
}


# Figures and tables ------------------------------------------------------------

if (do_figures) {
pacman::p_load(knitr)
r <- readRDS(path_est("dip", "dip"))

# re-centred event studies (Sun-Abraham), one series per reference wave
x <- r$recentred[estimator == "Sun-Abraham"][, reference := factor(reference, names(references))]
ggsave(path_fig("dip_reference_sunab"), plot_es(x, color = "reference", dodge = 0.6, xlab = "Waves relative to job loss"),
       width = 14, height = 2.5 + 2.2 * ceiling(uniqueN(x$outcome) / 2))
x <- r$recentred[estimator == "CS"][, reference := factor(reference, names(references))]
ggsave(path_fig("dip_reference_cs"), plot_es(x, color = "reference", dodge = 0.6, xlab = "Waves relative to job loss"),
       width = 14, height = 2.5 + 2.2 * ceiling(uniqueN(x$outcome) / 2))

# cohort steps, distance: change between consecutive pre-waves by cohort
s <- r$steps[outcome == "distance"]
s[, lab := sprintf("wave %d (%d -> %d)%s", cohort_wave, from_wave, to_wave, fifelse(weights_change, " *", ""))]
p <- ggplot(s, aes(estimate, reorder(lab, -cohort_wave))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_linerange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), linewidth = 0.4) +
  geom_point(aes(size = n_treated)) + scale_size_area(max_size = 3, name = "Job losers") +
  facet_wrap(~ step, scales = "free_y", nrow = 1) +
  theme_hanno() +
  labs(x = "Change in distance, job losers minus never unemployed (95% CI)", y = "Cohort (wave of job loss; waves of the step)",
       caption = "* the vote-share weights of distance change between the two waves (new election)")
ggsave(path_fig("dip_cohort_steps_distance"), p, width = 13, height = 1.5 + 0.28 * uniqueN(s$cohort_wave))

# job-loss chance before job loss
pl <- r$pr_lose[term > -1000]
p <- ggplot(pl, aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.4) +
  geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ model, scales = "free_y") +
  scale_x_continuous(breaks = -5:-1, labels = c("<= -5", -4:-1)) +
  theme_hanno() +
  labs(x = "Waves relative to job loss", y = "Chance of losing job within 12 months (pp)")
ggsave(path_fig("dip_pr_lose_job"), p, width = 9, height = 3.5)

tab <- dcast(r$contrasts[, .(estimator, Outcome = outcome_labels[outcome], contrast,
                             v = sprintf("%.3f (%.3f)", estimate, se))], estimator + Outcome ~ contrast, value.var = "v")
writeLines(kable(tab, format = "latex", booktabs = TRUE, linesep = ""), path_tab("dip_contrasts"))
}

### END
