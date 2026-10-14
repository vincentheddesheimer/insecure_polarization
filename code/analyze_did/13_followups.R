### New DiD analysis: follow-ups on the coauthors' questions (October 2026)
# 1. Transience: is the effect in waves 2-3 larger than in waves 5-8? Contrasts of the
#    event-study estimates with their full covariance (Sun-Abraham, Callaway-Sant'Anna), for job
#    loss and large personal income loss (pinc25), all / men / women.
# 2. Periods: Callaway-Sant'Anna effects by wave of treatment and by calendar wave, and the main
#    estimates (a) with the panel ending in wave 19 (before 2020) and (b) without treatments from
#    wave 20 on. Wave w is fielded around December of year 1999 + w (wave 23: winter 2022/23,
#    energy price shock; wave 24: after the November 2023 election).
# 3. Party outcome, named in advance and exploratory: rating of the PVV (0-10), with PVV vote
#    intention as a secondary check (politics module, data/liss_politics_to26.csv).
# 4. Large personal income loss: composition and reasons, drops that look voluntary (birth of a
#    child, retirement, studies, housework), other thresholds (20/30/50%), and drops in the
#    self-reported net wage of the income module (the background income is imputed when missing).
# Fast estimators only (Sun-Abraham, Callaway-Sant'Anna with analytic SEs, imputation); the full
# set for pinc25 runs in 02-08 with DID_TREATMENTS=pinc25. Saves results/estimates/followups/.

rm(list = ls())
here::i_am("code/analyze_did/13_followups.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fixest, did, didimputation)

ap_outcomes <- if (test_mode) "distance" else c("distance", "spread", "like_min")
contr <- list(`waves 0-8 vs -1` = ev(0:8, 1/9),
              `waves 0-8 vs -5..-3` = c(ev(0:8, 1/9), ev(-5:-3, -1/3)),
              `pre: -1 vs -5..-2` = ev(-5:-2, -1/4),
              `peak: waves 2-3` = ev(2:3, 1/2),
              `late: waves 5-8` = ev(5:8, 1/4),
              `peak - late` = c(ev(2:3, 1/2), ev(5:8, -1/4)))
fit_contrasts <- function(d, y, est) {
  f <- if (est == "CS") cs_fit(d, y) else sa_fit(d, y)
  rbindlist(lapply(names(contr), function(nm) {
    l <- lincom(f$b, f$V, contr[[nm]]); data.table(contrast = nm, estimate = l[["estimate"]], se = l[["se"]])
  }))[, n_treated := f$n_treated]
}
bjs_avg <- function(d, y) {
  d <- as.data.frame(prep_outcome(d, y))
  d$w_overall <- with(d, as.numeric(D == 1 & t - G <= event_window[2])); d$w_overall <- d$w_overall / sum(d$w_overall)
  ov <- did_imputation(data = d, yname = y, gname = "G", tname = "t", idname = "id", wtr = "w_overall", cluster_var = "id")
  data.table(contrast = "waves 0-8 (imputation)", estimate = ov$estimate[1], se = ov$std.error[1], n_treated = length(unique(d$id[d$G > 0])))
}
# run a list of jobs in forked workers, one thread each (see della/run_script.slurm)
run_jobs <- function(jobs, fun) rbindlist(parallel::mclapply(jobs, function(j) {
  setDTthreads(1); fixest::setFixest_nthreads(1)
  tryCatch(fun(j), error = function(e) data.table(job = paste(unlist(j), collapse = "|"), error = conditionMessage(e)))
}, mc.cores = n_cores), fill = TRUE)

if (do_estimate) {
panels <- load_panels()

# politics module: PVV rating and vote intention
pol <- fread(here("data", "liss_politics_to26.csv"), select = c("nomem_encr", "wave", "thermo_PVV", "elect_today_PVV"))
pol <- pol[!duplicated(pol[, .(nomem_encr, wave)])]
codes_pvv <- list(thermo = pol[, .N, by = thermo_PVV][order(thermo_PVV)], vote = pol[, .N, by = elect_today_PVV][order(elect_today_PVV)])
pol[!between(thermo_PVV, 0, 10), thermo_PVV := NA]
pol[!elect_today_PVV %in% 0:1, elect_today_PVV := NA]
add_pol <- function(p) setorder(merge(p, pol, by.x = c("id", "wave"), by.y = c("nomem_encr", "wave"), all.x = TRUE), id, t)[]
pan <- list(jobloss = add_pol(panels$jobloss), pinc25 = add_pol(panels$pinc25))
samp <- function(tr, sm) { d <- pan[[tr]]; if (sm == "men") d <- d[male_base == 1]; if (sm == "women") d <- d[male_base == 0]; d }


# 1. Transience (and the averages), job loss and pinc25 -------------------------
jobs <- CJ(tr = c("jobloss", "pinc25"), sm = c("all", "men", "women"), y = ap_outcomes, est = c("Sun-Abraham", "CS"), sorted = FALSE)
transience <- run_jobs(split(jobs, seq_len(nrow(jobs))), function(j)
  fit_contrasts(samp(j$tr, j$sm), j$y, j$est)[, `:=`(treatment = j$tr, sample = j$sm, outcome = j$y, estimator = j$est)])
message("transience:"); print(transience[outcome == "distance" & contrast %in% c("peak: waves 2-3", "late: waves 5-8", "peak - late")])


# 2. Periods ----------------------------------------------------------------
cs_agg <- function(d, y) {
  dd <- prep_outcome(d, y)
  a <- did::att_gt(yname = y, tname = "t", idname = "id", gname = "G", data = as.data.frame(dd), control_group = "nevertreated",
                   base_period = "universal", allow_unbalanced_panel = TRUE, est_method = "dr", clustervars = "id", bstrap = FALSE)
  waves <- dd[, .(wave = t_cal[1] + 7L), by = t]
  ng <- dd[G > 0, .(n_treated = uniqueN(id)), by = G]
  rbindlist(lapply(c("group", "calendar"), function(type) {
    g <- did::aggte(a, type = type, na.rm = TRUE, bstrap = FALSE, cband = FALSE)
    r <- data.table(type = type, t = g$egt, estimate = g$att.egt, se = g$se.egt)[waves, on = "t", wave := i.wave]
    if (type == "group") r[ng, on = c(t = "G"), n_treated := i.n_treated]   # people treated in that wave
    r[]
  }), fill = TRUE)
}
by_period <- rbindlist(lapply(c("jobloss", "pinc25"), function(tr)
  cs_agg(pan[[tr]], "distance")[, `:=`(treatment = tr, outcome = "distance")]))

variants <- list(`all waves` = function(d) d,
                 `panel ends in wave 19` = function(d) d[wave <= 19],
                 `without treatments from wave 20 on` = function(d) d[!(G > 0 & G + 7L >= 20)])
jobs <- CJ(tr = c("jobloss", "pinc25"), v = names(variants), est = c("Sun-Abraham", "CS"), sorted = FALSE)
periods <- run_jobs(split(jobs, seq_len(nrow(jobs))), function(j)
  fit_contrasts(variants[[j$v]](pan[[j$tr]]), "distance", j$est)[, `:=`(treatment = j$tr, variant = j$v, estimator = j$est)])
message("periods:"); print(periods[contrast %in% c("waves 0-8 vs -1", "peak: waves 2-3")])


# 3. PVV rating and vote intention (exploratory) --------------------------------
jobs <- CJ(tr = c("jobloss", "pinc25"), sm = c("all", "men", "women"), y = c("thermo_PVV", "elect_today_PVV"),
           est = c("Sun-Abraham", "CS", "BJS"), sorted = FALSE)
pvv <- run_jobs(split(jobs, seq_len(nrow(jobs))), function(j) {
  d <- samp(j$tr, j$sm)
  r <- if (j$est == "BJS") bjs_avg(d, j$y) else fit_contrasts(d, j$y, j$est)
  r[, `:=`(treatment = j$tr, sample = j$sm, outcome = j$y, estimator = j$est)]
})
pvv_es <- rbindlist(lapply(c("jobloss", "pinc25"), function(tr) {
  f <- sa_fit(pan[[tr]], "thermo_PVV")
  data.table(treatment = tr, outcome = "thermo_PVV", term = as.numeric(names(f$b)), estimate = f$b, se = sqrt(diag(f$V)))
}))
pvv_means <- pol[, .(mean_thermo = mean(thermo_PVV, na.rm = TRUE), sd_thermo = sd(thermo_PVV, na.rm = TRUE),
                     share_vote = mean(elect_today_PVV, na.rm = TRUE), n = .N), by = wave][order(wave)]
message("PVV:"); print(pvv[contrast %in% c("waves 0-8 vs -1", "waves 0-8 (imputation)") & sample == "all"])


# 4. Large personal income loss: checks --------------------------------------------
liss <- fread(here("data", "liss_to26.csv"))
liss <- liss[!duplicated(liss[, .(nomem_encr, wave)])]
liss[, `:=`(id = nomem_encr, t = as.integer(wave - 7))]
liss <- liss[age >= 18 & age <= 65]
liss[, `:=`(emp = occupation %in% emp_codes, unemp = occupation %in% unemp_codes)]
inc <- fread(here("data", "liss_income_to26.csv"),
             select = c("nomem_encr", "wave", "net_wage_1", "inc_change_stopwork", "inc_change_startwork", "inc_change_changework",
                        "inc_change_promot", "inc_change_soc_ben_inc", "inc_change_soc_ben_dec"))
inc <- inc[!duplicated(inc[, .(nomem_encr, wave)])]
reason_cols <- grep("^inc_change_", names(inc), value = TRUE)
codes_inc <- c(list(net_wage_1 = inc[, .(n = .N, n_pos = sum(net_wage_1 > 0, na.rm = TRUE), min = min(net_wage_1, na.rm = TRUE),
                                         p50 = median(net_wage_1[net_wage_1 > 0], na.rm = TRUE), max = max(net_wage_1, na.rm = TRUE))]),
               lapply(setNames(reason_cols, reason_cols), function(v) inc[, .N, by = v][order(get(v))]))
d <- merge(liss[, c("id", "t", "wave", names(outcome_labels), "occupation", "emp", "unemp", "male", "no_children_hh",
                    "net_monthly_income"), with = FALSE],
           inc, by.x = c("id", "wave"), by.y = c("nomem_encr", "wave"), all.x = TRUE)
setorder(d, id, t)
d[, consec := fifelse(is.na(shift(t)), FALSE, t - shift(t) == 1), by = id]
# values in the directly preceding wave (NA after a gap)
lag1 <- function(x, t) fifelse(t - shift(t) == 1L, shift(x), NA)
d[, `:=`(inc_l = lag1(net_monthly_income, t), wage_l = lag1(net_wage_1, t), kids_l = lag1(no_children_hh, t),
         occ_l = lag1(occupation, t)), by = id]
d[, inc_l2 := lag1(inc_l, t), by = id]
drop_ev <- function(x, x_l, thr) d$consec & x_l > 0 & x <= (1 - thr) * x_l
for (thr in c(0.20, 0.25, 0.30, 0.50)) d[, (paste0("ev_p", 100 * thr)) := drop_ev(net_monthly_income, inc_l, thr)]
d[, ev_wage25 := consec & wage_l > 0 & net_wage_1 > 0 & net_wage_1 <= 0.75 * wage_l]
d[, voluntary := (no_children_hh > kids_l) %in% TRUE | occupation %in% 7:9]   # more children in the household, student, homemaker, pensioner
# drops that may reflect measurement rather than a loss: after a temporary rise (income in the wave
# before at least a third above the wave before that), of the self-employed or in a family business
# (occupation 2-3 in the wave before), or from implausible amounts (more than 20,000 euros a month)
d[, suspect := (inc_l >= 4 / 3 * inc_l2) %in% TRUE | occ_l %in% 2:3 | (inc_l > 20000) %in% TRUE]

# absorbing panel as in 01_sample.R (make_absorbing), with baseline gender
absorb <- function(evcol, drop_voluntary = FALSE, drop_suspect = FALSE) {
  x <- copy(d)
  x[, ev := get(evcol) %in% TRUE]
  x[, G := if (any(ev)) t[which(ev)[1]] else 0L, by = id]
  x[, prev_emp := fifelse(t - shift(t) == 1L, shift(emp), NA), by = id]   # employed in the directly preceding wave
  x <- x[!id %in% x[G > 0 & t == G & !(prev_emp %in% TRUE), unique(id)]]
  if (drop_voluntary) x <- x[!id %in% x[G > 0 & t == G & voluntary == TRUE, unique(id)]]
  if (drop_suspect) x <- x[!id %in% x[G > 0 & t == G & suspect == TRUE, unique(id)]]
  x <- x[(G > 0 & t >= G) | emp]
  x[, D := as.integer(G > 0 & t >= G)]
  x[, male_base := male[1], by = id]
  x[, c("ev", "prev_emp") := NULL][]
}
inc_variants <- list(
  `drop >= 25% (main)`            = function() absorb("ev_p25"),
  `drop >= 25%, men`              = function() absorb("ev_p25")[male_base == 1],
  `drop >= 25%, women`            = function() absorb("ev_p25")[male_base == 0],
  `drop >= 25%, without voluntary-looking drops` = function() absorb("ev_p25", drop_voluntary = TRUE),
  `drop >= 25%, without drops that may be measurement` = function() absorb("ev_p25", drop_suspect = TRUE),
  `drop >= 20%`                   = function() absorb("ev_p20"),
  `drop >= 30%`                   = function() absorb("ev_p30"),
  `drop >= 50%`                   = function() absorb("ev_p50"),
  `self-reported wage drop >= 25%` = function() absorb("ev_wage25"))
inc_panels <- lapply(inc_variants, function(f) f())

# composition at the event
inc_info <- rbindlist(lapply(names(inc_panels), function(v) {
  p <- inc_panels[[v]]
  tr <- unique(p[G > 0, .(id, G)])
  w <- merge(d, tr, by = "id")[t == G]
  r <- data.table(variant = v, persons = uniqueN(p$id), treated = nrow(tr), share_women = w[, mean(male == 0, na.rm = TRUE)],
                  unemployed = w[, mean(unemp)], employed = w[, mean(emp)], new_child = w[, mean((no_children_hh > kids_l) %in% TRUE)],
                  to_zero = w[, mean(net_monthly_income == 0, na.rm = TRUE)], suspect = w[, mean(suspect)],
                  pensioner_student_homemaker = w[, mean(occupation %in% 7:9)])
  for (rc in reason_cols) r[, (rc) := w[, mean(get(rc) == 1, na.rm = TRUE)]]
  r
}))
message("income-loss variants:"); print(inc_info)

jobs <- CJ(v = names(inc_panels), y = ap_outcomes, est = c("Sun-Abraham", "CS", "BJS"), sorted = FALSE)
inc_est <- run_jobs(split(jobs, seq_len(nrow(jobs))), function(j) {
  p <- inc_panels[[j$v]]
  r <- if (j$est == "BJS") bjs_avg(p, j$y) else fit_contrasts(p, j$y, j$est)
  r[, `:=`(variant = j$v, outcome = j$y, estimator = j$est)]
})
message("income-loss estimates:"); print(inc_est[outcome == "distance" & contrast %in% c("waves 0-8 vs -1", "waves 0-8 (imputation)", "pre: -1 vs -5..-2")])

dir.create(here("results", "estimates", "followups"), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(transience = transience, by_period = by_period, periods = periods, pvv = pvv, pvv_es = pvv_es, pvv_means = pvv_means,
             inc_info = inc_info, inc_est = inc_est, codes_pvv = codes_pvv, codes_inc = codes_inc),
        path_est("followups", "followups"))
}

### END
