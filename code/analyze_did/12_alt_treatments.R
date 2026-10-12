### New DiD analysis: alternative treatments of economic insecurity
# Could an income drop or a rise in perceived insecurity replace job loss as the main treatment?
# For each candidate this script reports
#   1. how many people it gives (treated persons, cohorts, share of the sample ever treated);
#   2. how persistent it is (state one wave after the event);
#   3. how it overlaps with job loss and retirement, and the job-loss chance just before it;
#   4. first estimates for distance, spread and like min: Sun-Abraham and Callaway-Sant'Anna
#      averages over waves 0-8 (against wave -1 and against waves -5 to -3), a pre-trend
#      contrast (wave -1 vs waves -5 to -2), imputation (BJS), and minimum detectable effects;
#   5. the within-person association of the outcomes with the job-loss chance (continuous).
# Sample and coding as for job loss (01_sample.R): ages 18-65, absorbing treatment = first event,
# which must follow an employed wave; before the event only employed waves, after it all waves;
# never treated = comparison group. Events compare a wave with the directly preceding wave.
# Needs data/liss_to26.csv and data/liss_income_to26.csv. Saves results/estimates/alt/alt.rds.

rm(list = ls())
here::i_am("code/analyze_did/12_alt_treatments.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fixest, did, didimputation)

alt_outcomes <- if (test_mode) "distance" else c("distance", "spread", "like_min")

treatments <- fread(text = "treat|label
jobloss|Job loss (benchmark)
hh10|Household income drop >= 10%
hh25|Household income drop >= 25%
hh10_emp|Household income drop >= 10%, still employed
p10|Personal income drop >= 10%
p25|Personal income drop >= 25%
p25_emp|Personal income drop >= 25%, still employed
insec50|Job-loss chance rises to >= 50%
insec50_keep|Job-loss chance rises to >= 50%, job kept 2 waves
hardship|Household starts using savings or debt
endsmeet|Trouble making ends meet (first report)", sep = "|")


# Data ----------------------------------------------------------------------

if (do_estimate) {
liss <- fread(here("data", "liss_to26.csv"))
liss <- liss[!duplicated(liss[, .(nomem_encr, wave)])]
liss[, `:=`(id = nomem_encr, t = as.integer(wave - 7))]
liss <- liss[age >= 18 & age <= 65]
liss[, `:=`(emp = occupation %in% 1:3, unemp = occupation %in% c(4, 6))]
inc <- fread(here("data", "liss_income_to26.csv"),
             select = c("nomem_encr", "wave", "pr_lose_job", "hh_fin_sat", "confr_trouble_ends_meet"))
inc <- inc[!duplicated(inc[, .(nomem_encr, wave)])]
inc[!between(pr_lose_job, 0, 100), pr_lose_job := NA]
d <- merge(liss[, c("id", "t", "wave", names(outcome_labels), "occupation", "emp", "unemp", "age", "male",
                    "education_cat", "no_hh", "net_monthly_income_cat", "net_monthly_income", "net_monthly_income_hh"), with = FALSE],
           inc, by.x = c("id", "wave"), by.y = c("nomem_encr", "wave"), all.x = TRUE)
setorder(d, id, t)
item_codes <- list(hh_fin_sat = d[, .N, by = hh_fin_sat][order(hh_fin_sat)],
                   confr_trouble_ends_meet = d[, .N, by = confr_trouble_ends_meet][order(confr_trouble_ends_meet)])

# events: change from the directly preceding wave (t - 1 observed)
lag_cols <- c("net_monthly_income_hh", "net_monthly_income", "pr_lose_job", "hh_fin_sat", "confr_trouble_ends_meet", "emp")
d[, paste0(lag_cols, "_l") := shift(.SD), by = id, .SDcols = lag_cols]
d[, consec := t - shift(t) == 1, by = id]
d[is.na(consec), consec := FALSE]
d[, `:=`(
  ev_jobloss  = unemp,
  ev_hh10     = consec & net_monthly_income_hh_l > 0 & net_monthly_income_hh <= 0.90 * net_monthly_income_hh_l,
  ev_hh25     = consec & net_monthly_income_hh_l > 0 & net_monthly_income_hh <= 0.75 * net_monthly_income_hh_l,
  ev_p10      = consec & net_monthly_income_l > 0 & net_monthly_income <= 0.90 * net_monthly_income_l,
  ev_p25      = consec & net_monthly_income_l > 0 & net_monthly_income <= 0.75 * net_monthly_income_l,
  ev_insec50  = consec & pr_lose_job_l < 50 & pr_lose_job >= 50,
  ev_hardship = consec & hh_fin_sat_l >= 3 & hh_fin_sat <= 2,
  ev_endsmeet = consec & confr_trouble_ends_meet_l == 0 & confr_trouble_ends_meet == 1)]
d[, `:=`(ev_hh10_emp = ev_hh10 & emp, ev_p25_emp = ev_p25 & emp, ev_insec50_keep = ev_insec50)]

# absorbing panel as in 01_sample.R (make_absorbing)
absorb <- function(key) {
  x <- copy(d)
  x[, ev := get(paste0("ev_", key)) %in% TRUE]
  x[, G := if (any(ev)) t[which(ev)[1]] else 0L, by = id]
  x[, prev_emp := shift(emp), by = id]
  bad <- x[G > 0 & t == G & !(prev_emp %in% TRUE), unique(id)]
  x <- x[!id %in% bad]
  if (key == "insec50_keep")                                 # insecurity without losing the job soon after
    x <- x[!id %in% x[G > 0 & t >= G & t <= G + 2 & unemp == TRUE, unique(id)]]
  x <- x[(G > 0 & t >= G) | emp]
  x[, D := as.integer(G > 0 & t >= G)]
  x[, c("ev", "prev_emp") := NULL][]
}
panels_alt <- setNames(lapply(treatments$treat, absorb), treatments$treat)


# 1-3: size, persistence, overlap ---------------------------------------------

persist <- function(key, w) {                                # share still in the state one wave later
  r0 <- w[rel == -1]; r1 <- w[rel == 1]
  m <- merge(r0, r1, by = "id", suffixes = c("_0", "_1"))
  switch(sub("_.*", "", key),
    jobloss  = r1[, mean(unemp)],
    hh10     = m[, mean(net_monthly_income_hh_1 <= 0.90 * net_monthly_income_hh_0, na.rm = TRUE)],
    hh25     = m[, mean(net_monthly_income_hh_1 <= 0.75 * net_monthly_income_hh_0, na.rm = TRUE)],
    p10      = m[, mean(net_monthly_income_1 <= 0.90 * net_monthly_income_0, na.rm = TRUE)],
    p25      = m[, mean(net_monthly_income_1 <= 0.75 * net_monthly_income_0, na.rm = TRUE)],
    insec50  = r1[, mean(pr_lose_job >= 50, na.rm = TRUE)],
    hardship = r1[, mean(hh_fin_sat <= 2, na.rm = TRUE)],
    endsmeet = r1[, mean(confr_trouble_ends_meet == 1, na.rm = TRUE)])
}
info <- rbindlist(lapply(treatments$treat, function(k) {
  p <- panels_alt[[k]]
  tr <- unique(p[G > 0, .(id, G)])
  w <- merge(d, tr, by = "id")[, rel := t - G]
  data.table(treat = k, persons = uniqueN(p$id), treated = nrow(tr), cohorts = uniqueN(tr$G),
             share_treated = nrow(tr) / uniqueN(p$id),
             unemp_at_event = w[rel == 0, mean(unemp)],
             unemp_within_2 = w[rel %in% 0:2, .(u = any(unemp)), by = id][, mean(u)],
             retired_at_event = w[rel == 0, mean(occupation %in% 9)],
             job_loss_chance_before = w[rel == -1, mean(pr_lose_job, na.rm = TRUE)],
             still_one_wave_later = persist(k, w))
}))
info <- merge(treatments, info, by = "treat", sort = FALSE)
message("treatments:"); print(info)


# 4: estimates --------------------------------------------------------------

contr <- list(`waves 0-8 vs -1` = ev(0:8, 1/9),
              `waves 0-8 vs -5..-3` = c(ev(0:8, 1/9), ev(-5:-3, -1/3)),
              `pre: -1 vs -5..-2` = ev(-5:-2, -1/4))
bjs_fit <- function(dd, y) {
  dd <- as.data.frame(prep_outcome(dd, y))
  dd$w_overall <- with(dd, as.numeric(D == 1 & t - G <= event_window[2])); dd$w_overall <- dd$w_overall / sum(dd$w_overall)
  ov <- did_imputation(data = dd, yname = y, gname = "G", tname = "t", idname = "id", wtr = "w_overall", cluster_var = "id")
  data.table(contrast = "waves 0-8 (imputation)", estimate = ov$estimate[1], se = ov$std.error[1])
}
cells <- CJ(treat = treatments$treat, outcome = alt_outcomes, est = c("Sun-Abraham", "CS", "BJS"), sorted = FALSE)
# forked workers: one thread each (OpenMP in a forked process can deadlock, see della/run_script.slurm)
est <- rbindlist(parallel::mclapply(seq_len(nrow(cells)), function(i) {
  setDTthreads(1); fixest::setFixest_nthreads(1)
  k <- cells$treat[i]; y <- cells$outcome[i]; e <- cells$est[i]
  r <- tryCatch({
    if (e == "BJS") bjs_fit(panels_alt[[k]], y) else {
      f <- if (e == "CS") cs_fit(panels_alt[[k]], y) else sa_fit(panels_alt[[k]], y)
      rbindlist(lapply(names(contr), function(nm) {
        l <- lincom(f$b, f$V, contr[[nm]]); data.table(contrast = nm, estimate = l[["estimate"]], se = l[["se"]])
      }))
    }
  }, error = function(err) data.table(contrast = "FAILED", estimate = NA_real_, se = NA_real_, error = conditionMessage(err)))
  r[, `:=`(treat = k, outcome = y, estimator = e)]
}, mc.cores = n_cores), fill = TRUE)
est[, mde := 2.80 * se]
message("estimates:"); print(est[outcome == "distance"])

# Sun-Abraham event studies for distance (figures)
es_sa <- rbindlist(lapply(treatments$treat, function(k) {
  f <- tryCatch(sa_fit(panels_alt[[k]], "distance"), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  data.table(treat = k, outcome = "distance", term = as.numeric(names(f$b)), estimate = f$b, se = sqrt(diag(f$V)))
}))


# 5: continuous job-loss chance ---------------------------------------------

cont <- rbindlist(lapply(alt_outcomes, function(y) {
  x <- d[emp & !is.na(pr_lose_job)]
  x <- x[!is.na(x[[y]])]
  m <- feols(as.formula(paste(y, "~ I(pr_lose_job / 10) | id + t")), x, cluster = ~id)
  data.table(outcome = y, estimate = coef(m)[[1]], se = se(m)[[1]], n_obs = nobs(m), n_persons = uniqueN(x$id))
}))
message("continuous:"); print(cont)

dir.create(here("results", "estimates", "alt"), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(info = info, est = est, es_sa = es_sa, cont = cont, item_codes = item_codes), path_est("alt", "alt"))
}


# Figures -------------------------------------------------------------------

if (do_figures) {
r <- readRDS(path_est("alt", "alt"))
x <- merge(r$es_sa, r$info[, .(treat, label)], by = "treat")
x[, label := factor(label, r$info$label)]
x <- rbind(x, unique(x[, .(treat, outcome, label)])[, `:=`(term = -1, estimate = 0, se = NA_real_)], fill = TRUE)
p <- ggplot(x[term >= event_window[1] & term <= event_window[2]], aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = -0.5, linetype = "dashed") +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.4, na.rm = TRUE) +
  geom_linerange(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), linewidth = 1, na.rm = TRUE) +
  geom_point(size = 1.8) +
  facet_wrap(~ label, ncol = 3, scales = "free_y", labeller = label_wrap_gen(width = 32)) +
  scale_x_continuous(breaks = seq(event_window[1], event_window[2], 2)) +
  theme_hanno() + labs(x = "Waves relative to event", y = "Change in distance (Sun-Abraham)")
ggsave(path_fig("alt_treatments_distance"), p, width = 11, height = 2 + 2.3 * ceiling(nrow(r$info) / 3))
}

### END
