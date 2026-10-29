### Income loss with and without job loss (Oct 29, 2026; answers Joanna's question of Oct 28)
# The two treatments are defined separately and overlap: a large income loss (pinc25) can come with a
# job loss or not, and a job loss (jobloss) with a large income drop or not. This script reports
#   1. the overlap: employment status of income losers in the wave of the loss (employed, unemployed
#      after job loss, other: retired, housework, studies, disability, ...) and the change in personal
#      net income of job losers from wave -1 to the wave of the loss;
#   2. the note's main specification (cs_fit: never-treated controls, no covariates) for subgroups of
#      the treated, each against all never treated: income loss by status in the wave of the loss, job
#      loss by whether personal income fell by 25 percent or more (or was not recorded in wave -1);
#      outcomes distance, like max, like min and the mean rating of the governing parties; overall
#      effect over waves 0-8, with differences between subgroups (covariance from the influence
#      functions, since the subgroups share the controls);
#   3. the same subgroups by intended vote in wave -1 (stacked within-person estimator of
#      17_extensions.R): rating of the governing parties and of one's own party.
# Not used by the note (14 does not read it). Saves results/estimates/split/split.rds. Runtime: under a minute.

rm(list = ls())
here::i_am("code/analyze_did/20_income_job_split.R")
Sys.setenv(DID_NO_FIGURES = "1")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(did)

panels <- load_panels()
in_range <- function(x, lo, hi) { x <- suppressWarnings(as.numeric(x)); fifelse(x >= lo & x <= hi, x, NA_real_) }


# Data: governing parties' mean rating and vote intention (as in 17_extensions.R) ------------------

parties <- c("VVD", "D66", "PVV", "CDA", "SP", "PvdA", "GL", "PvdD", "CU", "SGP", "FvD", "TON", "Volt",
             "JA21", "DENK", "50PLUS", "BBB", "BIJ1", "VNL", "NSC", "GL_PvdA")
hdr <- names(fread(here("data", "liss_politics_to26.csv"), nrows = 0))
pol <- fread(here("data", "liss_politics_to26.csv"),
             select = c("nomem_encr", "wave", paste0("thermo_", parties), grep("^vote_today_", hdr, value = TRUE)))
pol <- pol[!duplicated(pol[, .(nomem_encr, wave)])]
th <- melt(pol[, c("nomem_encr", "wave", paste0("thermo_", parties)), with = FALSE], id.vars = c("nomem_encr", "wave"),
           variable.name = "party", value.name = "rating", variable.factor = FALSE)
th[, `:=`(party = sub("thermo_", "", party), rating = in_range(rating, 0, 10))]
th <- th[!is.na(rating)]
th[cabinet, on = c("wave", "party"), gov := TRUE][is.na(gov), gov := FALSE]
gov_mean <- th[wave %in% cabinet$wave, .(gov_mean = if (any(gov)) mean(rating[gov]) else NA_real_), by = .(id = nomem_encr, wave)]
vote_cols <- intersect(paste0("vote_today_", c(parties, "other")), names(pol))
vt <- melt(pol[, c("nomem_encr", "wave", vote_cols), with = FALSE], id.vars = c("nomem_encr", "wave"), variable.name = "party",
           value.name = "v", variable.factor = FALSE)[, party := sub("vote_today_", "", party)][v %in% 1]
vt[cabinet, on = c("wave", "party"), gov := TRUE][is.na(gov), gov := FALSE]
vt <- vt[, .SD[1], by = .(nomem_encr, wave)]                        # one party per person-wave
vote_b <- merge(unique(pol[, .(id = nomem_encr, wave)]), vt[, .(id = nomem_encr, wave, own_party = party, vote_gov = gov)],
                by = c("id", "wave"), all.x = TRUE)
vote_b[, vote_group := fcase(vote_gov %in% TRUE, "governing", !is.na(own_party), "other party", default = "none named")]
vote_b[!wave %in% cabinet$wave, vote_group := NA_character_]
add_pol <- function(d) setorder(merge(d, gov_mean, by = c("id", "wave"), all.x = TRUE), id, t)[]


# 1. Overlap and subgroups -------------------------------------------------------------------------

# income losers: status in the wave of the loss (unemployed within the next two waves, for reference)
st <- panels$pinc25_all[G > 0 & t == G, .(id, occupation)]
st[, grp := fcase(occupation %in% emp_codes, "inc_kept", occupation %in% unemp_codes, "inc_jobloss", default = "inc_other")]
later <- panels$pinc25_all[G > 0 & t >= G & t <= G + 2, .(unemp_0_2 = any(unemp)), by = id]
st[later, on = "id", unemp_0_2 := i.unemp_0_2]
# job losers: personal net income in wave -1 and in the wave of the loss (a filter written as
# t %in% c(G - 1L, G) would compare each row with every person's G; see SPEC.md, pitfall of Oct 14)
jl <- panels$jobloss_all[G > 0 & (t == G - 1L | t == G), .(id, rel = fifelse(t == G, "at", "before"), net_monthly_income)]
jl <- dcast(jl, id ~ rel, value.var = "net_monthly_income")
jl[, chg := fifelse(before > 0, at / before - 1, NA_real_)]
jl[, grp := fcase(is.na(before) | is.na(at) | before <= 0, "job_inc_unknown", at <= 0.75 * before, "job_incdrop", default = "job_noincdrop")]
gj <- unique(panels$jobloss[G > 0, .(id, Gj = G)]); gi <- unique(panels$pinc25[G > 0, .(id, Gi = G)])
both <- merge(gj, gi, by = "id")
overlap <- list(
  inc_status = st[, .N, by = .(grp, occupation)][order(grp, -N)],
  inc_unemp_0_2 = st[, .N, by = .(grp, unemp_0_2)],
  job_income = jl[!is.na(chg), .(n = .N, drop25 = mean(chg <= -0.25), drop10 = mean(chg <= -0.10), rise = mean(chg > 0),
                                 median_chg = median(chg))][, n_unknown := jl[is.na(chg), .N]],
  both = data.table(job_n = nrow(gj), inc_n = nrow(gi), both_n = nrow(both), same_wave = both[Gi == Gj, .N],
                    inc_first = both[Gi < Gj, .N], job_first = both[Gj < Gi, .N]))

grp_ids <- c(split(st$id, st$grp), split(jl$id, jl$grp))
base_of <- c(inc_kept = "pinc25", inc_jobloss = "pinc25", inc_other = "pinc25",
             job_incdrop = "jobloss", job_noincdrop = "jobloss", job_inc_unknown = "jobloss")
grp_lab <- c(pinc25 = "Income loss, all", inc_kept = "Income loss, still employed", inc_jobloss = "Income loss with job loss",
             inc_other = "Income loss, left work otherwise", jobloss = "Job loss, all",
             job_incdrop = "Job loss, income -25% or more", job_noincdrop = "Job loss, smaller or no income drop",
             job_inc_unknown = "Job loss, no income in wave -1")
# treated subgroup and all never treated (the *_all panels keep every wave, for the stacked estimator)
sub_panel <- function(g, all = FALSE) {
  if (g %in% c("pinc25", "jobloss")) return(add_pol(panels[[paste0(g, if (all) "_all")]]))
  d <- panels[[paste0(base_of[[g]], if (all) "_all")]]
  add_pol(d[G == 0 | id %in% grp_ids[[g]]])
}


# 2. CS by subgroup ----------------------------------------------------------------------------------

ys <- c("distance", "like_max", "like_min", "gov_mean")
cells <- CJ(g = names(grp_lab), y = ys, sorted = FALSE)
fits <- parallel::mclapply(seq_len(nrow(cells)), function(i) {
  setDTthreads(1)
  try_est(cs_fit(sub_panel(cells$g[i]), cells$y[i], return_inf = TRUE), paste(cells$g[i], cells$y[i]))
}, mc.cores = n_cores, mc.preschedule = FALSE)
names(fits) <- cells[, paste(g, y, sep = "|")]
sds <- sapply(ys, function(y) sd(prep_outcome(add_pol(panels$jobloss), y)[G == 0][[y]]))   # never unemployed, as in 14
res <- rbindlist(lapply(names(fits), function(k) { f <- fits[[k]]; if (is.null(f)) return(NULL); s <- strsplit(k, "|", fixed = TRUE)[[1]]
  a <- lc(f, "Avg"); data.table(group = s[1], outcome = s[2], estimate = a$estimate, se = a$se, pre_p = pre_p(f), n_treated = f$n_treated) }))
res[, `:=`(p = 2 * pnorm(-abs(estimate / se)), est_sd = estimate / sds[outcome], label = grp_lab[group])]

# difference between two subgroups: per-person influence of the waves 0-8 average (V = crossprod(inf) / n^2)
avg_inf <- function(f) { w <- win_weights(f, windows$Avg); data.table(id = f$ids, psi = drop(f$inf[, names(w)] %*% w) / nrow(f$inf)) }
diff_test <- function(g1, g2, y) {
  f1 <- fits[[paste(g1, y, sep = "|")]]; f2 <- fits[[paste(g2, y, sep = "|")]]
  m <- merge(avg_inf(f1), avg_inf(f2), by = "id", all = TRUE, suffixes = c("1", "2"))
  m[is.na(psi1), psi1 := 0][is.na(psi2), psi2 := 0]
  stopifnot(abs(sqrt(sum(m$psi1^2)) - lc(f1, "Avg")$se) < 1e-6)
  d <- lc(f1, "Avg")$estimate - lc(f2, "Avg")$estimate; se <- sqrt(sum((m$psi1 - m$psi2)^2))
  data.table(contrast = paste(g1, "-", g2), outcome = y, diff = d, se = se, p = 2 * pnorm(-abs(d / se)))
}
diffs <- rbindlist(lapply(ys, function(y) rbind(diff_test("inc_kept", "inc_jobloss", y), diff_test("inc_kept", "inc_other", y),
                                                 diff_test("job_incdrop", "job_noincdrop", y))))


# 3. By intended vote in wave -1 ------------------------------------------------------------------
# stack_rows() and cell_did() as in 17_extensions.R (keep in sync); by_vote() as blame_by_vote() there

stack_rows <- function(d, y, base_cols = character()) {
  d <- prep_outcome(d, y)
  rbindlist(lapply(sort(unique(d[G > 1, G])), function(g) {
    b <- d[t == g - 1L & emp %in% TRUE, c("id", y, "t_cal", base_cols), with = FALSE]
    setnames(b, c(y, "t_cal"), c("y_b", "t_cal_b"))
    if (length(base_cols)) setnames(b, base_cols, paste0("baseline_", base_cols))
    x <- d[G %in% c(0L, g) & t != g - 1L & t - g >= event_window[1] & t - g <= event_window[2]]
    x <- x[t >= g | emp %in% TRUE][b, on = "id", nomatch = 0]
    x[, c(list(id = id, g = g, e = t - g, wave_t = t_cal + 7L, wave_b = t_cal_b + 7L, treated = G == g, dy = get(y) - y_b, y_b = y_b),
          if (length(base_cols)) setNames(mget(paste0("baseline_", base_cols)), base_cols) else list())]
  }))
}
cell_did <- function(rows, group_cols = character(), cell_cols = character(), w = NULL) {
  rows <- copy(rows); rows[, wt := if (is.null(w)) 1 else get(w)]
  cc <- c(group_cols, cell_cols, "g", "e")
  rows[, `:=`(n_t = sum(wt[treated]), n_c = sum(wt[!treated]),
              m_t = sum((wt * dy)[treated]) / sum(wt[treated]), m_c = sum((wt * dy)[!treated]) / sum(wt[!treated])), by = cc]
  rows <- rows[n_t > 0 & n_c > 0]
  cells <- unique(rows[, c(cc, "n_t", "n_c", "m_t", "m_c"), with = FALSE])[, cw := n_t / sum(n_t), by = c(group_cols, "e")]
  rows[cells, on = cc, cw := i.cw]
  rows[, psi := fifelse(treated, cw * wt * (dy - m_t) / n_t, -cw * wt * (dy - m_c) / n_c)]
  grp <- if (length(group_cols)) unique(rows[, ..group_cols]) else data.table(all = "all")
  lapply(split(seq_len(nrow(grp)), seq_len(nrow(grp))), function(i) {
    sel <- if (length(group_cols)) grp[i][rows, on = group_cols, nomatch = 0] else rows
    sc <- if (length(group_cols)) grp[i][cells, on = group_cols, nomatch = 0] else cells
    b <- sc[order(e), .(b = sum(cw * (m_t - m_c))), by = e]; b <- setNames(b$b, b$e)
    inf <- dcast(sel[, .(psi = sum(psi)), by = .(id, e)], id ~ e, value.var = "psi", fill = 0)
    M <- as.matrix(inf[, names(b), with = FALSE]); V <- crossprod(M); dimnames(V) <- list(names(b), names(b))
    list(group = if (length(group_cols)) grp[i] else NULL, b = b, V = V, n_treated = uniqueN(sel[treated == TRUE, id]))
  })
}
by_vote <- function(d) {
  d <- merge(d, vote_b, by = c("id", "wave"), all.x = TRUE)
  out <- list()
  r <- stack_rows(d, "gov_mean", base_cols = "vote_group")[!is.na(vote_group)]
  for (f in cell_did(r, "vote_group")) out[[paste("gov_mean", f$group$vote_group)]] <- c(f, list(outcome = "gov_mean", vote_group = f$group$vote_group))
  # own party: change in the rating of the party intended in wave -1, from wave -1 to wave t
  r <- stack_rows(d, "distance", base_cols = c("vote_group", "own_party"))[vote_group %in% c("governing", "other party")]
  rt <- th[, .(id = nomem_encr, wave = wave, own_party = party, rating)]
  r[rt, on = c("id", wave_t = "wave", "own_party"), r_t := i.rating][rt, on = c("id", wave_b = "wave", "own_party"), r_b := i.rating]
  r <- r[!is.na(r_t) & !is.na(r_b)][, dy := r_t - r_b]
  for (f in cell_did(r, "vote_group")) out[[paste("own_party", f$group$vote_group)]] <- c(f, list(outcome = "own_party", vote_group = f$group$vote_group))
  out
}
vg <- setdiff(names(grp_lab), "job_inc_unknown")
vfits <- parallel::mclapply(vg, function(g) { setDTthreads(1); try_est(by_vote(sub_panel(g, all = TRUE)), g) },
                            mc.cores = n_cores, mc.preschedule = FALSE)
names(vfits) <- vg
vres <- rbindlist(lapply(vg, function(g) rbindlist(lapply(vfits[[g]], function(f) {
  a <- lc(f, "Avg"); data.table(group = g, outcome = f$outcome, vote_group = f$vote_group, estimate = a$estimate, se = a$se,
                                pre_p = pre_p(f), n_treated = f$n_treated) }))))
vres[, `:=`(p = 2 * pnorm(-abs(estimate / se)), label = grp_lab[group])]

saveRDS(list(overlap = overlap, summary = res, diffs = diffs, by_vote = vres, sizes = sapply(grp_ids, length)), path_est("split", "split"))
options(width = 200)
print(overlap)
print(res[, .(label, outcome, est = round(estimate, 3), se = round(se, 3), p = round(p, 3), sd = round(est_sd, 2), pre = round(pre_p, 2), n_treated)][order(outcome, label)])
print(diffs[, .(contrast, outcome, diff = round(diff, 3), se = round(se, 3), p = round(p, 3))])
print(vres[, .(label, outcome, vote_group, est = round(estimate, 3), se = round(se, 3), p = round(p, 3), pre = round(pre_p, 2), n_treated)][order(outcome, vote_group, label)])

### END
