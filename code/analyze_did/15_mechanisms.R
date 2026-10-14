### APSR note: mechanisms and competing explanations (October 2026)
# 1. Channels: effects of a large income loss and of job loss on candidate channels between economic
#    loss and affective polarization, in the design of the note (Callaway-Sant'Anna, never-treated
#    controls, wave -1 as baseline, no covariates): economic situation, well-being, political
#    engagement, ideology and identity, political efficacy, attitudes toward immigrants,
#    redistribution, social connectedness, and party ratings (blame: governing parties, radical left
#    and far right, whether a governing party is the least liked). Squared distance is split exactly
#    into the governing and the other parties' parts; the governing parties' share of its increase gets
#    a delta-method standard error and is compared with first-order and baseline benchmarks.
# 2. Horse race: common explanations of affective polarization turned into events (e.g. becoming a
#    party adherent, a 2+ point drop in confidence in institutions, with a stable baseline) and
#    estimated with the same design on distance, among all 18-65 year olds. The events are changes in
#    attitudes, not shocks: they are endogenous (reverse causality, common shocks, regression to the
#    mean), so these estimates benchmark the size of the income-loss effect; they are not causal
#    effects of the explanations.
# 3. Mediation: difference-in-differences mediation with double machine learning (Huber & Oberhaensli
#    2026; causalweight::didmedDMLpanel with folds drawn by person, didmed_folds below) on stacked
#    cohort data. Outcome: distance in wave -1 (pre) and mean distance in waves 2-4 (post); mediator:
#    mean in waves 0-1 (timing A, before the outcome window) or 2-4 (timing B, with the outcome);
#    covariates: cohort dummies, covariates in wave -1 and the mediator at baseline; controls: never
#    treated in the same calendar waves; standard errors clustered by person (controls appear in
#    several cohorts' stacks). The indirect effect needs, besides conditional parallel trends, that
#    people with and without a loss who share the mediator value and the covariates would have followed
#    parallel trends without the loss (no unobserved confounders of the mediator-outcome relationship,
#    including ones the loss affects), and overlap. With several correlated mediators, the single-
#    mediator estimates omit the others; the joint model is the main result.
#
# Timing of the modules (00_setup.R): the politics module of wave w is fielded from December of
# 1999 + w, and income (the treatment) is measured in the month of that interview, so a loss in wave g
# happens between the politics interviews of waves g - 1 and g. The income module (June) and the
# personality module (May; November in waves 14-15) of wave w lie between the politics interviews of
# waves w and w + 1. As outcomes, these items use wave g - 2 as the reference (shift_between); as
# events (horse race), G is the module wave in which the change is first seen, so the reference
# politics wave certainly precedes it; as mediators, their baseline is the module before the interview
# of wave g - 1. See between_items.
#
# Needs the local data files data/liss_politics_to26.csv, data/liss_income_to26.csv and
# data/liss_to26.csv (licensed microdata, never in git) and the panels (01_sample.R).
# Saves results/estimates/mechanisms/{channels,horse,mediation}.rds (each part is cached per fit;
# DID_MECH_REFIT=1 redoes all fits). 14_apsr_note.R turns them into the note's exhibits and numbers.
# Parts to run: Sys.setenv(DID_MECH_PARTS = "channels,horse,mediation") (default: all).

rm(list = ls())
here::i_am("code/analyze_did/15_mechanisms.R")
Sys.setenv(DID_NO_FIGURES = "1")   # estimation only; 14_apsr_note.R draws the figures
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(did, causalweight)

refit <- Sys.getenv("DID_MECH_REFIT") == "1"
parts <- if (Sys.getenv("DID_MECH_PARTS") != "") strsplit(Sys.getenv("DID_MECH_PARTS"), ",")[[1]] else
  c("channels", "horse", "mediation")
tr_lab <- c(pinc25 = "Large income loss", jobloss = "Job loss")


# Data ------------------------------------------------------------------------

in_range <- function(x, lo, hi) fifelse(x >= lo & x <= hi, as.numeric(x), NA_real_)
dedup <- function(d) d[!duplicated(d[, .(nomem_encr, wave)])]

# politics module (fielded waves 8-14 and 16-26)
parties <- c("VVD", "D66", "PVV", "CDA", "SP", "PvdA", "GL", "PvdD", "CU", "SGP", "FvD", "TON", "Volt",
             "JA21", "DENK", "50PLUS", "BBB", "BIJ1", "VNL", "NSC", "GL_PvdA")
pol <- dedup(fread(here("data", "liss_politics_to26.csv"),
  select = c("nomem_encr", "wave", "lr_scale", "politics_interest", "news_interest", "news_internet", "partisan",
             "parties_dontcare_myopinion", "noinfluence_on_pol", "foreign_toomany", "pref_immigrant_adapt",
             "pref_redistribution", "satisfac_econ", "confidence_index", "distance", paste0("thermo_", parties))))
pol[, `:=`(
  lr_extreme   = abs(in_range(lr_scale, 0, 10) - 5),                   # 0 (centre) to 5 (0 or 10)
  pol_interest = 3 - in_range(politics_interest, 1, 3),                # 0 not, 1 fairly, 2 very interested
  news_int     = 3 - in_range(news_interest, 1, 3),
  news_online  = in_range(news_internet, 0, 1),                        # follows the news on the internet
  adherent     = 2 - in_range(partisan, 1, 2),                         # adherent of a party (from wave 12)
  alienated    = 2 - in_range(parties_dontcare_myopinion, 1, 2),       # "parties are only interested in my vote, not my opinion": true
  no_influence = 2 - in_range(noinfluence_on_pol, 1, 2),               # "people like me have no influence at all on government policy": true
  foreigners   = in_range(foreign_toomany, 1, 5),                      # "too many people of foreign origin in NL", 5 fully agree
  immig_adapt  = in_range(pref_immigrant_adapt, 1, 5),                 # 1 keep own customs ... 5 adapt entirely
  redist       = in_range(pref_redistribution, 1, 5),                  # 1 income differences should increase ... 5 decrease
  econ_sat     = in_range(satisfac_econ, 0, 10))]                      # satisfaction with the Dutch economy

# party ratings (0-10); cabinets at the time of fieldwork (December to March). Caretaker cabinets that
# consist of the coalition parties are kept (Rutte III in 2020/21, Rutte IV in 2023/24). Wave 26
# (2025/26) is left out of the governing-party outcomes: the PVV and NSC had left the Schoof cabinet in
# 2025, so the caretaker cabinet no longer matched the coalition, and a new government was being formed
# after the October 2025 election. Ratings of single parties (SP, PVV) cover all waves.
th <- melt(pol[, c("nomem_encr", "wave", paste0("thermo_", parties)), with = FALSE], id.vars = c("nomem_encr", "wave"),
           variable.name = "party", value.name = "rating", variable.factor = FALSE)
th[, `:=`(party = sub("thermo_", "", party), rating = in_range(rating, 0, 10))]
th <- th[!is.na(rating)]
cabinet <- rbind(
  CJ(wave = 8:10,              party = c("CDA", "PvdA", "CU")),          # Balkenende IV
  CJ(wave = 11:12,             party = c("VVD", "CDA")),                 # Rutte I (PVV support agreement, not in cabinet)
  CJ(wave = c(13:14, 16:17),   party = c("VVD", "PvdA")),                # Rutte II
  CJ(wave = 18:24,             party = c("VVD", "CDA", "D66", "CU")),    # Rutte III and IV
  CJ(wave = 25,                party = c("PVV", "VVD", "NSC", "BBB")))   # Schoof
main_party <- data.table(wave = c(8:14, 16:25), party = c(rep("CDA", 3), rep("VVD", 13), "PVV"))  # largest party in cabinet
th[cabinet, on = c("wave", "party"), gov := TRUE][is.na(gov), gov := FALSE]
ratings <- th[wave %in% cabinet$wave, .(
  gov_mean  = if (any(gov)) mean(rating[gov]) else NA_real_,
  least_gov = if (any(gov) && .N >= 2) as.numeric(any(gov & rating == min(rating))) else NA_real_), by = .(nomem_encr, wave)]
# distance (Wagner 2021) as in the build (01_LISS_politics_transformation.R): vote shares of the last
# election before fieldwork, rescaled over the rated parties; distance of each party's rating from the
# highest rating. dist_opp: the same among the parties not in government, so that a fall in the
# ratings of governing parties cannot raise it.
vs <- fread(here("data", "raw", "vote_shares.csv"))[, .(
  party = fcase(party == "GrLinks", "GL", party %in% c("TROTS", "DPK"), "TON", party == "GL/PvdA", "GL_PvdA", default = party),
  election_year = as.integer(election_year), share)]
th[, election_year := election_before(wave)][vs, on = c("party", "election_year"), share := i.share]
# (as in the build: at least two rated parties with a vote share; the highest rating over all rated parties)
wdist <- function(r, s) { ok <- !is.na(s); if (sum(ok) < 2) return(NA_real_); w <- s[ok] / sum(s[ok]); sqrt(sum(w * (r[ok] - max(r))^2)) }
chk <- th[, .(d = wdist(rating, share)), by = .(nomem_encr, wave)][pol, on = c("nomem_encr", "wave"), nomatch = 0]
stopifnot(chk[, identical(is.na(d), is.na(distance)) && max(abs(d - distance), na.rm = TRUE) < 1e-8])   # reproduces the build
# exact decomposition: squared distance = sum over parties of weight x (rating - highest rating)^2,
# split into the governing parties' part and the other parties' part (weights over all rated parties)
# gap1: weight x (highest rating - rating), for the first-order benchmark: if a loss cooled feelings
# toward every party other than the most-liked one by the same amount, the governing parties' share of
# the increase in squared distance would be their share of sum(weight x gap)
th[, `:=`(gap2 = share / sum(share, na.rm = TRUE) * (rating - max(rating))^2,
          gap1 = share / sum(share, na.rm = TRUE) * (max(rating) - rating)), by = .(nomem_encr, wave)]
dec <- th[wave %in% cabinet$wave, if (sum(!is.na(share)) >= 2) .(dist2 = sum(gap2, na.rm = TRUE),
          dist2_gov = sum(gap2[gov], na.rm = TRUE), dist2_opp = sum(gap2[!gov], na.rm = TRUE),
          gap1_gov = sum(gap1[gov], na.rm = TRUE), gap1 = sum(gap1, na.rm = TRUE)), by = .(nomem_encr, wave)]
stopifnot(dec[chk, on = c("nomem_encr", "wave"), nomatch = 0][, max(abs(sqrt(dist2) - d), na.rm = TRUE)] < 1e-8)
ratings <- Reduce(function(a, b) merge(a, b, by = c("nomem_encr", "wave"), all = TRUE), list(
  ratings, dec,
  th[wave %in% cabinet$wave & !gov, .(dist_opp = wdist(rating, share)), by = .(nomem_encr, wave)],
  th[main_party, on = c("wave", "party"), nomatch = 0][, .(nomem_encr, wave, main_gov = rating)],
  th[party == "SP", .(nomem_encr, wave, rate_SP = rating)],
  th[party == "PVV", .(nomem_encr, wave, rate_PVV = rating)]))

# income module (June, waves 8-26)
inc <- dedup(fread(here("data", "liss_income_to26.csv"),
  select = c("nomem_encr", "wave", "sat_fin_sit", "hh_fin_sat", "happy", "life_ladder")))
inc[, `:=`(fin_sat   = in_range(sat_fin_sit, 0, 10),          # satisfaction with own financial situation
           hh_finsit = in_range(hh_fin_sat, 1, 5),            # 1 accumulating debts ... 5 a lot of money to spare
           happy     = in_range(happy, 0, 10),
           life_sat  = in_range(life_ladder, 0, 10))]

# personality module (May; November in waves 14-15)
soc <- dedup(fread(here("data", "liss_to26.csv"), select = c("nomem_encr", "wave", "feel_connected_to_others")))
soc[, connected := in_range(feel_connected_to_others, 1, 7)]

pol_items <- c("lr_extreme", "pol_interest", "news_int", "news_online", "adherent", "alienated", "no_influence",
               "foreigners", "immig_adapt", "redist", "econ_sat")
inc_items <- c("fin_sat", "hh_finsit", "happy", "life_sat")
rating_items <- c("gov_mean", "main_gov", "rate_SP", "rate_PVV", "least_gov", "dist_opp", "dist2", "dist2_gov", "dist2_opp", "gap1_gov", "gap1")
# items fielded between politics interviews (income module in June, personality module in May): their
# wave g - 1 can already follow a loss in wave g, so they get the reference wave g - 2 (shift_between()
# and the explanation in 00_setup.R)
between_items <- c(inc_items, "connected", between_outcomes)
extra <- Reduce(function(a, b) merge(a, b, by = c("nomem_encr", "wave"), all = TRUE), list(
  pol[, c("nomem_encr", "wave", pol_items, "confidence_index"), with = FALSE],
  ratings, inc[, c("nomem_encr", "wave", inc_items), with = FALSE], soc[, .(nomem_encr, wave, connected)]))
setnames(extra, "nomem_encr", "id")
rm(pol, th, inc, soc)

panels <- load_panels()
add_extra <- function(d) {
  d <- merge(d[, !intersect(setdiff(names(extra), c("id", "wave")), names(d)), with = FALSE], extra,
             by = c("id", "wave"), all.x = TRUE)
  setorder(d, id, t)[]
}
px <- lapply(panels[names(tr_lab)], add_extra)

# outcome SDs among the never treated of the job-loss panel (as in 14_apsr_note.R)
sd_of <- function(y) sd(prep_outcome(px$jobloss, y)[G == 0][[y]])
sd_dist <- sd_of("distance")

# window averages (lc() and pre_p() in 00_setup.R): waves 0-1, 2-4 and 0-8
summarise_fit <- function(f) rbindlist(lapply(c("Early", "Mid", "Avg"), function(nm) {
  x <- lc(f, nm); data.table(window = nm, estimate = x$estimate, se = x$se)
}))[, `:=`(pre_p = pre_p(f), n_treated = f$n_treated)][]

# cached fits: one list per part, tagged with the panels and definitions (cache_tag() in 00_setup.R);
# missing fits run in parallel batches of n_cores (forked workers, one data.table thread each) and the
# cache is saved after every batch
cached_fits <- function(name, keys, fun) {
  path <- path_est("mechanisms", name)
  cache <- if (file.exists(path) && !refit) readRDS(path) else list()
  if (!identical(cache$tag, cache_tag())) cache <- list(tag = cache_tag())
  todo <- keys[vapply(keys, function(k) is.null(cache[[k]]), logical(1))]   # missing or failed
  for (batch in split(todo, ceiling(seq_along(todo) / n_cores))) {
    message(name, ": ", paste(batch, collapse = ", "))
    cache[batch] <- parallel::mclapply(batch, function(k) { setDTthreads(1); try_est(fun(k), k) },
                                       mc.cores = n_cores, mc.preschedule = FALSE)
    saveRDS(cache, path)
  }
  cache
}


# 1. Channels ---------------------------------------------------------------------

chan_spec <- tribble(
  ~outcome,            ~group,                    ~label,
  "fin_sat",           "Economic situation",      "Satisfaction with own finances (0-10)",
  "hh_finsit",         "Economic situation",      "Household finances (1 debts ... 5 money to spare)",
  "econ_sat",          "Economic situation",      "Satisfaction with the Dutch economy (0-10)",
  "happy",             "Well-being",              "Happiness (0-10)",
  "life_sat",          "Well-being",              "Life satisfaction (0-10)",
  "pol_interest",      "Engagement",              "Political interest (0-2)",
  "news_int",          "Engagement",              "Interest in news (0-2)",
  "news_online",       "Engagement",              "Follows news online (0/1)",
  "lr_extreme",        "Ideology and identity",   "Ideological extremity (0-5)",
  "adherent",          "Ideology and identity",   "Party adherent (0/1)",
  "confidence_index",  "Trust and efficacy",      "Confidence in institutions (0-10)",
  "alienated",         "Trust and efficacy",      "Parties only want my vote (0/1)",
  "no_influence",      "Trust and efficacy",      "No influence on government (0/1)",
  "generalized_trust", "Trust and efficacy",      "Generalized trust (0-10)",
  "foreigners",        "Immigration and redistribution", "Too many foreigners (1-5)",
  "immig_adapt",       "Immigration and redistribution", "Immigrants should adapt (1-5)",
  "redist",            "Immigration and redistribution", "Income differences should shrink (1-5)",
  "connected",         "Social connectedness",    "Feels connected to others (1-7)",
  "gov_mean",          "Party ratings",           "Governing parties (mean rating)",
  "main_gov",          "Party ratings",           "Main governing party",
  "rate_SP",           "Party ratings",           "Radical-left (SP)",
  "rate_PVV",          "Party ratings",           "Far-right (PVV)",
  "least_gov",         "Party ratings",           "Least-liked party governs (0/1)",
  "dist_opp",          "Party ratings",           "Distance among parties not in government",
  "dist2",             "Distance decomposition",  "Distance squared",
  "dist2_gov",         "Distance decomposition",  "Distance squared: governing parties' part",
  "dist2_opp",         "Distance decomposition",  "Distance squared: other parties' part")
setDT(chan_spec)

if ("channels" %in% parts) {
  keys <- c(CJ(treatment = names(tr_lab), outcome = c(chan_spec$outcome, "distance"))[, paste(treatment, outcome, sep = "|")])
  # the two parts of squared distance keep their influence functions (same people, same order), so
  # that the share of the governing parties in the increase gets a standard error
  fits_chan <- cached_fits("channels", keys, function(k) {
    s <- strsplit(k, "|", fixed = TRUE)[[1]]
    cs_fit(if (s[2] %in% between_items) shift_between(px[[s[1]]]) else px[[s[1]]], s[2],
           return_inf = s[2] %in% c("dist2_gov", "dist2_opp"))
  })
  keys_fit <- setdiff(names(fits_chan), "tag")
  chan <- rbindlist(lapply(keys_fit, function(k) {
    if (is.null(fits_chan[[k]])) return(NULL)
    s <- strsplit(k, "|", fixed = TRUE)[[1]]
    summarise_fit(fits_chan[[k]])[, `:=`(treatment = s[1], outcome = s[2])]
  }))
  chan[, sd := vapply(outcome, sd_of, numeric(1))][, `:=`(est_sd = estimate / sd, se_sd = se / sd)]
  chan <- chan_spec[chan, on = "outcome"]

  # governing parties' share of the increase in squared distance, a / (a + b), with a delta-method
  # standard error from the person-level influence functions of the two parts (clustered by person)
  gov_share <- rbindlist(lapply(names(tr_lab), function(tr) rbindlist(lapply(c("Avg", "Mid"), function(nm) {
    fa <- fits_chan[[paste(tr, "dist2_gov", sep = "|")]]; fb <- fits_chan[[paste(tr, "dist2_opp", sep = "|")]]
    stopifnot(identical(fa$ids, fb$ids))
    colnames(fa$inf) <- names(fa$b); colnames(fb$inf) <- names(fb$b)
    w <- win_weights(fa, windows[[nm]])
    ia <- drop(fa$inf[, names(w), drop = FALSE] %*% w); ib <- drop(fb$inf[, names(w), drop = FALSE] %*% w)
    a <- sum(fa$b[names(w)] * w); b <- sum(fb$b[names(w)] * w)
    ish <- (b * ia - a * ib) / (a + b)^2
    data.table(treatment = tr, window = nm, share = a / (a + b), se = sqrt(sum(ish^2)) / length(ish))
  }))))
  # benchmarks: the governing parties' share of squared distance (gap^2) and of sum(weight x gap) among
  # the treated in the wave before the loss, and of squared distance among the never treated
  bench <- rbindlist(lapply(names(tr_lab), function(tr) {
    d <- prep_outcome(px[[tr]], "dist2")
    x <- d[G > 1 & t == G - 1L]
    data.table(treatment = tr, sq_treated = x[, sum(dist2_gov) / sum(dist2)], first_order = x[, sum(gap1_gov) / sum(gap1)],
               sq_never = d[G == 0, sum(dist2_gov) / sum(dist2)])
  }))
  saveRDS(list(fits = fits_chan[keys_fit], summary = chan, spec = chan_spec, gov_share = gov_share, bench = bench,
               gov_share_base = bench[treatment == "jobloss", sq_never]),
          path_est("mechanisms", "channels_summary"))

  message("channels (SD units):")
  print(dcast(chan[window %in% c("Early", "Mid")], label ~ treatment + window,
              value.var = c("est_sd"), fun.aggregate = function(x) round(x, 3)))
  message("governing parties' share of the increase in squared distance:"); print(gov_share); print(bench)
}


# 2. Horse race ---------------------------------------------------------------------
# Events: the first wave in which an item moves by at least `delta` (in the direction of the sign)
# from the person's value in the directly preceding wave in which the item was fielded. Absorbing,
# like the treatments of the note. To limit regression to the mean after a transitory deviation, the
# item must not have moved by `delta` or more into that preceding wave (stable baseline); people
# whose first change does not meet this, or follows an unobserved wave, are dropped, as are people
# never observed in two consecutive fielded waves of the item (their event status is unknown).
# Timing: G is the wave in which the change is first seen; for items fielded between politics
# interviews (income and personality modules), the reference politics wave G - 1 therefore certainly
# precedes the change (00_setup.R). Sample: everyone aged 18-65 (no employment restriction; the
# income-loss and job-loss estimates come from the labor-force panels of the note).

events <- tribble(
  ~event,         ~item,              ~delta, ~label,
  "extreme",      "lr_extreme",            2, "Ideological extremity rises 2+ points",
  "adherent",     "adherent",              1, "Becomes a party adherent",
  "interest",     "pol_interest",          1, "Political interest rises",
  "online",       "news_online",           1, "Starts following news online",
  "distrust",     "confidence_index",     -2, "Confidence in institutions falls 2+ points",
  "alienated",    "alienated",             1, "Comes to see parties as only wanting votes",
  "foreigners",   "foreigners",            2, "'Too many foreigners' rises 2+ points",
  "finsat",       "fin_sat",              -2, "Satisfaction with own finances falls 2+ points",
  "econ",         "econ_sat",             -2, "Satisfaction with the economy falls 2+ points",
  "happy",        "happy",                -2, "Happiness falls 2+ points",
  "isolated",     "connected",            -2, "Feeling connected to others falls 2+ points")
setDT(events)

if ("horse" %in% parts) {
  full <- dedup(fread(here("data", "liss_to26.csv"), select = c("nomem_encr", "wave", "age", "distance")))
  full <- full[age >= 18 & age <= 65][, `:=`(id = nomem_encr, t = as.integer(wave - 7))][, nomem_encr := NULL]
  full <- merge(full, extra, by = c("id", "wave"), all.x = TRUE)
  setorder(full, id, t)

  event_panel <- function(item, delta) {
    e <- full[!is.na(get(item)), .(id, wave, t, v = get(item))]
    fielded <- sort(unique(e$wave))
    e[, prev := c(NA_integer_, fielded)[match(wave, fielded)]]   # previous fielded wave (NA for the first)
    e[, prev2 := c(NA_integer_, fielded)[match(prev, fielded)]]
    e[, `:=`(v_prev = v[match(prev, wave)], v_prev2 = v[match(prev2, wave)]), by = id]
    e[, hit := if (delta > 0) v - v_prev >= delta else v - v_prev <= delta]
    at_risk <- e[!is.na(hit), unique(id)]
    first <- e[hit %in% TRUE][order(t)][, .SD[1], by = id]
    unstable <- first[is.na(v_prev2) | abs(v_prev - v_prev2) >= abs(delta), id]
    g <- first[!id %in% unstable, .(id, G = t)]
    d <- full[id %in% setdiff(at_risk, unstable), .(id, t, wave, distance)]
    d[g, on = "id", G := i.G][is.na(G), G := 0L]
    d[, D := as.integer(G > 0 & t >= G)][]
  }
  fits_horse <- cached_fits("horse", events$event, function(k) {
    s <- events[event == k]; d <- event_panel(s$item, s$delta)
    f <- cs_fit(d, "distance")
    f$n_units <- uniqueN(prep_outcome(d, "distance")$id); f
  })
  horse <- rbindlist(lapply(events$event, function(k) {
    f <- fits_horse[[k]]; if (is.null(f)) return(NULL)
    summarise_fit(f)[, `:=`(event = k, n_units = f$n_units)]
  }))
  horse <- events[horse, on = "event"]
  # the two treatments of the note (labor-force panels; main specification, fitted in part 1)
  fits_main <- readRDS(path_est("mechanisms", "channels"))
  for (tr in names(tr_lab)) {
    f <- fits_main[[paste(tr, "distance", sep = "|")]]
    horse <- rbind(horse, summarise_fit(f)[, `:=`(event = tr, item = NA, delta = NA, label = tr_lab[[tr]],
                                                   n_units = f$n_units)], fill = TRUE)
  }
  horse[, `:=`(share_treated = n_treated / n_units, est_sd = estimate / sd_dist, se_sd = se / sd_dist)]
  # implied shift in average distance (SD units) if every event's effect were causal: ATT x share treated
  horse[, contribution_sd := est_sd * share_treated]
  saveRDS(list(fits = fits_horse, summary = horse, events = events), path_est("mechanisms", "horse_summary"))
  message("horse race (distance, SD units):")
  print(dcast(horse, label + share_treated + pre_p ~ window, value.var = "est_sd", fun.aggregate = function(x) round(x, 3)))
}


# 3. Mediation ---------------------------------------------------------------------

med_spec <- tribble(
  ~mediator,          ~module,   ~label,                                  ~mechanism,
  "confidence_index", "politics", "Confidence in institutions",           "Institutional distrust",
  "satisfaction_gov", "politics", "Satisfaction with the government",     "Government blame",
  "alienated",        "politics", "Parties only want my vote",            "Political alienation",
  "fin_sat",          "between",  "Satisfaction with own finances",       "Financial strain",
  "econ_sat",         "politics", "Satisfaction with the economy",        "Economic pessimism",
  "happy",            "between",  "Happiness",                            "Emotional distress",
  "foreigners",       "politics", "Too many foreigners",                  "Outgroup threat",
  "redist",           "politics", "Income differences should shrink",     "Redistribution demand",
  "lr_extreme",       "politics", "Ideological extremity",                "Ideological extremity",
  "pol_interest",     "politics", "Political interest",                   "Political engagement",
  "connected",        "between",  "Feels connected to others",            "Social isolation",
  "adherent",         "politics", "Party adherent",                       "Partisan identity")
setDT(med_spec)
joint_meds <- setdiff(med_spec$mediator, "adherent")   # adherence is asked from wave 12 on only

# one row per person and cohort: treated (G = g) and never treated observed in wave g - 1 and in at
# least one of waves g + 2 ... g + 4 (event time counts the waves with distance, as in the note).
# Covariates are measured in wave g - 1 for everyone in the stack (the same wave as the baseline
# mediator). Mediator windows (calendar waves of the politics interviews of the stated event times):
#   A: waves 0-1, after the loss and before the outcome window; for items fielded between politics
#      interviews, the modules fielded after the interviews of waves 0 and 1 (certainly after the loss)
#   B: waves 2-4, together with the outcome
#   A2 (sensitivity, items fielded between politics interviews only): the modules fielded after the
#      interviews of waves -1 and 0, i.e. event times 0-1 in the channel analysis; the first of these can
#      precede the loss
# Baseline mediator: wave g - 1; for items fielded between politics interviews, the module before the
# interview of wave g - 1 (it certainly precedes the loss).
stack_cohorts <- function(d, meds) {
  dd <- prep_outcome(d, "distance")
  wave_of <- sort(unique(dd$t_cal)) + 7L; n_t <- length(wave_of)
  val <- d[, c("id", "wave", meds), with = FALSE]
  med_mean <- function(ids, waves, m) {
    v <- val[.(ids), on = "id"][wave %in% waves, .(x = mean(get(m), na.rm = TRUE)), by = id]
    v[.(ids), on = "id"][is.nan(x), x := NA_real_]$x
  }
  rbindlist(lapply(sort(unique(dd[G > 0, G])), function(g) {
    if (g < 2 || g + 2 > n_t) return(NULL)
    post <- (g + 2):min(g + 4, n_t)
    u <- dd[G %in% c(0L, g)]
    s <- u[t == g - 1, .(id, treated = as.integer(G > 0), y0 = distance,
                        agegrp = cut(age, c(17, 29, 44, 54, 65), labels = c("18-29", "30-44", "45-54", "55-65")),
                        male, educ = factor(education_cat), hhsize = pmin(no_hh, 5), inccat = net_monthly_income_cat)]
    s <- s[u[t %in% post, .(y1 = mean(distance)), by = id], on = "id", nomatch = 0]
    s[, cohort := g]
    for (m in meds) {
      btw <- med_spec[mediator == m, module] == "between"
      s[, (paste0(m, "_base")) := med_mean(id, if (btw) wave_of[g - 1] - 1L else wave_of[g - 1], m)]
      s[, (paste0(m, "_A")) := med_mean(id, wave_of[g:min(g + 1, n_t)], m)]
      s[, (paste0(m, "_A2")) := med_mean(id, if (btw) wave_of[(g - 1):g] else wave_of[g:min(g + 1, n_t)], m)]
      s[, (paste0(m, "_B")) := med_mean(id, wave_of[post], m)]
    }
    s
  }))
}

# causalweight::didmedDMLpanel (version 1.1.6), with two changes. (1) Cross-fitting folds are drawn by
# person: people without a loss appear in several stacks, and with folds drawn by row the same person
# would be in the training and the evaluation fold. (2) It also returns the share of the indirect
# effect in the total effect, with a delta-method standard error from the scores. Standard errors are
# clustered by person (CR2, as in causalweight). The estimator and its scores are otherwise unchanged.
didmed_folds <- function(y0, y1, d, m, x, cluster, MLmethod = "parametric", trim = 0.05, k = 3) {
  MLfunct <- causalweight:::MLfunct
  ydiff <- y1 - y0
  ybin <- 1 * (length(unique(ydiff)) == 2 & min(ydiff) == 0 & max(ydiff) == 1)
  controls <- data.frame(x); dt <- 1 * (d == 1); dc <- 1 * (d == 0)
  set.seed(1)
  ids <- unique(cluster); fold_of <- setNames(sample(rep_len(seq_len(k), length(ids))), ids)
  fold <- unname(fold_of[as.character(cluster)])
  param <- vector("list", k)
  for (i in seq_len(k)) {
    te <- which(fold == i); tr <- which(fold != i)
    Xtr <- data.frame(1, controls)[tr, ]; Xte <- data.frame(1, controls)[te, ]
    Xmtr <- data.frame(1, m, controls)[tr, ]; Xmte <- data.frame(1, m, controls)[te, ]
    pr <- function(fit, newx) drop(predict(fit, newx, onlySL = TRUE)$pred)
    param[[i]] <- data.frame(row = te,
      mudc  = pr(MLfunct(y = ydiff[tr], x = Xtr, d1 = dc[tr], MLmethod = MLmethod, ybin = ybin), Xte),
      pidt  = pr(MLfunct(y = dt[tr], x = Xtr, MLmethod = MLmethod, ybin = 1), Xte),
      pidc  = pr(MLfunct(y = dc[tr], x = Xtr, MLmethod = MLmethod, ybin = 1), Xte),
      mudcm = pr(MLfunct(y = ydiff[tr], x = Xmtr, d1 = dc[tr], MLmethod = MLmethod, ybin = ybin), Xmte),
      pidtm = pr(MLfunct(y = dt[tr], x = Xmtr, MLmethod = MLmethod, ybin = 1), Xmte),
      pidcm = pr(MLfunct(y = dc[tr], x = Xmtr, MLmethod = MLmethod, ybin = 1), Xmte))
  }
  p <- do.call(rbind, param); p <- p[order(p$row), ]
  p <- cbind(dt, dc, y1, y0, ydiff, cl = cluster, p)
  trimmed <- (p$pidc < trim * p$dc) | (p$pidcm < trim * p$dc)
  p <- p[!trimmed, ]
  for (v in c("pidt", "pidc", "pidtm", "pidcm")) p[[v]] <- ifelse(p[[v]] == 0, .Machine$double.eps, p[[v]])
  n <- nrow(p)
  ydcmdc <- n * (p$dt * (p$y0 + p$mudc) / sum(p$dt) + (p$ydiff - p$mudc) * (p$dc * p$pidt / p$pidc) / sum(p$dc * p$pidt / p$pidc))
  ydcmdt <- n * (p$dt * (p$y0 + p$mudcm) / sum(p$dt) + (p$ydiff - p$mudcm) * (p$dc * p$pidtm / p$pidcm) / sum(p$dc * p$pidtm / p$pidcm))
  ydtmdt <- n * (p$dt * p$y1 / sum(p$dt))
  sc <- list(total = ydtmdt - ydcmdc, direct = ydtmdt - ydcmdt, indirect = ydcmdt - ydcmdc)
  est <- vapply(sc, mean, numeric(1))
  share <- est[["indirect"]] / est[["total"]]
  sc$share <- (sc$indirect - share * sc$total) / est[["total"]]   # linearized score of the share
  se <- vapply(sc, function(z) clubSandwich::coef_test(lm(z ~ 1), vcov = "CR2", cluster = p$cl)$SE, numeric(1))
  list(estimate = c(est, share = share), se = se, trimmed = sum(trimmed))
}

# Nuisance models: "parametric" (OLS for the outcome trend, logit for the propensities) in the main
# runs; the covariates are few (cohort dummies, five covariates, baseline mediators) and the sample
# large, so the lasso adds little, and it is ~100 times slower (causalweight wraps cv.glmnet in a
# 10-fold super learner). The lasso is run for the joint models as a check. Income category enters
# linearly, as in the note's covariate check (as 13 dummies, its sparse top and bottom cells nearly
# separate the treated in the logit propensity models).
run_med <- function(s, meds, timing, ml) {
  mcols <- paste0(meds, "_", timing); bcols <- paste0(meds, "_base")
  covs <- c("agegrp", "male", "educ", "hhsize", "inccat")
  s <- s[complete.cases(s[, c("y0", "y1", mcols, bcols, covs), with = FALSE])]
  s <- s[cohort %in% s[treated == 1, unique(cohort)]]          # cohorts with treated people only
  x <- model.matrix(~ factor(cohort) + agegrp + male + factor(educ) + hhsize + inccat, data = s)[, -1]
  x <- cbind(x, as.matrix(s[, ..bcols]))
  out <- didmed_folds(y0 = s$y0, y1 = s$y1, d = s$treated, m = as.matrix(s[, ..mcols]), x = x, cluster = s$id, MLmethod = ml)
  data.table(effect = names(out$estimate), estimate = unname(out$estimate), se = unname(out$se),
             n = nrow(s), n_treated = sum(s$treated), n_persons = uniqueN(s$id), trimmed = out$trimmed)
}

if ("mediation" %in% parts) {
  between_meds <- med_spec[module == "between", mediator]
  med_sets <- c(setNames(as.list(med_spec$mediator), med_spec$mediator), list(joint = joint_meds))
  keys <- rbind(CJ(treatment = names(tr_lab), set = names(med_sets), timing = c("A", "B"), ml = "parametric", sorted = FALSE),
                CJ(treatment = names(tr_lab), set = c(between_meds, "joint"), timing = "A2", ml = "parametric", sorted = FALSE),
                CJ(treatment = names(tr_lab), set = "joint", timing = c("A", "B"), ml = "lasso", sorted = FALSE))[
    , paste(treatment, set, timing, ml, sep = "|")]
  stacks <- lapply(px, stack_cohorts, meds = med_spec$mediator)
  fits_med <- cached_fits("mediation", keys, function(k) {
    s <- strsplit(k, "|", fixed = TRUE)[[1]]
    run_med(stacks[[s[1]]], med_sets[[s[2]]], s[3], s[4])
  })
  med <- rbindlist(lapply(setdiff(names(fits_med), "tag"), function(k) {
    if (is.null(fits_med[[k]])) return(NULL)
    s <- strsplit(k, "|", fixed = TRUE)[[1]]
    fits_med[[k]][, `:=`(treatment = s[1], set = s[2], timing = s[3], ml = s[4])]
  }))
  med[effect != "share", `:=`(est_sd = estimate / sd_dist, se_sd = se / sd_dist)]
  saveRDS(list(fits = fits_med, summary = med, spec = med_spec), path_est("mechanisms", "mediation_summary"))
  # total effects (each mediator's complete cases) against the main estimate over waves 2-4
  fits_main <- readRDS(path_est("mechanisms", "channels"))
  cs_mid <- sapply(names(tr_lab), function(tr) lc(fits_main[[paste(tr, "distance", sep = "|")]], "Mid")$estimate)
  message("total effects by mediator sample (main estimate, waves 2-4: ", paste(names(cs_mid), round(cs_mid, 3), collapse = ", "), "):")
  print(med[effect == "total", .(min = round(min(estimate), 3), max = round(max(estimate), 3), n_min = min(n)), by = .(treatment, ml)])
  message("mediation (share of the total effect, SE):")
  print(med[effect == "share", .(treatment, set, timing, ml, share = round(estimate, 2), se = round(se, 2))][order(treatment, set, timing)])
}

### END
