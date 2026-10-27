### Extensions (October 2026): measurement, incumbent blame, treatments, inference
# Analyses requested after the audit; 14_apsr_note.R turns the saved summaries into SI exhibits. Four parts:
# 1. Measurement: distance and spread over a constant party set with fixed weights (Kasper,
#    Schumacher & Bakker 2025); leader-based distance and spread and the rating of the prime
#    minister vs. opposition leaders (Reiljan et al. 2024); scale-use variants (distance relative to
#    the person's spread of ratings, highest minus median rating, mean rating, number of parties rated
#    2 or lower, number of parties rated); Reiljan's partisan affect (API, partisans only); confidence in
#    six non-political institutions (each, mean, spread, highest and lowest) as a scale-use anchor.
# 2. Incumbent blame: effects by intended vote in wave -1 (governing party, other party, none) on
#    distance, the governing parties' ratings and the rating of one's own party; party-level ratings
#    when the party governs vs. when it does not; vote intention, abstention and protest; anger
#    (hostile, irritable), anxiety (afraid, nervous), self-esteem and loneliness.
# 3. Treatments: job loss with vs. without unemployment benefit, and by the reason for leaving the job
#    (company closure or reorganization, other dismissal, other or no answer; Work and Schooling); the
#    partner's job loss and income loss as treatments; size of the income loss (dose bins).
# 4. Inference: equivalence bounds for the pre-trends, attrition weighting.
# 5. Selection: within-person comparisons within the same sector and occupation before the loss.
#
# Designs: CS = the note's main specification (cs_fit in 00_setup.R). Stacked = the within-person
# estimator of 14_apsr_note.R (one employment rule for treated and controls), generalized here to
# subgroups defined in wave -1, party-level cells, and inverse-probability weights.
# Needs data/liss_to26.csv, data/liss_politics_to26.csv, data/liss_income_to26.csv, the raw LISS
# politics, personality, social integration, background and Work and Schooling files (LISS_RAW) and
# the panels (01); does not read 14's results. Saves results/estimates/extensions/*.rds (aggregates only).
# Parts: Sys.setenv(DID_EXT_PARTS = "measurement,blame,treatments,inference,selection"); DID_EXT_REFIT=1 refits.

rm(list = ls())
here::i_am("code/analyze_did/17_extensions.R")
Sys.setenv(DID_NO_FIGURES = "1")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(did, readstata13)

refit <- Sys.getenv("DID_EXT_REFIT") == "1"
parts <- if (Sys.getenv("DID_EXT_PARTS") != "") strsplit(Sys.getenv("DID_EXT_PARTS"), ",")[[1]] else
  c("measurement", "blame", "treatments", "inference", "selection")
tr_lab <- c(pinc25 = "Large income loss", jobloss = "Job loss")
raw <- path.expand(Sys.getenv("LISS_RAW"))
in_range <- function(x, lo, hi) { x <- suppressWarnings(as.numeric(x)); fifelse(x >= lo & x <= hi, x, NA_real_) }

# cached fits, as in 15_mechanisms.R (tagged with the panels and definitions; parallel batches)
cached_fits <- function(name, keys, fun) {
  path <- path_est("extensions", name)
  cache <- if (file.exists(path) && !refit) readRDS(path) else list()
  if (!identical(cache$tag, cache_tag())) cache <- list(tag = cache_tag())
  todo <- keys[vapply(keys, function(k) is.null(cache[[k]]), logical(1))]
  for (batch in split(todo, ceiling(seq_along(todo) / n_cores))) {
    message(name, ": ", paste(batch, collapse = ", "))
    cache[batch] <- parallel::mclapply(batch, function(k) { setDTthreads(1); try_est(fun(k), k) },
                                       mc.cores = n_cores, mc.preschedule = FALSE)
    saveRDS(cache, path)
  }
  cache[setdiff(names(cache), "tag")]
}
# Work and Schooling items (named vector: new name = item number), read from the archive zips in
# LISS_RAW/work_schooling/zip without unpacking them to disk; columns renamed by name, because
# read.dta13 returns them in file order
read_ws <- function(items) {
  td <- tempfile("ws"); dir.create(td); on.exit(unlink(td, recursive = TRUE))
  rbindlist(lapply(list.files(file.path(raw, "work_schooling", "zip"), pattern = "^cw[0-9]{2}[a-z].*[.]zip$", full.names = TRUE), function(z) {
    f <- unzip(z, exdir = td, junkpaths = TRUE); on.exit(unlink(f))
    pre <- sub("_EN.*", "", basename(z)); v <- paste0(pre, items)
    d <- as.data.table(suppressWarnings(read.dta13(f, select.cols = c("nomem_encr", v), convert.factors = FALSE)))
    setnames(d, c("nomem_encr", v), c("id", names(items)))
    d[, ws_year := 2000L + as.integer(substr(pre, 3, 4))][]
  }))
}
# windows: Early, Mid, Avg against wave -1; CleanMid, CleanAvg against the mean of waves -5 to -3
summarise_fit <- function(f, sd = NA_real_) rbindlist(lapply(c("Early", "Mid", "Avg", "CleanMid", "CleanAvg"), function(nm) {
  x <- lc(f, nm); data.table(window = nm, estimate = x$estimate, se = x$se)
}))[, `:=`(pre_p = pre_p(f), n_treated = f$n_treated, est_sd = estimate / sd, se_sd = se / sd)][]


# Data: party ratings, leaders, votes ---------------------------------------------

parties <- c("VVD", "D66", "PVV", "CDA", "SP", "PvdA", "GL", "PvdD", "CU", "SGP", "FvD", "TON", "Volt",
             "JA21", "DENK", "50PLUS", "BBB", "BIJ1", "VNL", "NSC", "GL_PvdA")
# confidence in six non-political institutions (0-10; the block of the note's confidence index, items
# 016 and 021-025): anchors for the person's general use of the scale
anchor_items <- paste0("conf_", c("police", "media", "military", "edusys", "healthcare", "science"))
pol <- fread(here("data", "liss_politics_to26.csv"),
             select = c("nomem_encr", "wave", "distance", "partisan_affect", paste0("thermo_", parties),
                        grep("^vote_today_", names(fread(here("data", "liss_politics_to26.csv"), nrows = 0)), value = TRUE),
                        "own_polinfluence_through_protest", anchor_items))
pol <- pol[!duplicated(pol[, .(nomem_encr, wave)])]
th <- melt(pol[, c("nomem_encr", "wave", paste0("thermo_", parties)), with = FALSE], id.vars = c("nomem_encr", "wave"),
           variable.name = "party", value.name = "rating", variable.factor = FALSE)
th[, `:=`(party = sub("thermo_", "", party), rating = in_range(rating, 0, 10))]
th <- th[!is.na(rating)]
vs <- fread(here("data", "raw", "vote_shares.csv"))[, .(
  party = fcase(party == "GrLinks", "GL", party %in% c("TROTS", "DPK"), "TON", party == "GL/PvdA", "GL_PvdA", default = party),
  election_year = as.integer(election_year), share)]
th[, election_year := election_before(wave)][vs, on = c("party", "election_year"), share := i.share]
th[cabinet, on = c("wave", "party"), gov := TRUE][is.na(gov), gov := FALSE]

# 1a. constant party set: VVD, CDA, D66, SP, PVV and the main left party (PvdA; GL-PvdA from 2023/24,
# when the two ran jointly), rated in every wave; equal weights, and fixed weights (each party's
# mean vote share over the seven elections)
const <- th[party %in% c("VVD", "CDA", "D66", "SP", "PVV") | (party == "PvdA" & wave <= 23) | (party == "GL_PvdA" & wave >= 24)]
const[party %in% c("PvdA", "GL_PvdA"), party := "Left"]
w_fixed <- vs[party %in% c("VVD", "CDA", "D66", "SP", "PVV", "PvdA", "GL_PvdA")][
  , party := fifelse(party %in% c("PvdA", "GL_PvdA"), "Left", party)][, .(w = mean(share)), by = party]
const[w_fixed, on = "party", w := i.w]
wd <- function(r, w) { w <- w / sum(w); sqrt(sum(w * (r - max(r))^2)) }
ws <- function(r, w) { w <- w / sum(w); sqrt(sum(w * (r - sum(w * r))^2)) }
m_const <- const[, if (.N >= 2) .(dist_const_eq = wd(rating, rep(1, .N)), spread_const_eq = ws(rating, rep(1, .N)),
                                  dist_const_fix = wd(rating, w)), by = .(nomem_encr, wave)]

# 1b. scale use (all rated parties)
m_scale <- th[, if (.N >= 2) .(mean_rating = mean(rating), max_rating = max(rating), min_rating = min(rating), rank_gap = max(rating) - median(rating), n_low = sum(rating <= 2),
                               n_rated = .N, sd_rating = sd(rating)), by = .(nomem_encr, wave)]
m_scale <- merge(m_scale, pol[, .(nomem_encr, wave, distance)], by = c("nomem_encr", "wave"))
m_scale[, dist_rel := fifelse(sd_rating > 0, distance / sd_rating, NA_real_)][, sd_rating := NULL]
# scale use outside politics: spread and mean of the six institution ratings (at least four answered).
# If the loss made people use the ends of the scale more, the spread of these ratings would rise too.
an <- melt(pol[, c("nomem_encr", "wave", anchor_items), with = FALSE], id.vars = c("nomem_encr", "wave"), value.name = "r")
an <- an[, r := in_range(r, 0, 10)][!is.na(r)]
m_scale <- merge(m_scale, an[, if (.N >= 4) .(anchor_sd = sd(r), anchor_mean = mean(r), anchor_max = max(r), anchor_min = min(r)), by = .(nomem_encr, wave)],
                 by = c("nomem_encr", "wave"), all = TRUE)
# each institution separately (which ones move)
m_inst <- dcast(an[, variable := sub("conf_", "inst_", variable)], nomem_encr + wave ~ variable, value.var = "r")
# distance net of the general spread: both in standard deviations (all person-waves), difference
m_scale[, dist_net_anchor := distance / sd(distance, na.rm = TRUE) - anchor_sd / sd(anchor_sd, na.rm = TRUE)][, distance := NULL]

# 1c. party leaders (raw politics files; "What do you think of <name>?"), mapped to their parties
leader_party <- c(`Jan Peter Balkenende` = "CDA", `Wouter Bos` = "PvdA", `Mark Rutte` = "VVD", `Jan Marijnissen` = "SP",
  `Agnes Kant` = "SP", `Femke Halsema` = "GL", `Alexander Pechtold` = "D66", `André Rouvoet` = "CU", `Bas van der Vlies` = "SGP",
  `Rita Verdonk` = "TON", `Geert Wilders` = "PVV", `Marianne Thieme` = "PvdD", `Job Cohen` = "PvdA", `Maxime Verhagen` = "CDA",
  `Emile Roemer` = "SP", `Kees van der Staaij` = "SGP", `Jolande Sap` = "GL", `Arie Slob` = "CU", `Diederik Samsom` = "PvdA",
  `Lilian Marijnissen` = "SP", `[Emile Roemer / Lilian Marijnissen]` = "SP", `Sybrand van Haersma Buma` = "CDA",
  `Bram van Ojik` = "GL", `Henk Krol` = "50PLUS", `Jan Nagel` = "50PLUS", `Jesse Klaver` = "GL", `Lodewijk Asscher` = "PvdA",
  `[Diederik Samsom / Lodewijk Asscher]` = "PvdA", `Gert-Jan Segers` = "CU", `Tunahan Kuzu` = "DENK", `Jan Roos` = "VNL",
  `Thierry Baudet` = "FvD", `Farid Azarkan` = "DENK", `Rob Jetten` = "D66", `Pieter Heerma` = "CDA", `Esther Ouwehand` = "PvdD",
  `Wopke Hoekstra` = "CDA", `Sigrid Kaag` = "D66", `Liane den Haan` = "50PLUS", `Lilianne Ploumen` = "PvdA",
  `Martin van Rooijen` = "50PLUS", `Laurens Dassen` = "Volt", `Joost Eerdmans` = "JA21", `Caroline van der Plas` = "BBB",
  `Sylvana Simons` = "BIJ1", `Attje Kuiken` = "PvdA", `Mirjam Bikker` = "CU", `Frans Timmermans` = "GL_PvdA",
  `Pieter Omtzigt` = "NSC", `Dilan Yeşilgöz` = "VVD", `Henri Bontenbal` = "CDA", `Chris Stoffer` = "SGP",
  `Stephan van Baarle` = "DENK", `Jimmy Dijk` = "SP", `Lidewij de Vos` = "FvD", `Jan Struijs` = "50PLUS", `Dick Schoof` = NA)
pm_name <- function(w) fcase(w <= 10, "Jan Peter Balkenende", w <= 24, "Mark Rutte", default = "Dick Schoof")
read_leaders <- function() {
  fs <- list.files(file.path(raw, "politics"), pattern = "^cv[0-9]{2}.*[.]dta$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    l <- varlabel(suppressWarnings(read.dta13(f, select.rows = 2)))
    l <- l[grepl("think of", l) & !grepl("party|parti|DENK[?]|Volt[?]|50Plus|JA21|NSC|BBB|Forum|SGP|ChristenUnie|GroenLinks|Trots", l)]
    nm <- sub("[?]$", "", sub(".*think of (our (caretaker )?Prime Minister )?", "", l))
    keep <- nm %in% names(leader_party); if (!any(keep)) return(NULL)
    d <- as.data.table(suppressWarnings(read.dta13(f, select.cols = c("nomem_encr", names(l)[keep]), convert.factors = FALSE)))
    d <- melt(d, id.vars = "nomem_encr", variable.name = "var", value.name = "rating", variable.factor = FALSE)
    d[, `:=`(leader = nm[keep][match(var, names(l)[keep])], wave = as.integer(substr(basename(f), 3, 4)), rating = in_range(rating, 0, 10))]
    d[!is.na(rating), .(nomem_encr, wave, leader, rating)]
  }))
}
ld <- read_leaders()
ld[, party := unname(leader_party[leader])][, election_year := election_before(wave)][vs, on = c("party", "election_year"), share := i.share]
ld[cabinet, on = c("wave", "party"), gov := TRUE][is.na(gov), gov := FALSE]
m_lead <- merge(
  ld[!is.na(share), if (.N >= 2) .(lead_dist = wd(rating, share), lead_spread = ws(rating, share)), by = .(nomem_encr, wave)],
  ld[, .(pm_rating = if (any(leader == pm_name(wave[1]))) rating[leader == pm_name(wave[1])][1] else NA_real_,
         opp_leaders = if (any(!gov & leader != pm_name(wave[1]) & !is.na(party))) mean(rating[!gov & leader != pm_name(wave[1]) & !is.na(party)]) else NA_real_),
     by = .(nomem_encr, wave)], by = c("nomem_encr", "wave"), all = TRUE)
m_lead[!wave %in% cabinet$wave, opp_leaders := NA_real_][, pm_minus_opp := pm_rating - opp_leaders]

# 2a/2c. vote intention ("If parliamentary elections were held today": party dummies vote_today_* in the
# build, all waves; none named = would not vote, does not know or did not answer) and protest
vote_cols <- intersect(paste0("vote_today_", c(parties, "other")), names(pol))
vt <- melt(pol[, c("nomem_encr", "wave", vote_cols), with = FALSE], id.vars = c("nomem_encr", "wave"), variable.name = "party",
           value.name = "v", variable.factor = FALSE)[, party := sub("vote_today_", "", party)][v %in% 1]
vt[cabinet, on = c("wave", "party"), gov := TRUE][is.na(gov), gov := FALSE]
vt <- vt[, .SD[1], by = .(nomem_encr, wave)]                        # one party per person-wave
m_vote <- merge(pol[, .(nomem_encr, wave, protest = in_range(own_polinfluence_through_protest, 0, 1))],
                vt[, .(nomem_encr, wave, vote_party = party, vote_gov = gov)], by = c("nomem_encr", "wave"), all.x = TRUE)
m_vote[, `:=`(intend_gov = fifelse(wave %in% cabinet$wave, as.numeric(vote_gov %in% TRUE), NA_real_),
              intend_PVV = as.numeric(vote_party %in% "PVV"), intend_SP = as.numeric(vote_party %in% "SP"),
              intend_none = as.numeric(is.na(vote_party)))]

# 2d. emotions and self-esteem (personality module, 1-7 / 1-7) and loneliness (social integration,
# De Jong Gierveld 6 items: 1 yes, 2 more or less, 3 no). Fielded between politics interviews.
read_items <- function(dir, prefix, items) {
  fs <- list.files(file.path(raw, dir), pattern = paste0("^", prefix, "[0-9]{2}.*[.]dta$"), full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    nm <- names(varlabel(suppressWarnings(read.dta13(f, select.rows = 2))))
    v <- nm[sub("^[a-z]{2}[0-9]{2}[a-z]", "", nm) %in% items]
    d <- as.data.table(suppressWarnings(read.dta13(f, select.cols = c("nomem_encr", v), convert.factors = FALSE)))
    setnames(d, v, paste0("i", sub("^[a-z]{2}[0-9]{2}[a-z]", "", v)))
    d[, wave := as.integer(substr(basename(f), 3, 4))][]
  }), fill = TRUE)
}
pers <- read_items("personality", "cp", sprintf("%03d", c(70:79, 149, 153, 156, 160, 165)))
for (v in grep("^i", names(pers), value = TRUE)) pers[[v]] <- in_range(pers[[v]], 1, 7)
pers[, `:=`(anger = rowMeans(cbind(i153, i156)), anxiety = rowMeans(cbind(i160, i165)),
            self_esteem = rowMeans(cbind(i070, i071, 8 - i072, i073, 8 - i074, i075, i076, 8 - i077, 8 - i078, 8 - i079)))]
soc <- read_items("social_integration_leisure", "cs", sprintf("%03d", 284:289))
for (v in grep("^i", names(soc), value = TRUE)) soc[[v]] <- in_range(soc[[v]], 1, 3)
soc[, loneliness := (i284 <= 2) + (i288 <= 2) + (i289 <= 2) + (i285 >= 2) + (i286 >= 2) + (i287 >= 2)]   # 0-6
m_emo <- merge(pers[, .(nomem_encr, wave, anger, anxiety, self_esteem)], soc[, .(nomem_encr, wave, loneliness)],
               by = c("nomem_encr", "wave"), all = TRUE)

ext <- Reduce(function(a, b) merge(a, b, by = c("nomem_encr", "wave"), all = TRUE),
              list(m_const, m_scale, m_inst, m_lead, m_vote[, !c("vote_party", "vote_gov")], m_emo))
setnames(ext, "nomem_encr", "id")

panels <- load_panels()
add_ext <- function(d) setorder(merge(d[, !intersect(setdiff(names(ext), c("id", "wave")), names(d)), with = FALSE], ext,
                                      by = c("id", "wave"), all.x = TRUE), id, t)[]
px <- lapply(panels[c(names(tr_lab), paste0(names(tr_lab), "_all"))], add_ext)
sd_of <- function(y) sd(prep_outcome(px$jobloss, y)[G == 0][[y]])
between_ext <- c("anger", "anxiety", "self_esteem", "loneliness")   # fielded between politics interviews


# Stacked within-person estimator ------------------------------------------------------
# rows: for every cohort g and wave t (event time e = t - g), people who suffer the loss in g and
# people who never do, observed in g - 1 and t, employed in g - 1 (and, before the loss, in t);
# after the loss all waves (d: an unrestricted *_all panel). base_cols are carried from wave g - 1.
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
# DiD cells (cell_cols + g + e), averaged within (group_cols, e) by the (weighted) number of treated;
# influence function summed by person, so standard errors are clustered by person. Returns one fit
# (b, V, n_treated) per group.
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


# 1. Measurement ------------------------------------------------------------------------

meas_spec <- c(dist_const_eq = "Distance, constant party set, equal weights", spread_const_eq = "Spread, constant party set, equal weights",
               dist_const_fix = "Distance, constant party set, fixed vote-share weights",
               lead_dist = "Distance between party leaders", lead_spread = "Spread of leader ratings",
               pm_rating = "Rating of the prime minister", opp_leaders = "Mean rating of opposition leaders",
               pm_minus_opp = "Prime minister minus opposition leaders",
               dist_rel = "Distance relative to the person's spread of ratings", rank_gap = "Highest minus median party rating",
               mean_rating = "Mean party rating", n_low = "Number of parties rated 2 or lower", n_rated = "Number of parties rated",
               partisan_affect = "Partisan affect (API, partisans only)", distance = "Distance (main)",
               anchor_sd = "Spread of confidence in non-political institutions", anchor_mean = "Mean confidence in non-political institutions",
               max_rating = "Rating of the highest-rated party", dist_net_anchor = "Distance minus spread of institution confidence (both in SD)",
               min_rating = "Rating of the lowest-rated party", anchor_max = "Highest confidence among the six institutions",
               anchor_min = "Lowest confidence among the six institutions",
               inst_police = "Confidence in the police", inst_media = "Confidence in the media", inst_military = "Confidence in the military",
               inst_edusys = "Confidence in the education system", inst_healthcare = "Confidence in health care", inst_science = "Confidence in science")
if ("measurement" %in% parts) {
  keys <- CJ(tr = names(tr_lab), y = names(meas_spec))[, paste(tr, y, sep = "|")]
  fits <- cached_fits("measurement", keys, function(k) { s <- strsplit(k, "|", fixed = TRUE)[[1]]; cs_fit(px[[s[1]]], s[2]) })
  meas <- rbindlist(lapply(names(fits), function(k) { s <- strsplit(k, "|", fixed = TRUE)[[1]]
    if (is.null(fits[[k]])) return(NULL); summarise_fit(fits[[k]], sd_of(s[2]))[, `:=`(treatment = s[1], outcome = s[2])] }))
  meas[, label := meas_spec[outcome]]
  saveRDS(list(fits = fits, summary = meas), path_est("extensions", "measurement_summary"))
  message("measurement (waves 2-4):"); print(meas[window == "Mid", .(treatment, label, est = round(estimate, 3), se = round(se, 3), sd = round(est_sd, 2), pre = round(pre_p, 2))][order(label, treatment)])
}


# 2. Incumbent blame --------------------------------------------------------------------

if ("blame" %in% parts) {
  # 2a. by intended vote in wave -1: governing party / other party / no party (would not vote,
  # does not know); outcomes distance, mean rating of governing parties (cabinet at the time), and
  # the rating of the party the person intended to vote for in wave -1 (own party)
  vote_b <- m_vote[, .(id = nomem_encr, wave, own_party = vote_party,
                      vote_group = fcase(vote_gov %in% TRUE, "governing", !is.na(vote_party), "other party", default = "none named"))]
  vote_b[!wave %in% cabinet$wave, vote_group := NA_character_]
  gov_mean <- th[wave %in% cabinet$wave, .(gov_mean = if (any(gov)) mean(rating[gov]) else NA_real_), by = .(id = nomem_encr, wave)]
  blame_by_vote <- function(tr) {
    d <- merge(px[[paste0(tr, "_all")]], vote_b, by = c("id", "wave"), all.x = TRUE)
    d <- merge(d, gov_mean, by = c("id", "wave"), all.x = TRUE)
    out <- list()
    for (y in c("distance", "gov_mean")) {
      r <- stack_rows(d, y, base_cols = "vote_group")[!is.na(vote_group)]
      for (f in cell_did(r, "vote_group")) out[[paste(y, f$group$vote_group)]] <- c(f, list(outcome = y, vote_group = f$group$vote_group))
    }
    # own party: change in the rating of the party intended in wave -1, from wave -1 to wave t
    r <- stack_rows(d, "distance", base_cols = c("vote_group", "own_party"))[vote_group %in% c("governing", "other party")]
    rt <- th[, .(id = nomem_encr, wave = wave, own_party = party, rating)]
    r[rt, on = c("id", wave_t = "wave", "own_party"), r_t := i.rating][rt, on = c("id", wave_b = "wave", "own_party"), r_b := i.rating]
    r <- r[!is.na(r_t) & !is.na(r_b)][, dy := r_t - r_b]
    for (f in cell_did(r, "vote_group")) out[[paste("own_party", f$group$vote_group)]] <- c(f, list(outcome = "own_party", vote_group = f$group$vote_group))
    out
  }
  # 2b. party-level ratings by whether the party governs in wave t: cells = party x cohort x wave;
  # the contrast between governing and not governing compares the same parties over time
  # parties rated in every wave; switchers: governing in some cabinet waves and not in others
  ps <- c("CDA", "PvdA", "VVD", "D66", "CU", "PVV", "SP", "GL")
  switchers <- ps[vapply(ps, function(p) uniqueN(cabinet$wave[cabinet$party == p]) %in% seq_len(uniqueN(cabinet$wave) - 1L), logical(1))]
  blame_by_status <- function(tr) {
    d <- px[[paste0(tr, "_all")]]
    r <- rbindlist(lapply(ps, function(p) {
      dp <- merge(d, th[party == p, .(id = nomem_encr, wave, r_p = rating)], by = c("id", "wave"), all.x = TRUE)
      stack_rows(dp, "r_p")[, party := p]
    }))
    r[cabinet[, .(wave, party, g_t = TRUE)], on = c(wave_t = "wave", "party"), status := "governing"][is.na(status), status := "not governing"]
    r <- r[wave_t %in% cabinet$wave]
    list(all = cell_did(r, "status", "party"), switchers = cell_did(r[party %in% switchers], "status", "party"))
  }
  keys <- c(paste0(names(tr_lab), "|by_vote"), paste0(names(tr_lab), "|by_status"))
  fits_b <- cached_fits("blame_stacked", keys, function(k) { s <- strsplit(k, "|", fixed = TRUE)[[1]]
    if (s[2] == "by_vote") blame_by_vote(s[1]) else blame_by_status(s[1]) })
  # 2c/2d. CS on votes, participation and emotions (emotions: reference wave -2, shift_between)
  ys <- c("intend_gov", "intend_PVV", "intend_SP", "intend_none", "protest", between_ext)
  keys2 <- CJ(tr = names(tr_lab), y = ys)[, paste(tr, y, sep = "|")]
  fits_c <- cached_fits("blame_cs", keys2, function(k) { s <- strsplit(k, "|", fixed = TRUE)[[1]]
    cs_fit(if (s[2] %in% between_ext) shift_between(px[[s[1]]]) else px[[s[1]]], s[2]) })
  bl <- list()
  for (k in grep("by_vote", names(fits_b), value = TRUE)) for (f in fits_b[[k]])
    bl[[length(bl) + 1]] <- summarise_fit(f)[, `:=`(treatment = sub("[|].*", "", k), analysis = "by vote in wave -1", outcome = f$outcome, group = f$vote_group)]
  for (k in grep("by_status", names(fits_b), value = TRUE)) for (set in c("all", "switchers")) for (f in fits_b[[k]][[set]])
    bl[[length(bl) + 1]] <- summarise_fit(f)[, `:=`(treatment = sub("[|].*", "", k), analysis = paste("party rating by status,", set), outcome = "party rating", group = f$group$status)]
  for (k in names(fits_c)) { s <- strsplit(k, "|", fixed = TRUE)[[1]]; if (is.null(fits_c[[k]])) next
    bl[[length(bl) + 1]] <- summarise_fit(fits_c[[k]], sd_of(s[2]))[, `:=`(treatment = s[1], analysis = "CS", outcome = s[2], group = "all")] }
  blame <- rbindlist(bl, fill = TRUE)
  # contrasts between groups (independent samples for vote groups; status contrast from the same people:
  # conservative SE without covariance)
  saveRDS(list(fits_stacked = fits_b, fits_cs = fits_c, summary = blame, switchers = switchers), path_est("extensions", "blame_summary"))
  message("blame (waves 2-4):"); print(blame[window == "Mid", .(treatment, analysis, outcome, group, est = round(estimate, 3), se = round(se, 3), pre = round(pre_p, 2), n_treated)])
}


# 3. Treatments -------------------------------------------------------------------------

# partners from the raw background files: in the month of each respondent's background record
# (date_bg), the household head's partner (position 2 or 3) or the head of a partner's household
read_partners <- function() {
  months <- unique(fread(here("data", "liss_to26.csv"), select = c("nomem_encr", "wave", "date_bg"))[!is.na(date_bg)])
  fs <- list.files(file.path(raw, "background"), pattern = "^avars_[0-9]{6}.*[.]dta$", full.names = TRUE)
  fs <- fs[as.integer(substr(basename(fs), 7, 12)) %in% unique(months$date_bg)]
  bg <- rbindlist(lapply(fs, function(f) {
    x <- as.data.table(suppressWarnings(read.dta13(f, select.cols = c("nomem_encr", "nohouse_encr", "positie", "belbezig", "nettoink_f"), convert.factors = FALSE)))
    x[, date_bg := as.integer(substr(basename(f), 7, 12))][]
  }), fill = TRUE)   # nettoink_f is missing before Jul 2008, as in the build (05_LISS_background)
  bg[, nettoink_f := as.numeric(nettoink_f)]
  heads <- bg[positie == 1, .(nohouse_encr, date_bg, head = nomem_encr, head_occ = belbezig, head_inc = nettoink_f)]
  parts <- bg[positie %in% 2:3, .(nohouse_encr, date_bg, partner = nomem_encr, p_occ = belbezig, p_inc = nettoink_f)]
  hp <- merge(heads, parts, by = c("nohouse_encr", "date_bg"))
  hp <- hp[, if (.N == 1) .SD, by = .(nohouse_encr, date_bg)]   # one partner
  pr <- rbind(hp[, .(nomem_encr = head, date_bg, partner_id = partner, partner_occ = p_occ, partner_inc = p_inc)],
              hp[, .(nomem_encr = partner, date_bg, partner_id = head, partner_occ = head_occ, partner_inc = head_inc)])
  merge(months, pr, by = c("nomem_encr", "date_bg"))[, .(id = nomem_encr, wave, partner_id, partner_occ, partner_inc)]
}
# partner treatments: the first wave in which the partner (the same person as in the wave before,
# employed then) loses the job / has income at least 25 percent lower; respondents aged 18-65 with a
# partner; never treated: partner never has the event; people who lose their own job in the same
# wave are dropped (own loss is the note's treatment)
partner_panel <- function(pp, event) {
  liss <- fread(here("data", "liss_to26.csv"), select = c("nomem_encr", "wave", "age", "male", "occupation", "distance"))
  liss <- liss[age >= 18 & age <= 65][, `:=`(id = nomem_encr, t = as.integer(wave - 7L))][, nomem_encr := NULL]
  d <- merge(liss, pp, by = c("id", "wave"))
  setorder(d, id, t)
  d[, `:=`(p_emp = partner_occ %in% emp_codes, p_unemp = partner_occ %in% unemp_codes, own_unemp = occupation %in% unemp_codes)]
  d[, same := t - shift(t) == 1L & partner_id == shift(partner_id), by = id]
  d[, `:=`(p_emp_l = shift(p_emp), p_inc_l = shift(partner_inc)), by = id]
  d[, ev := same %in% TRUE & p_emp_l %in% TRUE & (if (event == "job") p_unemp else (p_inc_l > 0 & partner_inc <= 0.75 * p_inc_l) %in% TRUE)]
  d[, G := if (any(ev)) t[which(ev)[1]] else 0L, by = id]
  d <- d[!id %in% d[G > 0 & t == G & own_unemp, id]]
  d <- d[(G > 0 & t >= G) | p_emp]                 # before the event (and for controls): partner employed
  d[, D := as.integer(G > 0 & t >= G)][]
}
if ("treatments" %in% parts) {
  # 3a. job loss with vs without unemployment benefit (WW) in the calendar year of the loss or the next
  # (income module of waves g and g + 1); WW requires dismissal through no fault of one's own
  inc <- fread(here("data", "liss_income_to26.csv"), select = c("nomem_encr", "wave", "b_unempl"))[, .(id = nomem_encr, wave, ww = b_unempl %in% 1)]
  jl <- copy(px$jobloss)
  ww_ev <- unique(jl[G > 0, .(id, gw = G + 7L)])
  ww_ev <- merge(ww_ev, inc, by = "id", allow.cartesian = TRUE)[wave - gw >= 0L & wave - gw <= 1L, .(ww = any(ww)), by = id]   # row-wise: waves g and g + 1
  jl[ww_ev, on = "id", ww := i.ww]
  # 3b. reason for the job loss: "For what reason did you stop working?" (Work and Schooling, cw*326),
  # asked of people not in paid work in April/May; taken from the first module after the loss (fielded
  # in calendar year 1999 + g or 2000 + g, between the politics interviews of waves g - 1 and g + 1),
  # if the year the person stopped working (cw*319) is missing or within 1998 + g to 2000 + g.
  # Closure: dismissed due to company closure or reorganization (code 9); other dismissal: end of a
  # (temporary) contract or dismissed for other reasons (11, 12); other: any other reason or no answer
  # (mostly back in work by the time of the module). Read from the zips in LISS_RAW/work_schooling/zip.
  ws <- read_ws(c(reason = "326", year_stop = "319"))[!is.na(reason)]
  rs <- merge(unique(jl[G > 0, .(id, g = G + 7L)]), ws, by = "id")[ws_year - g >= 1999L & ws_year - g <= 2000L]
  rs <- rs[is.na(year_stop) | (year_stop - g >= 1998L & year_stop - g <= 2000L)][order(id, ws_year)][, .SD[1], by = id]
  jl[rs, on = "id", reason := i.reason]
  jl[G > 0, reason_grp := fcase(reason %in% 9, "closure", reason %in% c(11, 12), "dismissal", default = "other")]
  # 3c. size of the first income drop of 10 percent or more (after an employed wave): 10-25, 25-50, 50+;
  # never treated: no drop of 10 percent or more; sample rules as in 01_sample.R
  liss <- fread(here("data", "liss_to26.csv"), select = c("nomem_encr", "wave", "age", "occupation", "net_monthly_income", "distance"))
  liss <- liss[age >= 18 & age <= 65][, `:=`(id = nomem_encr, t = as.integer(wave - 7L), emp = occupation %in% emp_codes)][, nomem_encr := NULL]
  setorder(liss, id, t)
  liss[, inc_l := fifelse(t - shift(t) == 1L, shift(net_monthly_income), NA_real_), by = id]
  liss[, emp_l := fifelse(t - shift(t) == 1L, shift(emp), NA), by = id]
  liss[, drop := fifelse(inc_l > 0, 1 - net_monthly_income / inc_l, NA_real_)]
  liss[, G := if (any((drop >= 0.10) %in% TRUE)) t[which((drop >= 0.10) %in% TRUE)[1]] else 0L, by = id]
  liss <- liss[!id %in% liss[G > 0 & t == G & !(emp_l %in% TRUE), id]]
  liss[, dose := if (G[1] > 0) drop[t == G[1]] else NA_real_, by = id]
  dose_panel <- function(lo, hi) {
    d <- liss[G == 0 | (dose >= lo & dose < hi)]
    d <- d[(G > 0 & t >= G) | emp]
    d[, D := as.integer(G > 0 & t >= G)][]
  }
  pp <- read_partners()
  tp <- list(ww_yes = function() jl[G == 0 | ww %in% TRUE], ww_no = function() jl[G == 0 | ww %in% FALSE],
             reason_closure = function() jl[G == 0 | reason_grp %in% "closure"],
             reason_dismissal = function() jl[G == 0 | reason_grp %in% "dismissal"],
             reason_other = function() jl[G == 0 | reason_grp %in% "other"],
             partner_job = function() partner_panel(pp, "job"), partner_inc = function() partner_panel(pp, "inc"),
             dose_10_25 = function() dose_panel(0.10, 0.25), dose_25_50 = function() dose_panel(0.25, 0.50), dose_50 = function() dose_panel(0.50, Inf))
  fits_t <- cached_fits("treatments", names(tp), function(k) cs_fit(tp[[k]](), "distance"))
  trt <- rbindlist(lapply(names(fits_t), function(k) if (!is.null(fits_t[[k]])) summarise_fit(fits_t[[k]], sd_of("distance"))[, treatment := k]))
  saveRDS(list(fits = fits_t, summary = trt, ww_share = jl[G > 0, mean(ww %in% TRUE)], reasons = unique(jl[G > 0, .(id, reason, reason_grp)])[, .N, by = .(reason_grp, reason)]), path_est("extensions", "treatments_summary"))
  message("treatments (distance, waves 2-4):"); print(trt[window == "Mid", .(treatment, est = round(estimate, 3), se = round(se, 3), pre = round(pre_p, 2), n_treated)])
}


# 4. Inference --------------------------------------------------------------------------

if ("inference" %in% parts) {
  sd_d <- sd_of("distance")
  main <- cached_fits("inference", paste0(names(tr_lab), "|main"), function(k) cs_fit(px[[sub("[|].*", "", k)]], "distance"))
  # 4a. equivalence: the smallest bound delta such that every pre-treatment estimate (waves -5 to -2)
  # lies within [-delta, delta] at the 5 percent level (intersection-union test)
  eqv <- rbindlist(lapply(names(tr_lab), function(tr) {
    f <- main[[paste0(tr, "|main")]]
    k <- intersect(as.character(-5:-2), names(f$b))
    data.table(treatment = tr, delta = max(abs(f$b[k]) + qnorm(0.95) * sqrt(diag(f$V)[k])))[, delta_sd := delta / sd_d][]
  }))
  # 4b. attrition: the within-person estimator weighted by the inverse probability of being observed
  # in wave t, given treatment, baseline distance and covariates in wave -1 (logit by cohort)
  ipw_fit <- function(tr) {
    dall <- px[[paste0(tr, "_all")]]
    d <- prep_outcome(dall, "distance")
    r <- stack_rows(dall, "distance")
    es <- setdiff(event_window[1]:event_window[2], -1L)
    # everyone at risk in a cohort's stack: observed and employed in g - 1; observed in t or not
    base <- rbindlist(lapply(sort(unique(r$g)), function(g) {
      b <- d[t == g - 1L & emp %in% TRUE & G %in% c(0L, g),
             .(id, g = g, treated = G == g, y_b = distance, age, male = fcoalesce(as.numeric(male), 0.5),
               educ = fcoalesce(as.character(education_cat), "missing"), linc = log1p(pmax(fcoalesce(net_monthly_income, 0), 0)))]
      b[rep(seq_len(.N), each = length(es))][, e := rep(es, times = nrow(b))]
    }))
    base <- base[paste(g, e) %in% unique(r[, paste(g, e)])]
    base <- r[, .(id, g, e, obs = TRUE)][base, on = c("id", "g", "e")][is.na(obs), obs := FALSE]
    base[, p := predict(glm(obs ~ treated + y_b + poly(age, 2) + male + factor(educ) + linc + factor(e), family = binomial(), data = .SD),
                        type = "response"), by = g]
    r[base[obs == TRUE], on = c("id", "g", "e"), ipw := 1 / i.p]
    r[is.na(ipw), ipw := 1]
    list(unweighted = cell_did(r)[[1]], weighted = cell_did(r, w = "ipw")[[1]])
  }
  fits_i <- cached_fits("inference", paste0(names(tr_lab), "|ipw"), function(k) ipw_fit(sub("[|].*", "", k)))[paste0(names(tr_lab), "|ipw")]
  att <- rbindlist(lapply(names(fits_i), function(k) rbindlist(lapply(names(fits_i[[k]]), function(v)
    summarise_fit(fits_i[[k]][[v]], sd_d)[, `:=`(treatment = sub("[|].*", "", k), version = v)]))))
  saveRDS(list(equivalence = eqv, attrition = att), path_est("extensions", "inference_summary"))
  message("equivalence:"); print(eqv); message("attrition (waves 2-4):"); print(att[window == "Mid"])
}

# 5. Selection on sector and occupation ----------------------------------------------------------
# Losses concentrate in some sectors and occupations. The within-person estimator compares people
# with and without a loss only within the same group of sector (six groups) and occupation (three
# groups) before the loss (cells by cohort, wave and group). Sector and occupation come from the first
# Work and Schooling module in which the person reports them (cw*402, cw*404; current or last job),
# which for people with a loss must be fielded before the politics interview of wave g - 1 (calendar
# year up to 1998 + g). Compared with the same estimator on the same people without matching.

if ("selection" %in% parts) {
  sec <- read_ws(c(sector = "402", prof = "404"))[sector %in% 1:15 & prof %in% 1:9]
  sec[, `:=`(sector_grp = fcase(sector %in% 1:5, "industry and construction", sector %in% 6:8, "trade, catering, transport",
                                sector %in% 9:10, "finance and business services", sector == 11, "public administration",
                                sector %in% 12:13, "education and health", default = "other services"),
             prof_grp = fcase(prof %in% 1:4, "higher and intermediate", prof == 5, "other non-manual", default = "manual"))]
  sel_fit <- function(tr) {
    d <- px[[paste0(tr, "_all")]]
    gs <- unique(d[, .(id, G)])
    b <- merge(sec, gs, by = "id")[G == 0L | ws_year - (G + 7L) <= 1998L][order(id, ws_year)][, .SD[1], by = id]
    d <- merge(d, b[, .(id, cell = paste(sector_grp, prof_grp, sep = " / "))], by = "id")
    r <- stack_rows(d, "distance", base_cols = "cell")
    list(unmatched = cell_did(r)[[1]], matched = cell_did(r, cell_cols = "cell")[[1]],
         coverage = data.table(treated_all = uniqueN(px[[paste0(tr, "_all")]][G > 0, id]), treated_with_cell = uniqueN(d[G > 0, id])),
         cells = b[, .N, by = .(treated = G > 0, sector_grp, prof_grp)])
  }
  fits_s <- cached_fits("selection", paste0(names(tr_lab), "|sector"), function(k) sel_fit(sub("[|].*", "", k)))[paste0(names(tr_lab), "|sector")]
  sd_d <- sd_of("distance")
  sel <- rbindlist(lapply(names(fits_s), function(k) rbindlist(lapply(c("unmatched", "matched"), function(v)
    summarise_fit(fits_s[[k]][[v]], sd_d)[, `:=`(treatment = sub("[|].*", "", k), version = v)]))))
  saveRDS(list(summary = sel, coverage = rbindlist(lapply(names(fits_s), function(k) fits_s[[k]]$coverage[, treatment := sub("[|].*", "", k)])),
               cells = rbindlist(lapply(names(fits_s), function(k) fits_s[[k]]$cells[, treatment := sub("[|].*", "", k)]))),
          path_est("extensions", "selection_summary"))
  message("selection (sector x occupation):"); print(sel[window %in% c("Mid", "Avg")])
}

### END
