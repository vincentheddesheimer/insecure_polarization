### Validation of the party-rating measure with LISS study 233, "Affective polarization"
# (Harteveld, University of Amsterdam; wave 1 August 2019, sl19a; wave 2 July 2021, sl21b).
# The study asks how people feel about each party's voters (0-100), about ideological, issue and
# social groups, whether they would mind a child marrying a voter of their least-liked party (1-7),
# whether they have come to hate people for their opinions (1-5) and, in 2021, how tolerant they are
# of the least-liked party's followers. Respondents carry nomem_encr, so they link to the panel.
#
# 1. Cross-section: correlations of the note's distance (politics waves around each study wave) with
#    distance computed from feelings toward voters (same formula, same parties, same weights), the
#    rating of the least-liked party's voters, social distance, hatred and intolerance; benchmark:
#    correlation of distance with itself in the adjacent politics wave.
# 2. Changes: for people in both study waves, the change in voter-based distance (2019 to 2021)
#    against the change in party-based distance over the same period (eight parties asked in both
#    years, fixed 2017 weights). The note's design uses within-person changes, so this is the check
#    that matters most.
# 3. General dispersion: correlation of distance with the spread of a person's ratings of social
#    groups (2019), and the correlation of party- and voter-based distance net of that spread.
# Losses between the two study waves are too few (12 large income losses, 3 job losses among people
# in both waves) to estimate effects on these measures; the script reports the counts.
#
# Needs data/liss_to26.csv, data/liss_politics_to26.csv, data/raw/vote_shares.csv, the panels (01)
# and the study files in LISS_RAW/affective_polarization. Saves results/estimates/ap_study/
# validation.rds (aggregates only).

rm(list = ls())
here::i_am("code/analyze_did/18_ap_validation.R")
Sys.setenv(DID_NO_FIGURES = "1")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(readstata13)

ap_dir <- file.path(path.expand(Sys.getenv("LISS_RAW")), "affective_polarization")
in_range <- function(x, lo, hi) { x <- suppressWarnings(as.numeric(x)); fifelse(x >= lo & x <= hi, x, NA_real_) }
wd <- function(r, w) { w <- w / sum(w); sqrt(sum(w * (r - max(r))^2)) }          # Wagner distance
ws <- function(r, w) { w <- w / sum(w); sqrt(sum(w * (r - sum(w * r))^2)) }      # Wagner spread


# Study data ------------------------------------------------------------------------------

# feelings toward voters, by party (item numbers); 2021 adds six parties
voter_items <- list(
  `2019` = c(VVD = "032", PVV = "033", CDA = "034", D66 = "035", GL = "036", SP = "037", PvdA = "038", FvD = "039"),
  `2021` = c(VVD = "032", PVV = "033", CDA = "034", D66 = "035", GL = "036", SP = "037", PvdA = "038", FvD = "039",
             PvdD = "081", CU = "082", Volt = "083", JA21 = "084", SGP = "085", DENK = "086"))
prefix <- c(`2019` = "sl19a", `2021` = "sl21b")
file_of <- c(`2019` = "sl19a_EN_2.0p.dta", `2021` = "sl21b_EN_1.0p.dta")
election <- c(`2019` = 2017L, `2021` = 2021L)          # last parliamentary election before fieldwork
# politics waves around each study wave (fielded December to March): 2018/19 and 2019/20 around
# August 2019; 2020/21 and 2021/22 around July 2021
pol_waves <- list(`2019` = c(before = 19L, after = 20L), `2021` = c(before = 21L, after = 22L))
# social groups (2019 only): education, origin, religion, region, urbanity
social_items <- sprintf("%03d", 49:59)
# issue and ideological groups, asked in both years: left/right, refugees, welfare, gender roles
issue_pairs <- list(c("041", "042"), c("043", "044"), c("045", "046"), c("047", "048"))

vs <- fread(here("data", "raw", "vote_shares.csv"))[, .(
  party = fcase(party == "GrLinks", "GL", party %in% c("TROTS", "DPK"), "TON", party == "GL/PvdA", "GL_PvdA", default = party),
  election_year = as.integer(election_year), share)]

read_study <- function(yr) {
  p <- prefix[[yr]]
  a <- as.data.table(suppressWarnings(read.dta13(file.path(ap_dir, file_of[[yr]]), convert.factors = FALSE)))
  it <- function(n) in_range(a[[paste0(p, n)]], 0, 100) / 10                         # 0-10 scale
  vi <- voter_items[[yr]]
  long <- rbindlist(lapply(names(vi), function(pt) data.table(id = a$nomem_encr, party = pt, rating = it(vi[[pt]]))))[!is.na(rating)]
  long[vs[election_year == election[[yr]]], on = "party", share := i.share]
  stopifnot(!anyNA(long$share))
  out <- long[, if (.N >= 2) .(v_dist = wd(rating, share), v_spread = ws(rating, share), v_min = min(rating), v_max = max(rating)), by = id]
  # same eight parties in both years with fixed 2017 weights (for changes)
  l8 <- long[party %in% names(voter_items[["2019"]])][vs[election_year == 2017L], on = "party", w17 := i.share]
  out <- merge(out, l8[, if (.N >= 2) .(v_dist8 = wd(rating, w17)), by = id], by = "id", all = TRUE)
  x <- data.table(id = a$nomem_encr,
    # social distance: 1 = would find it very difficult ... 7 = very happy; reversed, higher = more
    # aversion; in 2019 code -9 marks respondents shown the wrong party (missing)
    social_distance = 8 - in_range(a[[paste0(p, "063")]], 1, 7),
    hate = in_range(a[[paste0(p, "066")]], 1, 5),
    issue_ap = rowMeans(sapply(issue_pairs, function(pr) abs(it(pr[1]) - it(pr[2]))), na.rm = FALSE))
  if (yr == "2021")   # intolerance of the least-liked party's followers: hire as teachers (rev.), attack, vote (rev.)
    x[, intolerance := (6 - in_range(a$sl21b091, 1, 5) + in_range(a$sl21b092, 1, 5) + 6 - in_range(a$sl21b093, 1, 5)) / 3]
  if (yr == "2019") {
    sg <- sapply(social_items, it)
    x[, social_spread := apply(sg, 1, function(r) if (sum(!is.na(r)) >= 8) sd(r, na.rm = TRUE) else NA_real_)]
  }
  merge(out, x, by = "id", all = TRUE)[, year := yr][]
}
ap <- rbindlist(lapply(names(prefix), read_study), fill = TRUE)


# Party ratings in the politics waves around each study wave --------------------------------

parties <- c("VVD", "D66", "PVV", "CDA", "SP", "PvdA", "GL", "PvdD", "CU", "SGP", "FvD", "TON", "Volt",
             "JA21", "DENK", "50PLUS", "BBB", "BIJ1", "VNL", "NSC", "GL_PvdA")
pol <- fread(here("data", "liss_politics_to26.csv"), select = c("nomem_encr", "wave", "distance", paste0("thermo_", parties)))
pol <- pol[!duplicated(pol[, .(nomem_encr, wave)]) & wave %in% unlist(pol_waves)]
th <- melt(pol[, c("nomem_encr", "wave", paste0("thermo_", parties)), with = FALSE], id.vars = c("nomem_encr", "wave"),
           variable.name = "party", value.name = "rating", variable.factor = FALSE)[, party := sub("thermo_", "", party)]
th <- th[, rating := in_range(rating, 0, 10)][!is.na(rating)]
party_side <- rbindlist(lapply(names(pol_waves), function(yr) rbindlist(lapply(names(pol_waves[[yr]]), function(side) {
  w <- pol_waves[[yr]][[side]]
  t1 <- th[wave == w & party %in% names(voter_items[[yr]])][vs[election_year == election[[yr]]], on = "party", share := i.share]
  t8 <- th[wave == w & party %in% names(voter_items[["2019"]])][vs[election_year == 2017L], on = "party", w17 := i.share]
  merge(merge(pol[wave == w, .(id = nomem_encr, p_dist = distance)],
              t1[!is.na(share), if (.N >= 2) .(p_dist_same = wd(rating, share)), by = .(id = nomem_encr)], by = "id", all = TRUE),
        t8[, if (.N >= 2) .(p_dist8 = wd(rating, w17)), by = .(id = nomem_encr)], by = "id", all = TRUE)[, `:=`(year = yr, side = side)]
}))))
# average of the two politics waves around the study wave (either one if only one is observed)
pw <- dcast(party_side, id + year ~ side, value.var = c("p_dist", "p_dist_same", "p_dist8"))
for (v in c("p_dist", "p_dist_same", "p_dist8"))
  pw[, (v) := rowMeans(cbind(get(paste0(v, "_before")), get(paste0(v, "_after"))), na.rm = TRUE)][is.nan(get(v)), (v) := NA_real_]
d <- merge(ap, pw, by = c("id", "year"))


# 1. Cross-section ------------------------------------------------------------------------------

cor_row <- function(x, y, dt) {
  ok <- complete.cases(dt[[x]], dt[[y]]); if (sum(ok) < 30) return(NULL)
  ct <- cor.test(dt[[x]][ok], dt[[y]][ok])
  data.table(x = x, y = y, r = unname(ct$estimate), lo = ct$conf.int[1], hi = ct$conf.int[2], n = sum(ok))
}
targets <- c("v_dist", "v_spread", "v_min", "v_max", "social_distance", "hate", "intolerance", "issue_ap", "social_spread", "p_dist_same")
cross <- rbindlist(lapply(names(prefix), function(yr) rbind(
  rbindlist(lapply(targets, function(y) cor_row("p_dist", y, d[year == yr]))),
  rbindlist(lapply(c("v_dist", "social_distance", "hate", "intolerance"), function(y) cor_row("p_dist_same", y, d[year == yr]))),
  cor_row("p_dist_before", "p_dist_after", d[year == yr]),                       # benchmark: adjacent waves
  cor_row("v_dist", "social_distance", d[year == yr]), cor_row("v_dist", "hate", d[year == yr]))[, year := yr]))
# party- and voter-based distance net of the spread of a person's ratings of social groups (2019)
d19 <- d[year == "2019" & complete.cases(p_dist, v_dist, social_spread)]
partial <- data.table(
  r_raw = d19[, cor(p_dist, v_dist)],
  r_partial = d19[, cor(resid(lm(p_dist ~ social_spread)), resid(lm(v_dist ~ social_spread)))],
  n = nrow(d19))


# 2. Changes, 2019 to 2021 ------------------------------------------------------------------------

ch <- dcast(d, id ~ year, value.var = c("v_dist8", "p_dist8", "p_dist", "social_distance", "hate", "issue_ap"))
ch[, `:=`(dv = v_dist8_2021 - v_dist8_2019, dp8 = p_dist8_2021 - p_dist8_2019, dp = p_dist_2021 - p_dist_2019,
          dsd = social_distance_2021 - social_distance_2019, dhate = hate_2021 - hate_2019, dissue = issue_ap_2021 - issue_ap_2019)]
changes <- rbind(cor_row("dp8", "dv", ch), cor_row("dp", "dv", ch), cor_row("dp", "dsd", ch), cor_row("dp", "dhate", ch),
                 cor_row("dp", "dissue", ch), cor_row("v_dist8_2019", "v_dist8_2021", ch), cor_row("p_dist8_2019", "p_dist8_2021", ch))
slope <- function(f, dt) { m <- lm(f, data = dt); data.table(term = names(coef(m))[2], b = coef(m)[[2]], se = sqrt(diag(vcov(m)))[[2]], n = nobs(m)) }
# reliability of the change in party distance: the change measured with the waves before the study
# waves (2018/19 to 2020/21) against the change measured with the waves after them (2019/20 to
# 2021/22); Spearman-Brown for the average of the two. Measurement error in the change of party
# distance attenuates the slope of voter-based change on it by this reliability.
ps <- dcast(party_side, id ~ year + side, value.var = "p_dist8")
rel_change <- ps[, cor(`2021_before` - `2019_before`, `2021_after` - `2019_after`, use = "complete.obs")]
rel_change_avg <- 2 * rel_change / (1 + rel_change)
slopes <- rbind(slope(dv ~ dp8, ch)[, model := "change: voter on party distance (same eight parties)"],
                slope(dv ~ dp, ch)[, model := "change: voter (eight parties) on the note's distance"],
                slope(v_dist ~ p_dist, d)[, model := "levels: voter on the note's distance (both years)"],
                slope(social_distance ~ p_dist, d)[, model := "levels: social distance on the note's distance"],
                slope(hate ~ p_dist, d)[, model := "levels: hatred on the note's distance"])


# Losses between the study waves (counts only) ----------------------------------------------------

both <- intersect(ap[year == "2019", id], ap[year == "2021", id])
pn <- load_panels()
losses <- rbindlist(lapply(c("pinc25", "jobloss"), function(tr)
  unique(pn[[tr]][G > 0 & id %in% both, .(id, g = G + 7L)])[, .(between = sum(g %in% 21L), around = sum(g %in% 20:22)), ][, treatment := tr]))

sds <- d[, .(sd_v_dist = sd(v_dist, na.rm = TRUE), sd_p_dist = sd(p_dist, na.rm = TRUE), mean_v_dist = mean(v_dist, na.rm = TRUE),
             mean_p_dist = mean(p_dist, na.rm = TRUE), n = .N), by = year]
slopes[, b_corrected := fifelse(model %like% "^change", b / rel_change_avg, NA_real_)]
res <- list(cross = cross, partial = partial, changes = changes, slopes = slopes, losses = losses, sds = sds,
            reliability = data.table(rel_change_single = rel_change, rel_change_avg = rel_change_avg),
            n = data.table(year = names(prefix), study = ap[, .N, by = year]$N, linked = d[, .N, by = year]$N, in_both = length(both)))
saveRDS(res, path_est("ap_study", "validation"))
options(width = 160)
message("cross-section:"); print(cross[, .(year, x, y, r = round(r, 2), lo = round(lo, 2), hi = round(hi, 2), n)])
message("partial (2019, net of social-group spread):"); print(partial)
message("changes 2019-2021:"); print(changes[, .(x, y, r = round(r, 2), lo = round(lo, 2), hi = round(hi, 2), n)])
message("slopes:"); print(slopes); print(res$reliability); message("losses among people in both waves:"); print(losses); print(sds); print(res$n)

### END
