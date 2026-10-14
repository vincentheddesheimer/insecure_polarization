### Imputed income: how many large income losses (pinc25) involve an imputed income value?
# LISS imputes net monthly personal income (background variable nettoink_f) when the reported value
# (nettoink) is missing. An event involves an imputed value if income in the event wave or in the
# directly preceding wave is imputed. Re-estimates the main Callaway-Sant'Anna averages (waves 0-8
# and 2-4) without the people whose event involves an imputed value.
# Needs the raw LISS background files (LISS_RAW in .Renviron: background/*.dta, cleaned/liss_combined_to26.csv).
# Saves results/estimates/apsr/imputed_check.rds (read by 14_apsr_note.R).

rm(list = ls())
here::i_am("code/analyze_did/16_imputed_income.R")
Sys.setenv(DID_NO_FIGURES = "1")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(readstata13, did)
R <- Sys.getenv("LISS_RAW")
f <- list.files(file.path(R, "background"), pattern = "[.]dta$", full.names = TRUE)
bg <- rbindlist(lapply(f, function(x) {
  v <- names(read.dta13(x, select.rows = 2))
  if (!"nettoink_f" %in% v) return(NULL)
  d <- suppressWarnings(read.dta13(x, select.cols = c("nomem_encr", "wave", "nettoink", "nettoink_f")))
  as.data.table(d)[, nettoink := as.numeric(as.character(nettoink))]
}), fill = TRUE)
bg[, imputed := (is.na(nettoink) | nettoink < 0) & !is.na(nettoink_f)]
bg[, changed := !is.na(nettoink) & nettoink >= 0 & !is.na(nettoink_f) & nettoink != nettoink_f]
cat("person-months:", nrow(bg), " imputed:", sum(bg$imputed), sprintf("(%.2f%%)", 100 * mean(bg$imputed)), " _f differs from reported:", sum(bg$changed), "\n")
# the month from which the build takes each person-wave's background variables (08_LISS_combine.R)
comb <- fread(file.path(R, "cleaned", "liss_combined_to26.csv"), select = c("nomem_encr", "wave", "date_bg"))
comb <- unique(comb[!is.na(date_bg)])[, .(nomem_encr, wave, date = date_bg)]
setnames(bg, "wave", "date"); bg[, date := as.integer(date)]
pw <- merge(comb, bg[, .(nomem_encr, date, imputed)], by = c("nomem_encr", "date"), all.x = TRUE)
pw <- pw[, .(imputed = any(imputed %in% TRUE)), by = .(nomem_encr, wave)]
pw[, t := as.integer(wave) - 7L]
panels <- load_panels(); p <- copy(panels$pinc25)
p <- merge(p, pw[, .(id = nomem_encr, t, imputed)], by = c("id", "t"), all.x = TRUE)
p[is.na(imputed), imputed := FALSE]
setorder(p, id, t)
# event involves an imputed value if income in the event wave or the directly preceding wave is imputed
evt <- p[G > 0 & t == G, .(id, G)]
prev <- pw[, .(id = nomem_encr, t, imp = imputed)]
evt <- merge(evt, prev[, .(id, t_ev = t, imp_ev = imp)], by.x = c("id", "G"), by.y = c("id", "t_ev"), all.x = TRUE)
evt[, t_pre := G - 1L]
evt <- merge(evt, prev[, .(id, t_pre = t, imp_pre = imp)], by = c("id", "t_pre"), all.x = TRUE)
evt[, imp_any := (imp_ev %in% TRUE) | (imp_pre %in% TRUE)]
cat("income-loss events:", nrow(evt), " involving an imputed value:", sum(evt$imp_any), sprintf("(%.1f%%)", 100 * mean(evt$imp_any)), "\n")
drop_ids <- evt[imp_any == TRUE, id]
# main specification (Callaway-Sant'Anna, as in the note): averages over waves 0-8 and 2-4
fit <- function(d) { f <- cs_fit(d, "distance"); sapply(list(avg = ev(0:8, 1/9), mid = ev(2:4, 1/3)), function(w) lincom(f$b, f$V, w)) }
main <- fit(panels$pinc25); noimp <- fit(panels$pinc25[!id %in% drop_ids])
cat("\nmain (all events):\n"); print(round(main, 3))
cat("\nwithout events involving imputed income (", length(drop_ids), "people dropped):\n"); print(round(noimp, 3))
saveRDS(list(n_events = nrow(evt), n_imp = sum(evt$imp_any), share_months = mean(bg$imputed), main = main, noimp = noimp),
        path_est("apsr", "imputed_check"))

### END
