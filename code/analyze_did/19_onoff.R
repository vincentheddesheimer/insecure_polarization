### Effect of being unemployed (on/off coding), for the SI of the APSR note
# The note's treatments are absorbing: once a person has lost a job or income, every later wave
# counts as treated. This script estimates a different quantity, the effect of currently being
# unemployed, with the treatment switching off when the person finds work again (panel
# jobloss_onoff in 01_sample.R: labor-force waves only, people unemployed in their first
# labor-force wave dropped). Estimator: fect with two-way fixed effects (counterfactual
# imputation), bootstrap clustered by person (200 replications). Reports the average effect over
# unemployed waves, effects by wave since entering unemployment, the effects in the waves after
# leaving unemployment, and fect's pre-trend F test. Outcome: distance only.
# Saves results/estimates/apsr/onoff.rds. Runtime: about 15-30 minutes locally.

rm(list = ls())
here::i_am("code/analyze_did/19_onoff.R")
Sys.setenv(DID_NO_FIGURES = "1")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(fect)

d <- prep_outcome(load_panels()$jobloss_onoff, "distance", absorbing = FALSE)
f <- fect(Y = "distance", D = "D", data = as.data.frame(d[, .(id, t, D, distance)]), index = c("id", "t"),
          force = "two-way", method = "fe", se = TRUE, vartype = "bootstrap", nboots = 200, cl = "id",
          parallel = TRUE, cores = n_cores, min.T0 = 3, seed = 20261014)
# fect labels the first treated wave 1 (and the first wave after exit 1); shift to 0
tidy <- function(tab, col) { tab <- as.data.table(tab, keep.rownames = "time")
  data.table(term = as.numeric(tab$time) - 1, estimate = tab[[col]], se = tab$S.E., n = tab$count) }
res <- list(att = data.table(estimate = f$est.avg[1, "ATT.avg"], se = f$est.avg[1, "S.E."]),
            es = tidy(f$est.att, "ATT"), off = if (!is.null(f$est.att.off)) tidy(f$est.att.off, "ATT.OFF"),
            pre_p = f$test.out$f.p, n_units = f$N, n_treated = f$Ntr,
            sd = sd(d[G == 0, distance]))
saveRDS(res, path_est("apsr", "onoff"))
print(res$att); print(res$es[term %between% c(-5, 8)]); print(res$off[term %between% c(0, 4)]); cat("pre-trend p:", res$pre_p, "\n")

### END
