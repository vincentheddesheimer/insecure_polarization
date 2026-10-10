### New DiD analysis: compare estimators
# Stacks the saved results of 02-08 and draws:
#   compare_jobloss_absorbing  event studies, estimators for first job loss (absorbing)
#   compare_jobloss_onoff      event studies, estimators for unemployment on/off (CS for reference)
#   compare_overall_jobloss    overall effects of job loss by estimator (forest plot) + .tex table
#   compare_gender             overall effects for men and women by estimator
#   compare_reemployment       effects of leaving unemployment (fect) and the CS placebo
#   compare_income             household and personal income drops by estimator
# Estimators whose results are missing are skipped. Event time counts the waves in which each
# outcome was fielded (prep_outcome in 00_setup.R). dCDH's overall effect is per unit of
# accumulated unemployment and is left out of the overall comparisons.

rm(list = ls())
here::i_am("code/analyze_did/09_compare.R")
source(here::here("code", "analyze_did", "00_setup.R"))
pacman::p_load(knitr)

files <- c("cs", "fect", "imputation", "dcdh", "sunab_twfe", "synthdid", "panelmatch")
ests <- lapply(setNames(files, files), function(e) {
  r <- load_est(e)
  if (is.null(r)) message("no saved results for ", e)
  r
})
tidy    <- rbindlist(lapply(ests, `[[`, "tidy"), fill = TRUE)
overall <- rbindlist(lapply(ests, `[[`, "overall"), fill = TRUE)

# main job-loss estimate of each estimator; group = which comparison figure it goes into
sel <- fread(text = "
estimator,spec,treatment,label,group
CS,main,jobloss,Callaway-Sant'Anna,absorbing
Imputation (BJS),main,jobloss,Imputation (BJS),absorbing
Sun-Abraham,main,jobloss,Sun-Abraham,absorbing
TWFE,main,jobloss,TWFE,absorbing
synthdid,main,jobloss,Synthetic DiD,absorbing
fect,fe,jobloss,fect FE,absorbing
CS,main,jobloss,Callaway-Sant'Anna (absorbing),onoff
fect,fe,jobloss_onoff,fect FE (on/off),onoff
dCDH,main,jobloss_onoff,dCDH (first switch),onoff
PanelMatch,main,jobloss_onoff,PanelMatch,onoff")
fig_h <- function(x) 2.5 + 2.2 * ceiling(uniqueN(x$outcome) / 2)


# Event studies: job loss -----------------------------------------------------

# wide figures with the estimators spread over most of each wave, so their coefficients within a
# wave are easy to tell apart
for (g in c("absorbing", "onoff")) {
  s <- sel[group == g]
  x <- merge(tidy[sample == "all"], s, by = c("estimator", "spec", "treatment"))
  if (!nrow(x)) next
  x[, label := factor(label, intersect(s$label, unique(label)))]
  ggsave(path_fig(paste0("compare_jobloss_", g)),
         plot_es(x, color = "label", dodge = 0.8, xlab = "Waves relative to job loss"),
         width = 20, height = fig_h(x))
}


# Overall effects: job loss -----------------------------------------------------

ov_sel <- sel[group == "absorbing" | label == "fect FE (on/off)"][, .(estimator, spec, treatment, label)]
ov <- merge(overall[sample == "all"], ov_sel, by = c("estimator", "spec", "treatment"))
if (nrow(ov)) {
  ov[, `:=`(label = factor(label, rev(ov_sel$label)), outcome_label = factor(outcome_labels[outcome], outcome_labels))]
  p <- ggplot(ov, aes(att, label)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_linerange(aes(xmin = att - 1.96 * se, xmax = att + 1.96 * se), linewidth = 0.4) +
    geom_linerange(aes(xmin = att - 1.645 * se, xmax = att + 1.645 * se), linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ outcome_label, scales = "free_x", ncol = 2) +
    theme_hanno() +
    labs(x = "Average effect of job loss, waves 0-8 (95% and 90% CI)", y = NULL,
         caption = "TWFE: static two-way FE. Synthetic DiD: conservative SE. fect on/off: effect while unemployed.")
  ggsave(path_fig("compare_overall_jobloss"), p, width = 10, height = fig_h(ov))

  tab <- dcast(ov[, cell := sprintf("%.3f (%.3f)", att, se)], label ~ outcome, value.var = "cell")
  setnames(tab, c("Estimator", outcome_labels[names(tab)[-1]]))
  writeLines(kable(tab[order(match(Estimator, ov_sel$label))], format = "latex", booktabs = TRUE, linesep = ""),
             path_tab("compare_overall_jobloss"))
}


# Gender ------------------------------------------------------------------------

gen <- overall[sample %in% c("men", "women") & treatment %in% c("jobloss", "jobloss_onoff") &
                 estimator != "dCDH" & spec %in% c("main", "fe")]
if (nrow(gen)) {
  gen[, `:=`(label = paste0(estimator, fifelse(treatment == "jobloss_onoff", " (on/off)", "")),
             sample = factor(sample, c("men", "women"), c("Men", "Women")),
             outcome_label = factor(outcome_labels[outcome], outcome_labels))]
  p <- ggplot(gen, aes(att, label, color = sample, shape = sample)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_linerange(aes(xmin = att - 1.96 * se, xmax = att + 1.96 * se), linewidth = 0.4,
                   position = position_dodge(width = 0.5)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    scale_color_manual(values = series_pal[1:2]) + scale_shape_manual(values = series_shapes[1:2]) +
    facet_wrap(~ outcome_label, scales = "free_x", ncol = 2) +
    theme_hanno() + theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(x = "Average effect of job loss, waves 0-8 (95% CI)", y = NULL)
  ggsave(path_fig("compare_gender"), p, width = 10, height = fig_h(gen))
}


# Reemployment --------------------------------------------------------------------

re <- rbind(tidy[estimator == "fect" & spec == "fe" & treatment == "reemp_off" & sample == "all"][, label := "fect FE: leaving unemployment"],
            tidy[estimator == "CS" & spec == "main" & treatment == "reemp" & sample == "all"][, label := "Callaway-Sant'Anna: reemployment placebo (paper design)"])
if (nrow(re)) ggsave(path_fig("compare_reemployment"), plot_es(re, color = "label", xlab = "Waves relative to reemployment"),
                     width = 10, height = fig_h(re))


# Income drops ------------------------------------------------------------------------

inc <- tidy[treatment %in% c("hh_inc", "inc") & sample == "all" & spec %in% c("main", "fe")]
if (nrow(inc)) for (tr in c("hh_inc", "inc")) {
  x <- inc[treatment == tr][, label := factor(estimator)]
  if (nrow(x)) ggsave(path_fig(paste0("compare_", tr)),
                      plot_es(x, color = "label", xlab = paste("Waves relative to", if (tr == "hh_inc") "household" else "personal", "income drop")),
                      width = 10, height = fig_h(x))
}

### END
