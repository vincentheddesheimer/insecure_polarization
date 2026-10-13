# New DiD analysis: specification

Agreed 7 Oct 2026. This file records every design choice; the scripts in this folder implement it. Items marked *(default)* were not asked about explicitly and can still be changed.

## Data and sample

- **Input:** `data/liss_to26.csv` (corrected build, waves 8–26). Each outcome uses the waves in which it is measured. Partisan affect is available from wave 12 (none in 15), trust is missing in wave 16, and heterophily is missing in wave 26.
- **Labor-force sample** *(definition details are the default)*:
  - ages 18–65;
  - before job loss, and throughout for the comparison group, only person-waves in which the person is employed (`occupation` 1–3);
  - after job loss, all person-waves are kept whatever the employment status, so the sample is not conditioned on post-treatment outcomes;
  - people unemployed in their first observed wave are dropped, because their first job loss is unknown.
- **Clustering:** by person (`nomem_encr`).

## Treatments

| Analysis | Treatment |
|---|---|
| Job loss, absorbing (main) | cohort G = first wave unemployed (`occupation` 4 or 6) after an employed wave; treated from G on |
| Job loss, on/off (robustness) | unemployed in the current wave (fect, DIDmultiplegtDYN) |
| Reemployment | "off" effect of leaving unemployment in fect, plus the paper-style placebo with CS |
| Household income drop | first above-median (≥10%) drop (`income_hh_rel_decrease_median`), absorbing |
| Personal income drop | first above-median (≥25%) drop (`income_cat_rel_decrease_median`), absorbing |
| Large personal income loss (`pinc25`, candidate main treatment) | first wave in which net personal income (background variables, euros) is at least 25% below the directly preceding wave, after an employed wave; absorbing; all estimators, all / men / women |
| Gender heterogeneity | every job-loss estimator run separately for men and women |

## Outcomes

- **Affective polarization:** `distance`, `spread`, `partisan_affect`.
- **In-/outgroup feelings:** `like_max`, `like_min`.
- **Trust:** `generalized_trust`.
- **Heterophily:** `red_overall_mean_distance` and the four components `red_distance_age_rel_mean`, `red_distance_gender_mean`, `red_distance_education_rel_mean`, `red_distance_ethnicity_rel_mean`.
- **Mechanism outcomes (APSR note only, `mechanism_labels` in `00_setup.R`):** `confidence_index` (mean confidence in the government, parliament, politicians and political parties, 0–10, at least three items answered), `confidence_gov`, `satisfaction_gov`, `satisfaction_dem`. Not in the estimator scripts' outcome loops.
- **Party weights (build, changed Oct 14, 2026):** distance, spread and partisan affect weight parties by their vote share in the last national election *before* fieldwork (previously the following election for the waves fielded Dec 2009, 2011, 2016 and 2020), rescaled to sum to one over the parties each respondent rated (previously raw national shares). Within-wave correlation of old and new distance: 0.96–0.99. Estimates from 02–11 on Della predate this change; 01, 12, 13 and 14 were rerun locally on the corrected data.

## Shared settings

- **Event window:** −5 to +8.
- **Anticipation:** none in the main specification; 1 wave as robustness.
- **Covariates:**
  - main specification: none;
  - robustness: baseline (pre-treatment) values of age group, gender, education, household size and income category, taken from the person's last wave before treatment, or the first observed wave for never-treated people *(default)*.
- **Sensitivity:** HonestDiD relative-magnitude bounds on the main Callaway–Sant'Anna event study.

## Estimators

### Callaway–Sant'Anna, `did` 2.5.1 (main)

- `att_gt(..., est_method = "dr", base_period = "universal", clustervars = "id", bstrap = TRUE, cband = TRUE)`, then `aggte(type = "dynamic", min_e = -5, max_e = 8)` and the overall ATT.
- **Main:** never-treated comparison group, `allow_unbalanced_panel = TRUE`.
- **Robustness:** not-yet-treated comparison group; balanced panel; anticipation = 1; baseline covariates.
- **Inference:** multiplier bootstrap with 1,000 draws *(default)*; uniform bands and pointwise 90/95% intervals.

### fect 2.4.5

- **Methods:** FE, IFE (number of factors by cross-validation), CFE (person-specific linear trends). CFE is the preferred check if FE shows pre-trends.
- **Treatment:** on/off unemployment (gives effects of entering and of leaving unemployment) and absorbing.
- **Tests:** placebo (3 pre-periods), carryover (on/off), equivalence (TOST), leave-one-out.
- **Inference:** bootstrap clustered by person, 200 replications; `min.T0 = 3` *(default)*.

### didimputation 0.5.1 (Borusyak–Jaravel–Spiess)

- **First stage, main:** `~ 0 | id + t`.
- **First stage, robustness:** wave effects interacted with baseline education, gender and age group.
- **Horizons and pre-trends:** `horizon = 0:8`, `pretrends = -5:-1`, `cluster_var = "id"`.

### DIDmultiplegtDYN 2.4.0 (de Chaisemartin–D'Haultfœuille)

- On/off treatment; effects for waves 0–8 and placebos for −5…−2 (see implementation notes).
- Not-yet-switchers as comparison (package default); `normalized = FALSE`; `same_switchers = TRUE`, `same_switchers_pl = TRUE`.

### synthdid (GitHub synth-inference/synthdid)

- **Per job-loss cohort and post-treatment wave:** job losers plus never-unemployed people observed in the pre-waves and that wave; 3 pre-waves (main), 5 pre-waves (robustness, full sample) (see implementation notes).
- **Estimation and aggregation:** synthetic DiD per cohort; ATT averaged with weights by number of treated people; per-period effect curves.
- **Inference:** placebo; cohort variances combined assuming independence across cohorts.

### PanelMatch 3.1.5

- Matching on 4 waves of employment history; CBPS weighting on 4 lagged outcomes plus baseline covariates.
- `qoi = "att"`, leads 0–8 (each matched separately), treatment reversal allowed.
- Estimand: effect of losing the job at t against people still employed at t, whatever happens to either group afterwards (Imai, Kim & Wang 2023); see implementation notes.
- Placebo test on pre-periods (covariate-only refinement; see implementation notes).

### Sun–Abraham and TWFE, `fixest`

- Event dummies from −5 to +8, reference period −1, endpoints binned (≤ −6, ≥ +9); person and wave fixed effects.
- Sun–Abraham uses the never-unemployed as the clean control cohort.

## Implementation notes (decided while writing and testing the scripts)

- **Waves per outcome (all estimators):** each outcome's panel keeps the person-waves with that outcome and numbers the waves in which it was fielded consecutively (`prep_outcome()` in `00_setup.R`), so event time counts fielded waves. The politics module was not fielded in 2015 (wave 15): for distance, spread, like max/min and partisan affect, wave 16 directly follows wave 14, and a job loss in wave 16 has wave 14 as its reference wave (−1) instead of an empty one. A job loss in an unfielded wave starts at the next fielded wave. Other gaps: trust in wave 16, partisan affect before wave 12, heterophily in wave 26.
- **Callaway–Sant'Anna, balanced panel:** the data are balanced before calling `att_gt()` (`balance_panel()`). `did` 2.5.1's own balancing fails with "Internal error: treated group g not found in cohort_vec" when it removes an entire cohort. Pre-balancing gives exactly did's estimates (checked to 1e-10) and drops such a cohort. Fixed upstream in the development version 2.5.1.902 (bcallaway11/did PR #275, Aug 2026; not yet on CRAN), with the same result: the cohort is dropped with a warning.
- **fect:** fect numbers the first treated wave as event time 1 (and the first wave after exit as 1); all output is shifted so that 0 = first treated wave. Gender splits use FE only.
- **DIDmultiplegtDYN:** needs `polars` attached. It uses `effects = 9, placebo = 4`, so that the estimates cover exactly −5…+8 (Effect_l → l−1, Placebo_l → −1−l). Its overall "average total effect" is per unit of accumulated unemployment and is not compared with the other ATTs.
- **Sun–Abraham / TWFE:**
  - With the per-outcome wave numbering every cohort has a reference wave. Before it, the wave-16 cohort had none for the politics outcomes (standard errors in the thousands). A safeguard still drops, and reports, any cohort without data in its reference wave.
  - Sun–Abraham's overall effect is aggregated over event times 0–8 only.
- **synthdid:**
  - Estimated per cohort *and* per post-treatment wave (k pre-waves plus that one wave); requiring the full −5…+8 window kept too few job losers. For distance at h = 0/2/4/8 the main design (k = 3) keeps 278/214/152/69 of 698 job losers, k = 5 keeps 173/135/97/43 (calendar waves with k = 5: 72/77/68/32).
  - Per-wave placebo SEs are combined across cohorts as if independent (approximate, because cohorts share controls).
  - The overall SE is a conservative bound (perfect correlation across waves within a cohort).
- **PanelMatch:**
  - **Lead by lead:** each lead (0–8) is matched separately, because PanelMatch keeps a job loss only if the person is observed in every wave up to the lead. Joint leads 0–8 kept 18 job losses; per lead with the per-outcome wave numbering, distance keeps 437/243/131/77/43 at leads 0/2/4/6/8. Event counts per lead are saved.
  - **Estimand:** matched controls only need to be employed at t; some lose their job by t+h, and their outcomes count as untreated. This is PanelMatch's ATT (effect of switching at t, not conditioning on later treatment) and differs from Callaway–Sant'Anna (never-unemployed controls) and dCDH (controls not yet switched at t+h). With effects of the same sign, it pulls PanelMatch's estimates toward zero at later leads. The weighted share of matched controls unemployed at t+h is saved per lead (`events$ctrl_treated`).
  - **SEs:** analytic ("conditional"); the block bootstrap takes about a day and can be switched on with `DID_PM_BOOT`.
  - **Placebo:** run for lead 0 with covariate-only refinement and analytic SEs.

- **Known-answer checks (`checks.R`):** every estimator script first runs its own estimation function on a simulated panel and stops if it misses the truth. The panel has person and wave effects, effects that grow by 0.5 per wave since treatment (so an off-by-one event time is off by 0.5), a wave without the outcome that is one cohort's calendar reference wave, and almost no noise. The checks cover:
  - event-time mapping, reference waves and wave renumbering;
  - aggregation (CS overall, BJS overall, Sun–Abraham's 0–8 aggregate, fect's average);
  - all CS specifications, including the balanced-panel workaround;
  - fect's event-time shift, dCDH's effect/placebo mapping and polars, synthdid cells for every cohort and number of pre-waves;
  - PanelMatch's lead mapping (matches a by-hand matched-set contrast to 1e-15) and its estimand.

  Any standard error above 0.25 also fails a check, to catch exploding standard errors. `00_checks.R` runs all checks with `DID_CHECK_ONLY = 1` and writes `results/tables/checks.txt`; run it after any package update.

## Running

From the repo root (paths resolve with `here()`; packages are pinned by renv):

```r
source("code/analyze_did/01_sample.R")   # panels -> data/analysis/panels.rds (seconds)
source("code/analyze_did/02_cs.R")       # ~1 h incl. HonestDiD
source("code/analyze_did/03_fect.R")     # several hours (FE/IFE/CFE x 2 codings x tests)
source("code/analyze_did/04_imputation.R")
source("code/analyze_did/05_dcdh.R")
source("code/analyze_did/06_sunab_twfe.R")
source("code/analyze_did/07_synthdid.R") # ~8-10 h (placebo SEs per cohort and wave)
source("code/analyze_did/08_panelmatch.R")  # ~1.5 h with analytic SEs
source("code/analyze_did/09_compare.R")  # comparison figures and table
source("code/analyze_did/10_dip.R")      # the t-1 dip: reference-wave contrasts, cohorts, job-loss expectations
source("code/analyze_did/11_power.R")    # minimum detectable effects
source("code/analyze_did/12_alt_treatments.R")  # candidate treatments: income drops, job insecurity, hardship
source("code/analyze_did/13_followups.R")  # transience, periods, PVV rating, income-loss checks
```

- **Test mode:** `Sys.setenv(DID_TEST = "1")` runs only distance with fewer bootstrap draws and writes `*_TEST` files.
- **Fewer outcomes:** `Sys.setenv(DID_OUTCOMES = "distance,spread")`.
- **Fewer fect methods:** `Sys.setenv(DID_FECT_METHODS = "fe")` (result files are then tagged with the methods).
- **Fewer treatments:** `Sys.setenv(DID_TREATMENTS = "pinc25")` runs 02-08 for the listed treatments only; their result files are tagged with the treatments, so other runs are never overwritten.
- **Checks only:** `Sys.setenv(DID_CHECK_ONLY = "1")`; all of them: `Rscript code/analyze_did/00_checks.R`.
- **Estimates only / figures only:** `DID_NO_FIGURES = "1"` / `DID_FIGURES_ONLY = "1"`. Estimates are saved per outcome (`<estimator>__<outcome>[__<methods>].rds`), so runs for different outcomes can run in parallel; `load_est()` stops if two files hold the same estimates.

### Running on Della (Princeton Research Computing)

The estimation runs on Della; figures and tables are drawn locally from the estimates.

- **Project:** `/scratch/gpfs/DANCYGIER/vh4264/insecure_polarization`, a private folder (`chmod 700`, because LISS is licensed). It holds a copy of `code/`, `renv.lock`, `renv/activate.R`, `.Rprofile`, `data/liss_to26.csv` and `data/analysis/panels.rds`.
- **Setup (once, on a login node):** `bash code/analyze_did/della/setup.sh`. It installs the lockfile versions (279 packages) with R 4.6.0 and verifies them. Notes:
  - CRAN packages come as RHEL 9 binaries from Posit Package Manager.
  - polars uses its pre-built Rust library (official release, SHA-256 checked), because Della's Rust is too old to compile it.
  - magick and haschaR are not installed (ImageMagick is missing on Della). That is why `00_setup.R` loads haschaR only when drawing figures.
- **Submit (login node, from the project folder):** `bash code/analyze_did/della/submit.sh test`, then `... submit.sh full`.
  - The first job is `00_checks.R`; the estimation jobs start only if it passes.
  - Full runs are array jobs with one task per outcome (fect: per outcome × method). Logs go to `code/analyze_did/della/logs/`.
- **Large income loss (pinc25), all estimators:** `OUTCOME_LIST="distance spread like_min" DID_TREATMENTS=pinc25 bash code/analyze_did/della/submit.sh full` (jobs 15414107-15414114, Oct 12, 2026). Rebuild the panels first (`SCRIPTS=01_sample.R sbatch --export=ALL code/analyze_did/della/run_r.slurm`).
- **Candidate treatments and follow-ups:** `sbatch code/analyze_did/della/run_alt.slurm` runs 12; `SCRIPTS=13_followups.R sbatch --export=ALL code/analyze_did/della/run_r.slurm` runs 13, which also needs `data/liss_politics_to26.csv` (the build's cleaned politics module, for the PVV rating).
- **Job limits:** keep time limits above one hour; shorter jobs land in Della's test QOS, which caps the number of jobs per user.
- **Follow-ups:** `sbatch code/analyze_did/della/run_followup.slurm` runs 10 and 11 (about 1 minute). 10 needs `data/liss_income_to26.csv`, the build's cleaned income module, for the job-loss expectation item (`pr_lose_job`, not in `liss_to26.csv`).
- **Collect:** copy `results/estimates/` back, then locally run each script with `DID_FIGURES_ONLY = 1`, and `09_compare.R`.
- **Full run, Oct 7, 2026:** jobs 15192722 (checks) and 15192723–15192729 (99 tasks, all COMPLETED, every check passed, no failed estimation). Longest task per script: fect 1 h 34 min, synthdid 52 min, PanelMatch 18 min, CS 7 min, dCDH 2 min, Sun–Abraham/TWFE and BJS under 1 min. The ledger is `logs/submitted_jobs.csv` in the Della project folder. Code was synced from the uncommitted working tree on branch `fix/build-audit`. fect's diagnostic plots, written during estimation, are in `results/figures/fect_diag/`.

## Outputs *(default)*

- **Estimates:** `.rds` in `results/estimates/<estimator>/`.
- **Figures:** `.pdf` in `results/figures/`, with `haschaR::theme_hanno()` and no end caps on error bars.
- **Tables:** `.tex` in `results/tables/`.
- **Overleaf:** nothing is written to Overleaf automatically, except by `14_apsr_note.R`, which writes the APSR note's exhibits and `numbers.tex` to `Apps/Overleaf/HB_insecure_polarization/apsr/` (agreed Oct 13, 2026).
- **Packages:** pinned with renv (`renv.lock` in the repo root).

## APSR research note (`14_apsr_note.R`, October 2026)

- **Treatments:** large personal income loss (`pinc25`, leads) and job loss (`jobloss`, supporting evidence).
- **Main estimator:** Callaway–Sant'Anna, never-treated controls, universal base period, no covariates, analytic (influence-function) standard errors so that window averages have their full covariance. Summaries: average over waves 0–8 and over waves 2–4.
- **Robustness, one check per identifying assumption:** not-yet-treated controls (comparison group), baseline covariates (selection on observables), waves −5 to −3 as baseline (anticipation), imputation estimator (parallel trends in all waves; more efficient), Rambachan–Roth relative-magnitude and smoothness bounds (size of violations). Sun–Abraham, TWFE, synthdid, PanelMatch, dCDH and fect are not used in the note.
- **Inputs:** the panels (01) and the saved results of 10, 12 and 13, which must be rerun on the same data. Fits and bounds are cached in `results/estimates/apsr/refits.rds`; `DID_APSR_REFIT=1` redoes them (about 45 minutes locally, most of it the relative-magnitude bounds).
- **Outputs:** figures, tables and `numbers.tex` (every number in the note's text as a LaTeX macro) in the Overleaf project's `apsr/` folder.
