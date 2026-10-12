#!/bin/bash
# Submit the DiD analysis on Della, from the project root on a login node:
#   bash code/analyze_did/della/submit.sh test   # DID_TEST: distance only, few bootstrap draws
#   bash code/analyze_did/della/submit.sh full   # all outcomes: one array task per outcome
#                                                # (03_fect.R: per outcome x method)
# The first job runs 00_checks.R (known-answer checks of every estimator); the estimation jobs
# start only if it passes, and every task repeats its estimator's check before estimating.
# Estimates go to results/estimates/<estimator>/. Figures and comparison tables are drawn locally
# after copying the estimates back (see SPEC.md, "Running on Della").
set -euo pipefail
MODE=${1:?usage: submit.sh test|full}
D=code/analyze_did/della
mkdir -p "$D/logs"
module purge
module load R/4.6.0
export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE

# outcomes from 00_setup.R (single source of truth), unless OUTCOME_LIST is set, e.g.
#   OUTCOME_LIST="distance spread like_min" DID_TREATMENTS=pinc25 bash code/analyze_did/della/submit.sh full
OUTCOME_LIST=${OUTCOME_LIST:-$(DID_NO_FIGURES=1 Rscript -e 'suppressMessages(source("code/analyze_did/00_setup.R")); cat(names(outcome_labels))')}
N=$(wc -w <<< "$OUTCOME_LIST")
export OUTCOME_LIST
if [ "$MODE" = test ]; then export DID_TEST=1; else unset DID_TEST; fi

# cpus and time limit per script (full mode); test mode: single task, 8 cpus, 3 hours
declare -A CPUS=([02_cs.R]=8 [03_fect.R]=16 [04_imputation.R]=4 [05_dcdh.R]=4 [06_sunab_twfe.R]=4 [07_synthdid.R]=16 [08_panelmatch.R]=4)
# (test run, distance: CS 4 min incl. HonestDiD, fect FE 8, BJS 0.5, dCDH 1.3, Sun-Abraham/TWFE 0.4, synthdid 9 with 20 placebo reps,
#  PanelMatch 11; full runs have 4-5x the bootstrap/placebo draws)
# limits above one hour keep the jobs out of the test QOS, which caps the number of jobs per user
declare -A TIME=([02_cs.R]=02:00:00 [03_fect.R]=12:00:00 [04_imputation.R]=01:02:00 [05_dcdh.R]=01:02:00 [06_sunab_twfe.R]=01:02:00 [07_synthdid.R]=03:00:00 [08_panelmatch.R]=03:00:00)

checks=$(SCRIPT=00_checks.R sbatch --parsable --job-name=did_00_checks --cpus-per-task=4 --time=01:02:00 \
         --output="$D/logs/%x_%j.out" --export=ALL "$D/run_script.slurm")
echo "checks: job $checks"
for s in 02_cs.R 03_fect.R 04_imputation.R 05_dcdh.R 06_sunab_twfe.R 07_synthdid.R 08_panelmatch.R; do
  if [ "$MODE" = test ]; then
    args=(--cpus-per-task=8 --time=03:00:00 --output="$D/logs/%x_%j.out")
  else
    n=$N; [ "$s" = 03_fect.R ] && n=$((3 * N))
    args=(--cpus-per-task="${CPUS[$s]}" --time="${TIME[$s]}" --array=0-$((n - 1)))
  fi
  id=$(SCRIPT=$s sbatch --parsable --job-name="did_${s%.R}" --dependency=afterok:"$checks" --kill-on-invalid-dep=yes \
       "${args[@]}" --export=ALL "$D/run_script.slurm")
  echo "$s: job $id"
done
