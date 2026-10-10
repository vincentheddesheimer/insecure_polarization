#!/bin/bash
# One estimator script on Della, submitted by submit.sh. Array task i = outcome i of OUTCOME_LIST
# (03_fect.R: outcome i / 3 x method i % 3). Estimates only; figures are drawn locally.
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=4G
#SBATCH --output=code/analyze_did/della/logs/%x_%A_%a.out
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load R/4.6.0
export DID_NO_FIGURES=1 RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
# BLAS single-threaded: Della's R uses OpenBLAS with OpenMP, and a process that forks workers
# (did's pl = TRUE, fect's bootstrap, mclapply in synthdid) after BLAS has started its OpenMP threads
# deadlocks the workers (seen with did: workers stuck in futex_wait). Parallelism comes from the
# forked workers; data.table and fixest get their thread counts in 00_setup.R.
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 FLEXIBLAS_NUM_THREADS=1 POLARS_MAX_THREADS=$SLURM_CPUS_PER_TASK

read -r -a OUTCOMES <<< "$OUTCOME_LIST"
METHODS=(fe ife cfe)
i=${SLURM_ARRAY_TASK_ID:-0}
if [ "${DID_TEST:-0}" != "1" ] && [ "$SCRIPT" != "00_checks.R" ]; then
  if [ "$SCRIPT" = "03_fect.R" ]; then
    export DID_OUTCOMES=${OUTCOMES[$((i / 3))]} DID_FECT_METHODS=${METHODS[$((i % 3))]}
  else
    export DID_OUTCOMES=${OUTCOMES[$i]}
  fi
fi

echo "$(date '+%F %T') $SCRIPT outcomes=${DID_OUTCOMES:-test} methods=${DID_FECT_METHODS:-all} test=${DID_TEST:-0} cpus=$SLURM_CPUS_PER_TASK node=$(hostname)"
Rscript "code/analyze_did/$SCRIPT"
echo "$(date '+%F %T') done"
