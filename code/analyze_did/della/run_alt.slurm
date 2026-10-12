#!/bin/bash
# Alternative treatments (12_alt_treatments.R), after re-running 10_dip.R to confirm that moving
# its helpers to 00_setup.R left its results unchanged. Estimates only; figures drawn locally.
# Submit from the project root:  sbatch code/analyze_did/della/run_alt.slurm
#SBATCH --job-name=did_alt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=01:02:00
#SBATCH --output=code/analyze_did/della/logs/%x_%j.out

set -eo pipefail                        # no `set -u` before module init ($PS1 is unbound)

cd "$SLURM_SUBMIT_DIR"
module purge
module load R/4.6.0
export DID_NO_FIGURES=1 RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 FLEXIBLAS_NUM_THREADS=1
echo "job=$SLURM_JOB_ID host=$(hostname) start=$(date -u +%FT%TZ)"

cp results/estimates/dip/dip.rds /tmp/dip_before_$SLURM_JOB_ID.rds
Rscript code/analyze_did/10_dip.R
Rscript -e 'a <- readRDS(commandArgs(TRUE)[1]); b <- readRDS("results/estimates/dip/dip.rds")
  ok <- isTRUE(all.equal(a$contrasts, b$contrasts)) && isTRUE(all.equal(a$recentred, b$recentred))
  cat("10_dip.R unchanged after moving helpers:", ok, "\n"); if (!ok) quit(status = 1)' /tmp/dip_before_$SLURM_JOB_ID.rds
Rscript code/analyze_did/12_alt_treatments.R
echo "done $(date -u +%FT%TZ)"
