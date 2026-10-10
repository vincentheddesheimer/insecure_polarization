#!/bin/bash
# Follow-up analyses on Della (from the della skill's cpu_r template): check of the shared
# Sun-Abraham builder (06 in check-only mode), then 10_dip.R (the t-1 dip) and 11_power.R
# (minimum detectable effects). Estimates only; figures and tables are drawn locally.
# Submit from the project root:  sbatch code/analyze_did/della/run_followup.slurm
#SBATCH --job-name=did_followup
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=01:02:00
#SBATCH --output=code/analyze_did/della/logs/%x_%j.out

set -eo pipefail                        # no `set -u` before module init ($PS1 is unbound)

cd "$SLURM_SUBMIT_DIR"
module purge
module load R/4.6.0
# renv is activated by the project's .Rprofile (so no --vanilla); BLAS single-threaded, see run_script.slurm
export DID_NO_FIGURES=1 RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 FLEXIBLAS_NUM_THREADS=1
echo "job=$SLURM_JOB_ID host=$(hostname) start=$(date -u +%FT%TZ)"

DID_CHECK_ONLY=1 Rscript code/analyze_did/06_sunab_twfe.R
Rscript code/analyze_did/10_dip.R
Rscript code/analyze_did/11_power.R
echo "done $(date -u +%FT%TZ)"
