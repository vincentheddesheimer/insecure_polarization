#!/bin/bash
# Run R scripts of code/analyze_did one after the other on Della (estimates only; figures locally).
# Submit from the project root:  SCRIPTS="01_sample.R 13_followups.R" sbatch --export=ALL code/analyze_did/della/run_r.slurm
#SBATCH --job-name=did_r
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
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 FLEXIBLAS_NUM_THREADS=1   # see run_script.slurm
echo "job=$SLURM_JOB_ID host=$(hostname) start=$(date -u +%FT%TZ) scripts=$SCRIPTS"
for s in $SCRIPTS; do
  echo "== $s $(date -u +%T)"
  Rscript "code/analyze_did/$s"
done
echo "done $(date -u +%FT%TZ)"
