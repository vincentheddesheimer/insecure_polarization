#!/usr/bin/env bash
# Rebuild the data and every number, table and figure of the APSR note, in order.
# Run from anywhere: bash code/run_apsr.sh   (about two hours on a laptop)
# Needs the raw LISS data in LISS_RAW (see code/build/liss/00_paths.R and .Renviron).
# Order: build (module cleaning, combine, compilation) -> panels -> analyses whose saved results the
# note reads (10, 12, 13, 15, 16) -> exhibits and numbers (14, written to the Overleaf folder).
set -euo pipefail
cd "$(dirname "$0")/.."

run() { echo "== $(date +%H:%M) $1"; Rscript "$1"; }

for f in code/build/liss/clean/0[1-8]_*.R; do run "$f"; done
run code/build/liss/01_Liss_data_compilation.R
run code/analyze_did/01_sample.R
for f in 10_dip 12_alt_treatments 13_followups 15_mechanisms 16_imputed_income; do run "code/analyze_did/$f.R"; done
run code/analyze_did/14_apsr_note.R
echo "== $(date +%H:%M) done"
