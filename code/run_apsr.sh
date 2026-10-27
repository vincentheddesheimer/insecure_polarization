#!/usr/bin/env bash
# Rebuild the data and every number, table and figure of the APSR note, in order.
# Run from anywhere: bash code/run_apsr.sh   (about two hours on a laptop)
# Needs the raw LISS data in LISS_RAW (see code/build/liss/00_paths.R and .Renviron), including the
# Work and Schooling zips in LISS_RAW/work_schooling/zip (read by 17) and LISS study 233 in
# LISS_RAW/affective_polarization (read by 18).
# Order: build (module cleaning, combine, compilation) -> copies of the cleaned politics and income
# modules in data/ -> panels -> analyses whose saved results the note reads (10, 12, 13, 15, 16, 17, 18,
# 19) -> exhibits and numbers (14, written to the Overleaf folder).
set -euo pipefail
cd "$(dirname "$0")/.."

run() { echo "== $(date +%H:%M) $1"; Rscript "$1"; }

for f in code/build/liss/clean/0[1-8]_*.R; do run "$f"; done
run code/build/liss/01_Liss_data_compilation.R
# 10, 12, 13, 15, 17 and 18 read the cleaned modules from data/ (licensed microdata, ignored by git);
# the build writes them to LISS_RAW/cleaned (code/build/liss/00_paths.R)
liss_clean=$(Rscript -e 'cat(file.path(path.expand(Sys.getenv("LISS_RAW", unset = "data/raw/liss")), "cleaned"))' | tail -n 1)   # last line: renv may print a notice first
cp "$liss_clean/liss_politics_to26.csv" "$liss_clean/liss_income_to26.csv" data/
run code/analyze_did/01_sample.R
for f in 10_dip 12_alt_treatments 13_followups 15_mechanisms 16_imputed_income 17_extensions 18_ap_validation 19_onoff; do run "code/analyze_did/$f.R"; done
run code/analyze_did/14_apsr_note.R
echo "== $(date +%H:%M) done"
