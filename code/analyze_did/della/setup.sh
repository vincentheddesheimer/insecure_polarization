#!/bin/bash
# One-time setup of the R library on Della (Princeton Research Computing).
# Run on a LOGIN node (compute nodes have no internet), from the project root:
#   bash code/analyze_did/della/setup.sh > code/analyze_did/della/setup.log 2>&1 &
# Installs the package versions pinned in renv.lock into renv/library. CRAN packages come as
# RHEL 9 binaries from Posit Package Manager (same versions as the lockfile); polars from
# GitHub at the pinned commit, with its pre-built Rust library (official pola-rs/r-polars release,
# SHA-256 checked against the pinned commit; compiling needs a newer Rust than Della has);
# synthdid from GitHub at the pinned commit. Not installed: magick (needs the
# ImageMagick system library, which Della lacks) and haschaR, which imports it; both are only
# needed for figures, which are drawn locally from the estimates computed here.
set -euo pipefail
module purge
module load R/4.6.0
export LIBR_POLARS_BUILD=false

Rscript -e '
  options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/rhel9/latest",
                    multiverse = "https://r-multiverse.r-universe.dev"),
          Ncpus = 4)
  skip <- c("magick", "haschaR")
  renv::restore(prompt = FALSE, repos = getOption("repos"), exclude = skip)
  lock <- renv::lockfile_read()$Packages
  lock <- lock[setdiff(names(lock), skip)]
  same <- vapply(names(lock), function(p) tryCatch(packageVersion(p) == package_version(lock[[p]]$Version),
                                                   error = function(e) FALSE), logical(1))
  bad  <- names(lock)[!same]
  if (length(bad)) stop("not installed at the lockfile version: ", paste(bad, collapse = ", "))
  cat(length(lock), "packages installed at the lockfile versions\n")
  for (p in c("did", "fect", "didimputation", "DIDmultiplegtDYN", "polars", "synthdid", "PanelMatch", "fixest", "HonestDiD"))
    cat(sprintf("%-18s %s\n", p, as.character(packageVersion(p))))
  library(polars)                                      # DIDmultiplegtDYN needs it attached
'
echo "setup done"
