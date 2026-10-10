### New DiD analysis: validate the environment before estimating
# Runs the shared checks (wave renumbering, Callaway-Sant'Anna balanced-panel workaround) and
# then every estimator script 02-08 in check-only mode (DID_CHECK_ONLY = 1): each script runs its
# own estimation function on simulated data with known effects (checks.R) and stops if the
# estimates miss the truth. Run once per machine, e.g. on Della after renv::restore(), and after
# any package update. Needs no data. Writes results/tables/checks.txt; stops if any check fails.

rm(list = ls())
here::i_am("code/analyze_did/00_checks.R")
source(here::here("code", "analyze_did", "00_setup.R"))
source(here("code", "analyze_did", "checks.R"))

res <- list(check_prep_outcome(strict = FALSE), check_cs_balancing(strict = FALSE))

scripts <- c("02_cs.R", "03_fect.R", "04_imputation.R", "05_dcdh.R", "06_sunab_twfe.R", "07_synthdid.R", "08_panelmatch.R")
for (s in scripts) {
  message(s)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(here("code", "analyze_did", s)),
                                  stdout = TRUE, stderr = TRUE, env = "DID_CHECK_ONLY=1"))
  lines <- grep("\\[(PASS|FAIL)\\]", out, value = TRUE)
  status <- attr(out, "status")
  if (length(lines)) message(paste(lines, collapse = "\n"))
  res[[s]] <- if (!is.null(status) && status != 0 || !length(lines))
    data.table(check = s, pass = FALSE, detail = paste(tail(out, 5), collapse = " | ")) else
    data.table(check = sub(".*\\] ([^:]+):.*", "\\1", lines), pass = grepl("\\[PASS\\]", lines),
               detail = sub(".*\\] [^:]+: ", "", lines))
}
res <- rbindlist(res)

dir.create(here("results", "tables"), recursive = TRUE, showWarnings = FALSE)
fwrite(res, here("results", "tables", "checks.txt"), sep = "\t")
pkgs <- c("did", "DRDID", "fect", "didimputation", "DIDmultiplegtDYN", "polars", "synthdid", "PanelMatch", "fixest", "HonestDiD")
message("Package versions: ", paste(pkgs, vapply(pkgs, function(p) as.character(packageVersion(p)), ""), collapse = ", "))
if (!all(res$pass)) stop("Some checks failed: ", paste(res[pass == FALSE, check], collapse = ", "))
message("All checks passed.")

### END
