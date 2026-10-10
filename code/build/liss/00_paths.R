### LISS build: paths
# Sourced at the top of every build script.
#
# The raw LISS files are not in the repo (LISS data licence). They live in one
# folder with a subfolder per module (politics, background, income, assets,
# housing, personality, social_integration_leisure). Point LISS_RAW at that
# folder, e.g. in a .Renviron file in the repo root (ignored by git):
#   LISS_RAW="~/Princeton Dropbox/Vincent Heddesheimer/Data/Panel_Surveys/LISS_to26_max"
# Without LISS_RAW the build looks in data/raw/liss/ (also ignored by git).

liss_raw <- path.expand(Sys.getenv("LISS_RAW", unset = here::here("data", "raw", "liss")))
if (!dir.exists(liss_raw)) stop("Raw LISS folder not found: ", liss_raw, ". Set LISS_RAW (see code/build/liss/00_paths.R).")

# cleaned module files contain microdata, so they stay next to the raw files
liss_clean <- file.path(liss_raw, "cleaned")
dir.create(liss_clean, showWarnings = FALSE)
