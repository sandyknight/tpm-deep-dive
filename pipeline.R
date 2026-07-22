# pipeline.R — cross-platform pipeline runner (the `make all` equivalent for
# machines without make or uv, i.e. the Windows estate).
#
# Usage, always from the project root:
#
#   Rscript pipeline.R            # run whatever is out of date
#   Rscript pipeline.R --force    # rerun every stage regardless
#
# The pipeline is three stages, each a separate process:
#
#   [1] data    python main.py          raw CSV -> data/*.parquet
#   [2] fits    Rscript R/fit_models.R  data parquets -> fit_summaries/*.parquet
#   [3] slides  Rscript R/make_slides.R fits + templates/ohid_theme.pptx
#                                         -> slides/tpm_odds_ratio_slides.pptx
#
# A stage is skipped when all of its outputs are newer than all of its
# inputs (plain file-mtime comparison, same idea as make). Editing a script,
# the config, or the raw data therefore reruns the right stages on the next
# invocation.
#
# What this script assumes:
#   - Working directory is the project root (it stops immediately if not).
#   - config.toml's raw_data points at the raw NDTMS extract.
#   - R packages restored: renv::restore() has been run (renv activates
#     itself through the project .Rprofile whenever R starts in this
#     directory — including the Rscript children this script launches).
#   - A Python >= 3.12 with polars (see "How Python is found" below).
#
# ---------------------------------------------------------------------------
# DEBUGGING GUIDE (written for the locked-down Windows machines)
#
# First move: run the failing stage by hand from the project root —
#
#   <python> main.py
#   Rscript R/fit_models.R
#   Rscript R/make_slides.R
#
# — because this script adds nothing to a stage except deciding whether to
# run it; a stage that fails here fails identically when run by hand, and
# by hand you see the full traceback.
#
# Failures BEFORE any stage runs:
#   "Run this from the project root"  -> cd to the repo first; relative
#       paths everywhere in the pipeline assume it.
#   "Missing pipeline input(s): ..."  -> an *input* file doesn't exist.
#       If it names the raw CSV, config.toml's raw_data is wrong for this
#       machine (note it is relative to the project root, and forward
#       slashes are fine on Windows). If it names a script or the pptx
#       template, the checkout is incomplete.
#
# Stage [1] failures:
#   "No Python found..." / "Configured python does not exist..." -> see
#       "How Python is found" below; the intended fix is setting `python`
#       in config.toml to the full path of a python.exe that has polars
#       installed (conda env paths are fine). Don't use ~/.Renviron for
#       this — the project ships its own .Renviron, and R only ever loads
#       one of the two, so user-level entries are silently ignored.
#   ModuleNotFoundError: polars / tomllib -> wrong interpreter (tomllib
#       means Python < 3.11): pip install -r requirements.txt into the
#       interpreter being used, or point PIPELINE_PYTHON at the right one.
#   Memory/speed: this stage streams a ~4 GB CSV; a few minutes on a
#       laptop is normal, an instant crash is not.
#
# Stage [2]/[3] failures:
#   "there is no package called ..." -> renv library not restored (run
#       renv::restore() in an R session here), or R was started somewhere
#       that skipped .Rprofile so the system library is being used.
#   "cannot open file 'R/models.R'" or similar -> working directory isn't
#       the project root; both R stages resolve every path relative to it,
#       so run them exactly as shown above.
#   Stage [2] parallelises via mirai daemons (separate Rscript processes,
#       works on Windows). If a daemon dies silently, rerun after
#       mirai::daemons(0) in a fresh session, or check a corporate
#       process-spawn/AV policy isn't killing child Rscript processes.
#   renv::status() printing an R-version note (lockfile 4.5.1 vs a newer
#       local R) is informational, never the cause of a failure.
#
# Skipping behaves wrongly? Timestamps are the only mechanism. Check with
#   file.mtime() that outputs really are newer than inputs; cloud-synced
#   folders (OneDrive) are known to produce misleading mtimes. --force
#   sidesteps the question entirely.
# ---------------------------------------------------------------------------

# --- Configuration ---------------------------------------------------------

# Values come from config.toml via flat 'key = "value"' text extraction,
# not a TOML parser (deliberate: no extra R dependency), so lines in
# config.toml must stay in exactly that shape.
config_value <- function(key) {
  line <- grep(
    sprintf('^%s\\s*=\\s*"', key),
    readLines("config.toml"),
    value = TRUE
  )
  if (length(line) == 0) {
    return("")
  }
  sub('^[a-z_]+\\s*=\\s*"([^"]*)".*$', "\\1", line[[1]])
}

RAW <- config_value("raw_data")
TEMPLATE <- "templates/ohid_theme.pptx"

# Stage outputs. DATA lists only the parquets consumed downstream; stage
# [1] writes the per-variable parquets alongside them in the same pass.
DATA <- c(
  "data/tpm_Basic_dataset.parquet",
  "data/tpm_classification_completeness.parquet",
  "data/tpm_AccmneedStart.parquet",
  "data/tpm_RefSrcGrp.parquet",
  "data/tpm_PrevJourneys.parquet"
)
FITS <- "fit_summaries/tpm_odds_ratios.parquet"
SLIDES <- "slides/tpm_odds_ratio_slides.pptx"

# Refuse to run from anywhere but the project root: every path above is
# root-relative, and the R stages assume it too.
if (!file.exists("pipeline.R") || !dir.exists("R")) {
  stop("Run this from the project root: Rscript pipeline.R")
}

# --- How Python is found ---------------------------------------------------
# In order:
#   1. The `python` key in config.toml, if non-empty — the intended place
#      for a machine-specific interpreter (e.g. a conda env's python.exe;
#      forward slashes work on Windows).
#   2. PIPELINE_PYTHON environment variable — same effect, for one-off
#      overrides at the shell. NOTE the .Renviron trap: R loads exactly ONE
#      .Renviron — the project's (which this repo ships, for the MKL fix)
#      if it exists, else the user's — so a PIPELINE_PYTHON added to
#      ~/.Renviron is silently ignored here. Prefer the config.toml key.
#   3. The project virtualenv: .venv/Scripts/python.exe (Windows) or
#      .venv/bin/python (elsewhere).
#   4. First of python / python3 on PATH. Note the Microsoft Store
#      "python" shim on Windows can be found here yet do nothing useful —
#      that presents as stage [1] "failing" with no output; use the
#      config.toml key to bypass it.
#
# An explicit interpreter (1 or 2) that doesn't exist on disk is an error
# up front, with the bad path printed — not a cryptic mid-stage failure.
#
# Conda note: pointing straight at a conda env's python.exe (without
# "conda activate") is usually fine for polars; if imports ever fail with
# "DLL load failed", run stage [1] from an activated Anaconda prompt
# instead: `python main.py`, then rerun this script for stages [2]-[3].
find_python <- function() {
  for (override in c(config_value("python"), Sys.getenv("PIPELINE_PYTHON"))) {
    if (nzchar(override)) {
      if (!file.exists(override)) {
        stop(
          "Configured python does not exist (or is not readable): ",
          override
        )
      }
      return(override)
    }
  }
  venv <- if (.Platform$OS.type == "windows") {
    ".venv/Scripts/python.exe"
  } else {
    ".venv/bin/python"
  }
  if (file.exists(venv)) {
    return(venv)
  }
  path <- Sys.which(c("python", "python3"))
  path <- path[nzchar(path)]
  if (length(path) == 0) {
    stop(
      "No Python found. Set `python` in config.toml, create a .venv, ",
      "or put python on PATH."
    )
  }
  path[[1]]
}

# --- Stage machinery -------------------------------------------------------

# TRUE if the stage must run: an output is missing, or the newest input is
# newer than the oldest output. Missing *inputs* are an error, not a
# trigger: a stage can never succeed without them, so fail loudly here
# with the offending path rather than obscurely mid-stage.
stale <- function(targets, deps) {
  missing <- deps[!file.exists(deps)]
  if (length(missing) > 0) {
    stop("Missing pipeline input(s): ", paste(missing, collapse = ", "))
  }
  !all(file.exists(targets)) ||
    max(file.mtime(deps)) > min(file.mtime(targets))
}

# Announce and run one stage; abort the pipeline on non-zero exit so later
# stages never consume half-written outputs. The stage's own stdout/stderr
# pass straight through, so its error messages appear above the abort.
run_stage <- function(label, cmd, args) {
  message("== ", label, ": ", cmd, " ", paste(args, collapse = " "))
  status <- system2(cmd, args)
  if (status != 0) {
    stop(label, " failed with exit status ", status)
  }
}

force <- "--force" %in% commandArgs(trailingOnly = TRUE)

# Children are launched from this R's own installation, not from PATH —
# on Windows Rscript is typically *not* on PATH, and this also guarantees
# the stages run under the same R version as the runner.
rscript <- file.path(R.home("bin"), "Rscript")

# --- The three stages ------------------------------------------------------
# Each stage's dependency list includes the scripts that produce it, so
# editing one triggers the right rebuilds. config.toml is a data-stage
# input because raw_data lives in it.

if (force || stale(DATA, c("main.py", "src/lib.py", "config.toml", RAW))) {
  run_stage("data", find_python(), "main.py")
} else {
  message("== data: up to date")
}

if (
  force ||
    stale(FITS, c("R/fit_models.R", "R/models.R", DATA))
) {
  run_stage("fits", rscript, "R/fit_models.R")
} else {
  message("== fits: up to date")
}

if (force || stale(SLIDES, c("R/make_slides.R", FITS, TEMPLATE))) {
  run_stage("slides", rscript, "R/make_slides.R")
} else {
  message("== slides: up to date")
}

message("Pipeline complete.")
