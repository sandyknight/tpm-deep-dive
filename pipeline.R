# Cross-platform pipeline runner: the equivalent of `make all` for machines
# without make or uv (e.g. Windows). Requires only R and a Python (>= 3.12,
# with polars installed) findable via .venv, PATH, or the PIPELINE_PYTHON
# environment variable.
#
# Usage, from the project root:  Rscript pipeline.R [--force]
#
# Stages are skipped when their outputs are newer than all of their inputs,
# mirroring the Makefile; --force reruns everything.

# Flat "key = \"value\"" extraction, not a full TOML parser (see config.toml)
RAW <- sub(
  '^raw_data\\s*=\\s*"([^"]*)".*$',
  "\\1",
  grep("^raw_data\\s*=", readLines("config.toml"), value = TRUE)[[1]]
)
TEMPLATE <- "templates/ohid_theme.pptx"
DATA <- c(
  "data/tpm_Basic_dataset.parquet",
  "data/tpm_classification_completeness.parquet",
  "data/tpm_AccmneedStart.parquet",
  "data/tpm_RefSrcGrp.parquet",
  "data/tpm_PrevJourneys.parquet"
)
FITS <- "fit_summaries/tpm_odds_ratios.parquet"
SLIDES <- "slides/tpm_odds_ratio_slides.pptx"

if (!file.exists("pipeline.R") || !dir.exists("R")) {
  stop("Run this from the project root: Rscript pipeline.R")
}

find_python <- function() {
  override <- Sys.getenv("PIPELINE_PYTHON", "")
  if (nzchar(override)) {
    return(override)
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
      "No Python found. Create a .venv, put python on PATH, ",
      "or set PIPELINE_PYTHON."
    )
  }
  path[[1]]
}

stale <- function(targets, deps) {
  missing <- deps[!file.exists(deps)]
  if (length(missing) > 0) {
    stop("Missing pipeline input(s): ", paste(missing, collapse = ", "))
  }
  !all(file.exists(targets)) ||
    max(file.mtime(deps)) > min(file.mtime(targets))
}

run_stage <- function(label, cmd, args) {
  message("== ", label, ": ", cmd, " ", paste(args, collapse = " "))
  status <- system2(cmd, args)
  if (status != 0) {
    stop(label, " failed with exit status ", status)
  }
}

force <- "--force" %in% commandArgs(trailingOnly = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")

if (force || stale(DATA, c("main.py", "src/lib.py", "config.toml", RAW))) {
  run_stage("data", find_python(), "main.py")
} else {
  message("== data: up to date")
}

if (force || stale(FITS, c("R/fit_models.R", "R/mod/models.R", "R/mod/lib.R", DATA))) {
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
