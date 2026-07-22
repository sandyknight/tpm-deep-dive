# TPM odds-ratio analysis

Logistic-regression odds ratios for the NDTMS Treatment Progress Measure:
Python pre-processes the raw extract into parquet datasets, R fits the GLMs
and builds the PowerPoint deck. Parquet files in `data/` are the contract
between the two stages.

## Layout

- `main.py`, `src/` — pre-processing (raw CSV → `data/*.parquet`)
- `R/fit_models.R`, `R/models.R` — weighted binomial GLMs → `fit_summaries/`
- `R/make_slides.R` — OR slide deck → `slides/`
- `Makefile` / `pipeline.R` — orchestrators (see below)

Machine-specific paths live in `config.toml`, which is not committed:
copy `config.example.toml` to `config.toml` and uncomment/edit the lines
for your machine (raw-data location, optionally a Python interpreter).
The raw extract itself lives outside the repo and is never committed. The
slide theme is self-contained: `templates/ohid_theme.pptx` is a blank deck
carrying the OHID master/layouts.

## Prerequisites

- **`config.toml`**: `cp config.example.toml config.toml`, then edit.
- **Python ≥ 3.12** with polars. With uv this is automatic; without it:
  `python -m venv .venv`, activate, `pip install -r requirements.txt`.
- **R ≥ 4.5.1**: packages are pinned with renv — from an R session in the
  project root, `renv::restore()` installs everything at the locked
  versions (renv activates automatically via the project `.Rprofile`).
  Every locked package declares an R floor of 4.1 or lower, and the
  lockfile's recorded R version is pinned to 4.5.1 via
  `renv::settings$r.version`, so restores on 4.5.1 won't warn.

## Running

Linux/macOS with make (uses uv by default; `make PYTHON=python` without it):

```sh
make            # everything that is out of date
make fits       # just up to the GLMs
```

Anywhere, including Windows, no make or uv required:

```sh
Rscript pipeline.R           # same stage-skipping as make
Rscript pipeline.R --force   # rerun everything
```

`pipeline.R` finds Python via the `python` key in `config.toml` (the
place to point at e.g. a conda env's `python.exe`), then `.venv`, then
`PATH`. The `PIPELINE_PYTHON` environment variable also works for one-off
shell overrides — but not from `~/.Renviron`, which R skips because the
project ships its own `.Renviron`.

## Notes

- The lockfile resolves packages from a dated Posit Package Manager
  snapshot (see `renv.lock`), which — unlike CRAN — serves Windows
  binaries for historical package versions, for current and old-release
  R. `renv::restore()` on Windows should therefore never need Rtools.
  If a source build is ever attempted anyway on a locked-down machine,
  the usual culprit is TEMP pointing at a no-execute/network path — set
  `TMPDIR` to a local directory (e.g. `C:\Temp`) in `.Renviron`.
- Running R here (4.6.1) with the lockfile pinned to 4.5.1 makes
  `renv::status()` print a version note; it's informational only.

- `.Renviron` sets `MKL_THREADING_LAYER=GNU`: on Linux machines where R is
  linked against Intel MKL, multithreaded BLAS otherwise returns silently
  wrong GLM coefficients. Harmless elsewhere — do not remove.
- Nothing data-derived (`data/`, `fit_summaries/`, `slides/`) is ever
  committed; the source extract is OFFICIAL_SENSITIVE.
