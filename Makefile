# TPM pipeline: Python pre-processing -> R GLMs -> PowerPoint.
# Parquet files in data/ are the contract between the Python and R stages.
# The OFFICIAL_SENSITIVE raw extract lives outside the repo and is never
# copied into it.

# Raw-data location comes from config.toml (flat key = "value" extraction)
RAW := $(shell sed -n 's/^raw_data *= *"\(.*\)".*/\1/p' config.toml)
TEMPLATE := templates/ohid_theme.pptx

# Override on machines without uv: make PYTHON=python
# (No make at all, e.g. Windows? Use `Rscript pipeline.R` instead.)
PYTHON ?= uv run python

# Only the parquets consumed downstream are tracked as targets; the same
# recipe also writes the 12 per-variable parquets (future GLMs will add
# their inputs here as they start being consumed).
DATA := data/tpm_Basic_dataset.parquet \
        data/tpm_classification_completeness.parquet \
        data/tpm_AccmneedStart.parquet \
        data/tpm_RefSrcGrp.parquet \
        data/tpm_PrevJourneys.parquet
FITS := fit_summaries/tpm_odds_ratios.parquet
SLIDES := slides/tpm_odds_ratio_slides.pptx

all: $(SLIDES)

# Grouped target (&:): one streaming pass over the raw CSV produces all the
# parquets together, so the recipe runs once, not once per file.
$(DATA) &: main.py src/lib.py config.toml $(RAW)
	$(PYTHON) main.py

$(FITS): R/fit_models.R R/mod/models.R R/mod/lib.R $(DATA)
	Rscript R/fit_models.R

$(SLIDES): R/make_slides.R $(FITS) $(TEMPLATE)
	Rscript R/make_slides.R

data: $(DATA)
fits: $(FITS)
slides: $(SLIDES)

# Derived model/slide outputs only; the data/ parquets take ~15s to rebuild
# with `make data` if you want a truly clean run.
clean:
	rm -f $(FITS) $(SLIDES)

.PHONY: all data fits slides clean
