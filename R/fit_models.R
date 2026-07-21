box::use(. / mod / lib)

BASIC_DATA <- "./data/tpm_Basic_dataset.parquet"
AGE <- "age_jy.output.grp_value"
SUBSTANCE <- "drug.grp_ocu_value"

# 2023-24 methodology break: per docs/treatment_progress_measure.md, periods
# earlier than FY2023-24 are "not fully comparable with figures from 2023-24
# onwards", so 2024-03-31 is the first new-methodology period and this pooled
# window sits entirely on the new methodology.
POOLED_PERIODS <- c("2024-03-31", "2025-03-31", "2026-03-31")

# One model per exposure, each with its own adjustment set (Table 2 fallacy)
# and its own source dataset. Within a single period the period term drops
# out; the pooled fit adds it. Not modelled, per the analysis plan: TxLen
# (reverse causation), the "Latest" variables (outcome-contemporaneous), and
# EarlyExitRsn (part of the outcome definition).
#
# `ref` is the OR baseline — presentation choice only; substance, referral
# and previous-journeys baselines match the treatment-retention iteration
# of this work rather than any convention. `exclude` drops missing-data
# levels of the exposure. In the per-variable datasets the exposure lives
# in a `level` column, renamed to `exposure` on load.
ANALYSES <- list(
  age = list(
    file = BASIC_DATA,
    exposure = AGE,
    ref = "18-24",
    adjust = character(),
    geography = NULL
  ),
  substance_group = list(
    file = BASIC_DATA,
    exposure = SUBSTANCE,
    ref = "Alcohol only",
    adjust = AGE,
    geography = "la"
  ),
  housing_start = list(
    file = "./data/tpm_AccmneedStart.parquet",
    exposure = "AccmneedStart",
    ref = "No housing problem",
    exclude = "Not stated / Missing",
    adjust = c(AGE, SUBSTANCE),
    geography = "la"
  ),
  referral_source = list(
    file = "./data/tpm_RefSrcGrp.parquet",
    exposure = "RefSrcGrp",
    ref = "Self, family & friends",
    exclude = "Inconsistent or missing",
    adjust = c(AGE, SUBSTANCE),
    geography = "la"
  ),
  previous_journeys = list(
    file = "./data/tpm_PrevJourneys.parquet",
    exposure = "PrevJourneys",
    ref = "2-3 prev. jys.",
    adjust = c(AGE, SUBSTANCE),
    geography = "la"
  ),
  # Case-mix standardisation: region is the exposure, so no LA term.
  region = list(
    file = BASIC_DATA,
    exposure = "region",
    ref = "London",
    adjust = c(AGE, SUBSTANCE),
    geography = NULL
  )
)

prep_dataset <- function(spec) {
  d <- lib$completeness_filter(nanoparquet::read_parquet(spec$file)) |>
    dplyr::mutate(
      tpm_value = dplyr::recode_values(
        tpm_value,
        from = c(
          "Showing substantial progress",
          "Not yet showing substantial progress",
          "Missing data",
          "New client"
        ),
        to = c(1L, 0L, NA_integer_, NA_integer_)
      )
    ) |>
    dplyr::filter(!is.na(tpm_value))
  if (!spec$exposure %in% colnames(d)) {
    d <- dplyr::rename(d, "{spec$exposure}" := level)
  }
  if (!is.null(spec$exclude)) {
    d <- dplyr::filter(d, !.data[[spec$exposure]] %in% spec$exclude)
  }
  stopifnot(spec$ref %in% d[[spec$exposure]])
  d[[spec$exposure]] <- stats::relevel(
    factor(d[[spec$exposure]]),
    ref = spec$ref
  )
  d
}

datasets <- purrr::map(ANALYSES, prep_dataset)

# Annual, non-overlapping periods only: rolling month-end periods (e.g.
# 2026-04-30) share 11 months with their neighbours and are excluded.
annual_periods <- datasets[[1]] |>
  dplyr::distinct(data_period) |>
  dplyr::filter(endsWith(data_period, "-03-31")) |>
  dplyr::pull(data_period) |>
  sort()

# Runs one fit on a mirai daemon. Daemons are fresh processes (which is why
# this parallelises on Windows too, unlike forking), so the function must be
# self-contained: the box module is loaded inside the worker via box.path,
# and each job carries its own already-prepared data.
fit_job <- purrr::in_parallel(
  \(job) {
    Sys.setenv(MKL_NUM_THREADS = "2") # workers mustn't oversubscribe BLAS
    options(box.path = box_path)
    box::use(mod / models)
    res <- models$fit_exposure_glm(
      job$data,
      exposure = job$spec$exposure,
      adjust = job$spec$adjust,
      geography = job$spec$geography
    )
    models$tidy_ors(res$fit, job$spec$exposure) |>
      dplyr::mutate(
        analysis = job$name,
        data_period = job$label,
        geography = res$geography,
        la_separation = res$la_separation,
        .before = 1
      )
  },
  box_path = file.path(getwd(), "R")
)

make_job <- function(name, spec, periods, label) {
  data <- datasets[[name]] |>
    dplyr::filter(data_period %in% periods) |>
    droplevels()
  list(name = name, spec = spec, data = data, label = label)
}

# Every fit (period x analysis, plus the pooled ones) is independent, so
# run them all as one flat job list. The pooled post-break fits are
# legitimate pooling — the annual -03-31 periods are disjoint (unlike the
# rolling periods) — and they are the slowest single fits, so they must
# start first, not run serially after the annual batch.
annual_jobs <- purrr::flatten(purrr::map(annual_periods, function(period) {
  purrr::imap(ANALYSES, \(spec, name) make_job(name, spec, period, period))
}))

pooled_jobs <- purrr::imap(ANALYSES, function(spec, name) {
  spec$adjust <- c(spec$adjust, "data_period")
  make_job(
    name,
    spec,
    POOLED_PERIODS,
    paste(range(POOLED_PERIODS), collapse = " to ")
  )
})

jobs <- c(unname(pooled_jobs), annual_jobs)

mirai::daemons(min(length(jobs), parallel::detectCores()))
fits <- purrr::map(jobs, fit_job)
mirai::daemons(0)

results <- purrr::list_rbind(fits)

# Fit log, printed post hoc: worker messages don't surface from daemons
results |>
  dplyr::distinct(analysis, data_period, geography, la_separation) |>
  purrr::pwalk(\(analysis, data_period, geography, la_separation) {
    message(sprintf(
      "%-18s %-24s geography=%-8s%s",
      analysis,
      data_period,
      geography,
      if (isTRUE(la_separation)) "  [LA separation -> region]" else ""
    ))
  })

dir.create("./fit_summaries", showWarnings = FALSE)
nanoparquet::write_parquet(
  results,
  "./fit_summaries/tpm_odds_ratios.parquet",
  metadata = c(
    outcome = "TPM binary: Showing=1, Not yet=0; Missing data and New client excluded",
    filter = "80% classification-completeness filter applied (LA x period)",
    conf_int = "Profile likelihood, 95%, exposure terms only",
    adjustment_sets = paste(
      "age: unadjusted within period;",
      "substance_group: age + LA;",
      "housing_start / referral_source / previous_journeys: age + substance + LA;",
      "region: age + substance (case-mix standardisation);",
      "LA models fall back to region on separation; pooled fits add data_period"
    ),
    reference_levels = paste(
      purrr::imap_chr(ANALYSES, \(spec, name) paste0(name, "=", spec$ref)),
      collapse = "; "
    ),
    source_datasets = paste(
      unique(purrr::map_chr(ANALYSES, "file")),
      collapse = ", "
    )
  )
)

print(results, n = 30)
