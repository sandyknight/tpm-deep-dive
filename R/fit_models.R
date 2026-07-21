box::use(. / mod / lib)
box::use(. / mod / models)

BASIC_DATA <- "./data/tpm_Basic_dataset.parquet"

# Reference levels for the ORs. Presentation choice only — flip here if the
# slides read better against a different baseline.
REF_AGE <- "18-24"
# Alcohol only rather than the conventional opioids baseline, for consistency
# with the previous treatment-retention iteration of this work.
REF_SUBSTANCE <- "Alcohol only"

# 2023-24 methodology break: per docs/treatment_progress_measure.md, periods
# earlier than FY2023-24 are "not fully comparable with figures from 2023-24
# onwards", so 2024-03-31 is the first new-methodology period and this pooled
# window sits entirely on the new methodology.
POOLED_PERIODS <- c("2024-03-31", "2025-03-31", "2026-03-31")

# One model per exposure, each with its own adjustment set (Table 2 fallacy).
# Within a single period the period term drops out; the pooled fit adds it.
ANALYSES <- list(
  age = list(
    exposure = "age_jy.output.grp_value",
    adjust = character(),
    geography = NULL
  ),
  substance_group = list(
    exposure = "drug.grp_ocu_value",
    adjust = "age_jy.output.grp_value",
    geography = "la"
  )
)

df <- lib$completeness_filter(nanoparquet::read_parquet(BASIC_DATA)) |>
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
  dplyr::filter(!is.na(tpm_value)) |>
  dplyr::mutate(
    age_jy.output.grp_value = stats::relevel(
      factor(age_jy.output.grp_value),
      ref = REF_AGE
    ),
    drug.grp_ocu_value = stats::relevel(
      factor(drug.grp_ocu_value),
      ref = REF_SUBSTANCE
    )
  )

# Annual, non-overlapping periods only: rolling month-end periods (e.g.
# 2026-04-30) share 11 months with their neighbours and are excluded.
annual_periods <- df |>
  dplyr::distinct(data_period) |>
  dplyr::filter(endsWith(data_period, "-03-31")) |>
  dplyr::pull(data_period) |>
  sort()

# Runs one fit on a mirai daemon. Daemons are fresh processes (which is why
# this parallelises on Windows too, unlike forking), so the function must be
# self-contained: the box module is loaded inside the worker via box.path,
# and `df` is shipped to it explicitly.
fit_job <- purrr::in_parallel(
  \(job) {
    Sys.setenv(MKL_NUM_THREADS = "2") # workers mustn't oversubscribe BLAS
    options(box.path = box_path)
    box::use(mod / models)
    data <- droplevels(dplyr::filter(df, data_period %in% job$periods))
    res <- models$fit_exposure_glm(
      data,
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
  df = df,
  box_path = file.path(getwd(), "R")
)

# Every fit (period x analysis, plus the pooled ones) is independent, so
# run them all as one flat job list. The pooled post-break fits are
# legitimate pooling — the annual -03-31 periods are disjoint (unlike the
# rolling periods) — and they are the slowest single fits, so they must
# start first, not run serially after the annual batch.
annual_jobs <- purrr::flatten(purrr::map(annual_periods, function(period) {
  purrr::imap(ANALYSES, \(spec, name) {
    list(name = name, spec = spec, periods = period, label = period)
  })
}))

pooled_jobs <- purrr::imap(ANALYSES, function(spec, name) {
  spec$adjust <- c(spec$adjust, "data_period")
  list(
    name = name,
    spec = spec,
    periods = POOLED_PERIODS,
    label = paste(range(POOLED_PERIODS), collapse = " to ")
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
      "%-16s %-18s geography=%-8s%s",
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
      "substance_group: age + LA (region on separation);",
      "pooled fits add data_period"
    ),
    reference_levels = sprintf("age=%s, substance=%s", REF_AGE, REF_SUBSTANCE),
    source_dataset = BASIC_DATA
  )
)

print(results, n = 30)
