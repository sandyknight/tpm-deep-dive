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

run_analysis <- function(data, name, spec, period_label) {
  res <- models$fit_exposure_glm(
    data,
    exposure = spec$exposure,
    adjust = spec$adjust,
    geography = spec$geography
  )
  message(sprintf(
    "%-16s %-18s geography=%-8s%s",
    name,
    period_label,
    res$geography,
    if (isTRUE(res$la_separation)) "  [LA separation -> region]" else ""
  ))
  models$tidy_ors(res$fit, spec$exposure) |>
    dplyr::mutate(
      analysis = name,
      data_period = period_label,
      geography = res$geography,
      la_separation = res$la_separation,
      .before = 1
    )
}

annual_fits <- purrr::map(annual_periods, function(period) {
  data <- df |>
    dplyr::filter(data_period == period) |>
    droplevels()
  purrr::imap(ANALYSES, \(spec, name) run_analysis(data, name, spec, period))
})

# Pooled post-break fit: the annual -03-31 periods are disjoint, so pooling
# with a period covariate is legitimate (unlike the rolling periods).
pooled_fits <- purrr::imap(ANALYSES, function(spec, name) {
  spec$adjust <- c(spec$adjust, "data_period")
  data <- df |>
    dplyr::filter(data_period %in% POOLED_PERIODS) |>
    droplevels()
  run_analysis(
    data,
    name,
    spec,
    paste(range(POOLED_PERIODS), collapse = " to ")
  )
})

results <- purrr::list_rbind(c(
  purrr::flatten(annual_fits),
  unname(pooled_fits)
))

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
