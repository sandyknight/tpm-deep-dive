#' Fit one weighted logistic regression for a single exposure and return the
#' fit plus what happened geographically.
#'
#' geography = NULL fits at national level (no geography term). geography =
#' "la" adjusts for LA (`dat.value`) but falls back to `region` if the LA fit
#' shows separation ("fitted probabilities numerically 0 or 1" warning) —
#' sparse LA x covariate cells make this likely. The region fallback swaps the
#' term rather than aggregating rows: with aggregated count data the weighted
#' GLM is identical either way.
#'
#' @export
fit_exposure_glm <- function(
  data,
  exposure,
  adjust = character(),
  geography = NULL
) {
  if (is.null(geography)) {
    res <- fit_once(data, c(exposure, adjust))
    res$geography <- "national"
    res$la_separation <- NA
    return(res)
  }
  stopifnot(identical(geography, "la"))
  res <- fit_once(data, c(exposure, adjust, "dat.value"))
  if (res$separation) {
    res <- fit_once(data, c(exposure, adjust, "region"))
    res$geography <- "region"
    res$la_separation <- TRUE
  } else {
    res$geography <- "la"
    res$la_separation <- FALSE
  }
  res
}

# Collapses to one row per covariate cell (successes/failures counts) and
# fits in binomial cbind form rather than weighted-Bernoulli form: the
# likelihood is identical up to a constant, so estimates and profile CIs
# match exactly, but IRLS and every profile refit see half the rows. The
# deviance also stops carrying each client's irreducible Bernoulli entropy,
# so the default convergence tolerance is meaningful again (the previous
# form needed epsilon = 1e-12 or profile.glm would find a better optimum).
fit_once <- function(data, rhs) {
  cells <- data |>
    dplyr::summarise(
      n_showing = sum(value[tpm_value == 1L]),
      n_not_yet = sum(value[tpm_value == 0L]),
      .by = dplyr::all_of(rhs)
    )
  separation <- FALSE
  fit <- withCallingHandlers(
    stats::glm(
      stats::reformulate(rhs, response = quote(cbind(n_showing, n_not_yet))),
      data = cells,
      family = stats::binomial(link = "logit")
    ),
    warning = function(w) {
      if (
        grepl("fitted probabilities numerically 0 or 1", conditionMessage(w))
      ) {
        separation <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  list(fit = fit, separation = separation)
}

#' Odds ratios with profile-likelihood CIs for the exposure terms only.
#'
#' Profiling is restricted to the exposure coefficients so the LA-adjusted
#' fits stay fast (broom::tidy(conf.int = TRUE) would profile every LA dummy
#' too); the intervals are identical to what broom returns. The reference
#' level is included as a row with OR = 1 and no CI, for plotting.
#'
#' @export
tidy_ors <- function(fit, exposure) {
  est <- broom::tidy(fit, exponentiate = TRUE) |>
    dplyr::filter(startsWith(term, exposure))
  ci <- suppressMessages(stats::confint(fit, parm = est$term))
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1, dimnames = list(est$term, names(ci)))
  }
  ref <- fit$xlevels[[exposure]][1]
  dplyr::bind_rows(
    tibble::tibble(
      term = paste0(exposure, ref),
      estimate = 1,
      reference = TRUE
    ),
    est |>
      dplyr::mutate(
        conf.low = exp(ci[, 1]),
        conf.high = exp(ci[, 2]),
        reference = FALSE
      ) |>
      dplyr::select(term, estimate, conf.low, conf.high, p.value, reference)
  ) |>
    # Two mutate calls: a single call would create the `exposure` column
    # first and `sub()` would then see the column, not the function argument.
    dplyr::mutate(
      level = sub(exposure, "", term, fixed = TRUE),
      .before = 1
    ) |>
    dplyr::mutate(
      exposure = exposure,
      n_clients = sum(fit$prior.weights),
      .before = 1
    )
}
