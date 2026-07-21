#' @export
get_metadata <- function(file) {
  meta <- nanoparquet::read_parquet_metadata(file)

  meta <- meta$file_meta_data$key_value_metadata[[1]]

  meta[meta['key'] != "ARROW:schema", ]
}

#' @export
completeness_filter <- function(df) {
  stopifnot(
    c("data_period", "dat", "dat.value", "region") %in% colnames(df)
  )
  dplyr::left_join(
    df,
    nanoparquet::read_parquet(
      "./data/tpm_classification_completeness.parquet"
    ) |>
      dplyr::select(data_period, dat, dat.value, region, meets_80pct),
    by = dplyr::join_by(data_period, dat, dat.value, region)
  ) |>
    dplyr::filter(meets_80pct == TRUE) |>
    dplyr::select(-meets_80pct)
}
