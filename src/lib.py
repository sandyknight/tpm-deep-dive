import polars as pl

DEMOGRAPHIC_COLS = [
    "data_period",
    "dat",
    "dat.value",
    "region",
    "age_jy.output.grp_value",
    "drug.grp_ocu_value",
    "tpm_value",
]

# tpm_detailed_value is kept in these datasets so the sub-category
# "Completed treatment but with an acute housing problem" can be excluded
# or reclassified in sensitivity analyses (housing is partly part of the
# outcome definition).
KEEP_TPM_DETAILED = {"AccmneedStart", "HousingLatestGrp"}

"""
Basic dataset - no variables/levels; aggregated to tpm_value +
demographics and substance group.
"""


def make_basic_dataset(file_path):
    (
        pl.scan_csv(file_path)
        .filter(pl.col("variable").eq("InTx"))
        .group_by(DEMOGRAPHIC_COLS)
        .agg(pl.col("value").sum())
        .collect()
        .write_parquet(
            "./data/tpm_Basic_dataset.parquet",
            metadata={
                "Removed in aggregation": "tpm_detailed_value, tpm_not_using_or_reduced_value",
                "levels": "InTx",
                "variables": "InTx",
                "original_dataset": "OFFICIAL_SENSITIVE_TPM_data_2026-06_v7.csv",
            },
        )
    )


"""
Per-variable datasets - one parquet per NDTMS variable, with counts by
demographics x tpm_value x level. Each supports a single-exposure model
(the level as exposure, demographics as adjusters); the joint
distribution across variables does not exist in the source data, so
these cannot be merged.
"""


def make_variable_datasets(file_path):
    counts = (
        pl.scan_csv(file_path)
        .filter(pl.col("variable").ne("InTx"))
        .group_by(DEMOGRAPHIC_COLS + ["tpm_detailed_value", "variable", "level"])
        .agg(pl.col("value").sum())
        .collect(engine="streaming")
    )
    for (variable,), df in counts.partition_by("variable", as_dict=True).items():
        removed = ["tpm_not_using_or_reduced_value"]
        group_cols = list(DEMOGRAPHIC_COLS)
        if variable in KEEP_TPM_DETAILED:
            group_cols.append("tpm_detailed_value")
        else:
            removed.append("tpm_detailed_value")
        (
            df.group_by(group_cols + ["level"])
            .agg(pl.col("value").sum())
            .write_parquet(
                f"./data/tpm_{variable}.parquet",
                metadata={
                    "Removed in aggregation": ", ".join(removed),
                    "levels": ", ".join(sorted(df["level"].unique())),
                    "variables": variable,
                    "original_dataset": "OFFICIAL_SENSITIVE_TPM_data_2026-06_v7.csv",
                },
            )
        )


"""
Count of TPM classified/new client/missing for completeness percentage and 80%
threshold.
"""

def make_classifiable_completeness_dataset(file_path):
    N_CLASSIFIABLE = (pl.col("n_showing") + pl.col("n_not_yet"))

    COMPLETENESS = N_CLASSIFIABLE / (
        N_CLASSIFIABLE + pl.col("n_missing_completed") + pl.col("n_missing_in_tx")
    )

    (
        pl.scan_csv(file_path)
        .filter(pl.col("variable") == "InTx")
        .with_columns(
            pl.when(pl.col("tpm_value") == "Missing data")
            .then(
                pl.when(
                    pl.col("tpm_detailed_value").str.starts_with(
                        "MISSING DATA - Completed"
                    )
                )
                .then(pl.lit("n_missing_completed"))
                .otherwise(pl.lit("n_missing_in_tx"))
            )
            .otherwise(
                pl.col("tpm_value").replace_strict(
                    {
                        "Showing substantial progress": "n_showing",
                        "Not yet showing substantial progress": "n_not_yet",
                        "New client": "n_new_client",
                        "Missing data": None,
                    }
                )
            )
            .alias("tpm_class")
        )
        .pivot(
            on="tpm_class",
            on_columns=[
                "n_showing",
                "n_not_yet",
                "n_missing_completed",
                "n_missing_in_tx",
                "n_new_client",
            ],
            values="value",
            index=["data_period", "dat", "dat.value", "region"],
            aggregate_function="sum",
        )
        .with_columns(
            n_classifiable=N_CLASSIFIABLE,
            completeness_pct=COMPLETENESS,
            meets_80pct=COMPLETENESS >= 0.8,
        )
        .collect(engine="streaming")
        .write_parquet(
            "./data/tpm_classification_completeness.parquet",
                metadata={
                "Removed in aggregation": "Everything except: data_period, dat, dat.value, region, and aspects of tpm_value/tpm_detailed_value",
                "levels": "InTx", 
                "variables": "InTx",
                "original_dataset": "OFFICIAL_SENSITIVE_TPM_data_2026-06_v7.csv",
                },
        )
    )
