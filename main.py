import polars as pl
from src import lib

RAW_DATA = "../shared/OFFICIAL_SENSITIVE_TPM_data_2026-06_v7.csv"

def main():
    lib.make_basic_dataset(file_path=RAW_DATA)
    lib.make_variable_datasets(file_path=RAW_DATA)
    lib.make_classifiable_completeness_dataset(file_path=RAW_DATA)


if __name__ == "__main__":
    main()
