import tomllib

from src import lib

with open("config.toml", "rb") as f:
    RAW_DATA = tomllib.load(f)["raw_data"]

def main():
    lib.make_basic_dataset(file_path=RAW_DATA)
    lib.make_variable_datasets(file_path=RAW_DATA)
    lib.make_classifiable_completeness_dataset(file_path=RAW_DATA)


if __name__ == "__main__":
    main()
