import polars as pl

@pl.api.register_dataframe_namespace("skp")
class SkpNamespace:
    def __init__(self, df: pl.DataFrame):
        self._df = df

    def inspect_col(self, col: str) -> list[str]:
        return self._df.select(col).unique().to_series().to_list()



