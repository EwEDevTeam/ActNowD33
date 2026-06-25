#
# A number of helper functions to deal with Ecopath with Ecosim time series output
#

from io import StringIO
from pathlib import Path
import csv
import pandas as pd


# The possible encodings across OS-es
EWE_TEXT_ENCODINGS = [
    "utf-8-sig",
    "utf-8",
    "cp1252",
    "latin1",
]


#
# Helper function, decodes EwE text to a string
#
def read_ewe_text(path: Path) -> str:
    for encoding in EWE_TEXT_ENCODINGS:
        try:
            return path.read_text(encoding=encoding)
        except UnicodeDecodeError:
            continue

    raise UnicodeDecodeError(
        "unknown",
        b"",
        0,
        1,
        f"Could not decode {path} with supported encodings: {EWE_TEXT_ENCODINGS}",
    )


#
# Utility function, returns the header row of a CSV file, skipping 
# the EwE fileheader if present.
#
def find_ewe_data_header_row_from_text(text: str, path: Path | None = None) -> int:
    lines = text.splitlines()

    inside_header = False

    for i, line in enumerate(lines):
        line_lower = line.strip().lower()

        if line_lower.startswith("</header"):
            inside_header = False
            continue

        if line_lower.startswith("<header end"):
            inside_header = False
            continue

        if line_lower.startswith("<header"):
            inside_header = True
            continue

        if inside_header:
            continue

        rows = list(csv.reader([line_lower], quotechar='"', delimiter=",", skipinitialspace=True,))
        if not rows:
            continue

        parts = [part.strip() for part in rows[0]]

        if len(parts) < 2:
            continue

        first_column = parts[0]

        if first_column.startswith("year"):
            return i

        if first_column.startswith("timestep"):
            return i

        if first_column.startswith("date"):
            return i

    if path is None:
        raise ValueError("Could not find EwE data header row.")

    raise ValueError(f"Could not find EwE data header row in {path}")


#
# Helper function, ensures that the time entrues are correctly represented as yyyy-MM-dd (year-month-day).
#
def normalise_ewe_time_column(time_series: pd.Series) -> pd.Series:
    values = time_series.astype(str).str.strip()

    parsed = pd.to_datetime(values, errors="coerce")

    missing = parsed.isna()
    if missing.any():
        numeric_values = pd.to_numeric(values[missing], errors="coerce")

        # Annual years, e.g. 2030
        annual_mask = numeric_values.notna() & (numeric_values % 1 == 0)

        replacement = pd.Series(index=values[missing].index, dtype="datetime64[ns]")
        replacement.loc[annual_mask] = pd.to_datetime(
            numeric_values[annual_mask].astype("Int64").astype(str) + "-01-01",
            errors="coerce",
        )

        parsed.loc[missing] = replacement

    return parsed.dt.strftime("%Y-%m-%d")


#
# Helper function, uses the methods above to read a CSV file into a dataframe, dealing
# with all possible EwE oddities
#
def read_ewe_timeseries_csv(path: str | Path) -> pd.DataFrame:
    path = Path(path)

    text = read_ewe_text(path)
    header_index = find_ewe_data_header_row_from_text(text, path)

    df = pd.read_csv(StringIO(text), skiprows=header_index)

    if len(df.columns) < 2:
        raise ValueError(f"Expected at least two columns in {path}")

    time_column = df.columns[0]
    value_column = df.columns[1]

    out_df = pd.DataFrame()
    out_df["date"] = normalise_ewe_time_column(df[time_column])
    out_df["value"] = pd.to_numeric(df[value_column], errors="coerce")

    out_df = out_df.dropna(subset=["date", "value"])
    out_df = out_df.sort_values("date")

    return out_df

#
# A Black Sea case study workaround, where CSV information was provided by 
# functional group instead of an aggregated total. Bad people. Baaaaaad people.
#
def read_ewe_timeseries_csv_that_isnt_properly_aggregated(path: Path, logger) -> pd.DataFrame:
    """
    Black Sea IBER workaround.

    Some indicators expected to be aggregated are exported as
    multiple consumer-group columns. This function sums all
    value columns row-wise.

    Do not use unless the source dataset is known to exhibit
    this problem.
    """

    text = read_ewe_text(path)
    header_index = find_ewe_data_header_row_from_text(text, path)

    raw_df = pd.read_csv(StringIO(text), skiprows=header_index)

    if len(raw_df.columns) < 2:
        raise ValueError(f"Expected at least two columns in {path}")

    out_df = pd.DataFrame()
    out_df["date"] = normalise_ewe_time_column(raw_df.iloc[:, 0])

    value_series = pd.to_numeric(raw_df.iloc[:, 1], errors="coerce")

    if len(raw_df.columns) <= 2:
        raise ValueError(
            f"Expected multiple consumer-group columns in {path}"
        )

    logger.warning(
        "!!! BLACK SEA IBER DATA REPAIR !!! "
        f"{path} has more than two columns, and the expected value column "
        "is zero. Summing all numeric consumer-group columns row-wise. "
        "Verify this repair with the modelling team."
    )

    value_df = raw_df.iloc[:, 1:].apply(pd.to_numeric, errors="coerce")
    value_series = value_df.sum(axis=1, skipna=True)

    out_df["value"] = value_series
    out_df = out_df.dropna(subset=["date", "value"])
    out_df = out_df.sort_values("date")

    return out_df


def enum_value(value) -> str:
    if hasattr(value, "value"):
        return value.value

    return str(value)