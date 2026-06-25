from pathlib import Path
import logging
import shutil
import pandas as pd

from helpers.ewe_helpers import read_ewe_timeseries_csv, enum_value
from helpers.actnow_helpers import (
    detect_cadence_from_dates,
    FolderTypeEnum,
    ScenarioEnum,
    INTERCOMPARISON_REFERENCE_PERIOD,
)


# ============================================================
# ActNow standardiser - Mediterranean OSMOSE
# ============================================================
#
# Expected input structure:
#
# raw/
#   reference/
#       base/
#           <indicator>.csv
#   GS/
#       base/
#           <indicator>.csv
#       intervention/
#           <indicator>.csv
#   IQ/
#       base/
#           <indicator>.csv
#       intervention/
#           <indicator>.csv
#   RR/
#       base/
#           <indicator>.csv
#       intervention/
#           <indicator>.csv
#   WM/
#       base/
#           <indicator>.csv
#       intervention/
#           <indicator>.csv
#
# Input files contain a date column in yyyy-MM format and a value column.
# We intentionally read them via read_ewe_timeseries_csv because its
# normalise_ewe_time_column() helper already expands yyyy-MM to yyyy-MM-dd.
# This is an OSMOSE dataset, but the date repair is generic and useful here.
#
# The historical anchor is the reference/base scenario. Relative-to-historical
# products are computed against the shared ActNow intercomparison reference
# period from helpers.actnow_helpers.INTERCOMPARISON_REFERENCE_PERIOD.
#
# If the reference data starts after the first reference year, the script uses
# the available overlap and logs a warning. For the current Med OSMOSE delivery,
# the reference series starts in 2006, so the effective anchor is 2006-2015.


SCENARIO_MAP = {
    "gs": ScenarioEnum.GS,
    "iq": ScenarioEnum.IQ,
    "rr": ScenarioEnum.RR,
    "wm": ScenarioEnum.WM,
}

RUN_TYPE_TO_PRODUCT = {
    "base": FolderTypeEnum.CLIMATE_ONLY,
    "intervention": FolderTypeEnum.CLIMATE_MANAGEMENT,
}

# Keep this intentionally conservative. Do not infer broad ecological mappings
# here unless agreed with the modelling team.
INDICATOR_MAP = {
    "evenness_q": "richness_proxy",
}

REFERENCE_SCENARIO_FOLDER = "reference"
REFERENCE_RUN_FOLDER = "base"
HISTORICAL_CONTEXT_SCENARIO = "reference"

RELATIVE_HISTORICAL_CLIMATE = "relative_historical_climate"
RELATIVE_HISTORICAL_INTERVENTION = "relative_historical_intervention"
RELATIVE_INTERVENTION = "relative_intervention"

HISTORICAL_REFERENCE_START_YEAR = INTERCOMPARISON_REFERENCE_PERIOD[0]
HISTORICAL_REFERENCE_END_YEAR = INTERCOMPARISON_REFERENCE_PERIOD[1]


# ------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------

def setup_logger(output_root: Path) -> logging.Logger:
    output_root.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger("standardise-osmose-med")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")

    console = logging.StreamHandler()
    console.setFormatter(formatter)
    logger.addHandler(console)

    file_handler = logging.FileHandler(
        output_root / "standardization.log",
        mode="w",
        encoding="utf-8",
    )
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    return logger


def read_timeseries(path: Path) -> pd.DataFrame:
    """
    Read an OSMOSE indicator time series.

    The file format is simple CSV, but the date values are yyyy-MM. The EwE
    helper already normalises this to yyyy-MM-dd, so we use it consistently for
    all absolute, reference, and relative-product reads.
    """
    df = read_ewe_timeseries_csv(path)

    required = {"date", "value"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"{path} is missing columns: {sorted(missing)}")

    return df[["date", "value"]].copy()


def write_timeseries(df: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(path, index=False)


def find_indicator_files(folder: Path) -> list[Path]:
    return sorted(folder.glob("*.csv"))


def find_data_folder(run_folder: Path) -> Path | None:
    if find_indicator_files(run_folder):
        return run_folder

    monthly = run_folder / "monthly"
    if monthly.exists() and find_indicator_files(monthly):
        return monthly

    annual = run_folder / "annual"
    if annual.exists() and find_indicator_files(annual):
        return annual

    return None


def canonical_indicator_name(file_stem: str) -> str:
    mapped = INDICATOR_MAP.get(file_stem, file_stem)
    return enum_value(mapped)


# ------------------------------------------------------------
# Absolute product writing
# ------------------------------------------------------------

def write_reference_context(
    input_root: Path,
    output_root: Path,
    logger: logging.Logger,
) -> None:
    reference_run_folder = input_root / REFERENCE_SCENARIO_FOLDER / REFERENCE_RUN_FOLDER
    data_folder = find_data_folder(reference_run_folder)

    if data_folder is None:
        logger.warning(f"No reference data found in {reference_run_folder}")
        return

    for file in find_indicator_files(data_folder):
        indicator = canonical_indicator_name(file.stem)
        df = read_timeseries(file)
        cadence = detect_cadence_from_dates(df)

        out_file = (
            output_root
            / "historical_context"
            / HISTORICAL_CONTEXT_SCENARIO
            / cadence
            / f"{indicator}.csv"
        )

        write_timeseries(df, out_file)
        logger.info(f"Wrote reference context: {out_file}")


def write_absolute_products(
    input_root: Path,
    output_root: Path,
    logger: logging.Logger,
) -> None:
    for scenario_folder in sorted(input_root.iterdir()):
        if not scenario_folder.is_dir():
            continue

        scenario_key = scenario_folder.name.lower()

        if scenario_key == REFERENCE_SCENARIO_FOLDER:
            continue

        if scenario_key not in SCENARIO_MAP:
            logger.warning(f"Skipping unmapped scenario folder: {scenario_folder.name}")
            continue

        scenario = enum_value(SCENARIO_MAP[scenario_key])

        for run_type, product_enum in RUN_TYPE_TO_PRODUCT.items():
            run_folder = scenario_folder / run_type
            data_folder = find_data_folder(run_folder)

            if data_folder is None:
                logger.warning(f"No data found for {scenario_folder.name}/{run_type}")
                continue

            product = enum_value(product_enum)

            for file in find_indicator_files(data_folder):
                indicator = canonical_indicator_name(file.stem)
                df = read_timeseries(file)
                cadence = detect_cadence_from_dates(df)

                out_file = output_root / product / scenario / cadence / f"{indicator}.csv"
                write_timeseries(df, out_file)
                logger.info(f"Wrote absolute product: {out_file}")


# ------------------------------------------------------------
# Relative products
# ------------------------------------------------------------

def reference_mean(
    historical_df: pd.DataFrame,
    indicator: str,
    logger: logging.Logger,
) -> float | None:
    df = historical_df.copy()
    df["year"] = pd.to_datetime(df["date"]).dt.year

    ref = df[
        (df["year"] >= HISTORICAL_REFERENCE_START_YEAR)
        & (df["year"] <= HISTORICAL_REFERENCE_END_YEAR)
    ].copy()

    if ref.empty:
        logger.warning(
            f"No reference-period rows for {indicator}: "
            f"{HISTORICAL_REFERENCE_START_YEAR}-{HISTORICAL_REFERENCE_END_YEAR}"
        )
        return None

    available_years = sorted(ref["year"].unique())
    expected_years = list(
        range(HISTORICAL_REFERENCE_START_YEAR, HISTORICAL_REFERENCE_END_YEAR + 1)
    )

    missing_years = sorted(set(expected_years) - set(available_years))
    if missing_years:
        logger.warning(
            f"Reference-period overlap for {indicator} is incomplete. "
            f"Using years {available_years}; missing {missing_years}."
        )

    return ref["value"].mean()


def generate_relative_historical(output_root: Path, logger: logging.Logger) -> None:
    historical_root = output_root / "historical_context" / HISTORICAL_CONTEXT_SCENARIO

    product_map = {
        enum_value(FolderTypeEnum.CLIMATE_ONLY): RELATIVE_HISTORICAL_CLIMATE,
        enum_value(FolderTypeEnum.CLIMATE_MANAGEMENT): RELATIVE_HISTORICAL_INTERVENTION,
    }

    for source_product, target_product in product_map.items():
        product_root = output_root / source_product

        if not product_root.exists():
            continue

        for input_file in sorted(product_root.glob("*/*/*.csv")):
            scenario = input_file.parts[-3]
            cadence = input_file.parts[-2]
            indicator = input_file.stem

            hist_file = historical_root / cadence / f"{indicator}.csv"

            if not hist_file.exists():
                logger.warning(f"No reference file for {input_file}")
                continue

            hist_df = read_timeseries(hist_file)
            ref_value = reference_mean(hist_df, indicator, logger)

            if ref_value is None or ref_value == 0:
                logger.warning(f"Invalid reference value for {hist_file}")
                continue

            df = read_timeseries(input_file)
            df["value"] = 100.0 * ((df["value"] / ref_value) - 1.0)

            out_file = output_root / target_product / scenario / cadence / f"{indicator}.csv"
            write_timeseries(df, out_file)
            logger.info(f"Wrote relative historical product: {out_file}")


def generate_relative_intervention(output_root: Path, logger: logging.Logger) -> None:
    climate_root = output_root / enum_value(FolderTypeEnum.CLIMATE_ONLY)
    management_root = output_root / enum_value(FolderTypeEnum.CLIMATE_MANAGEMENT)

    if not climate_root.exists() or not management_root.exists():
        return

    for management_file in sorted(management_root.glob("*/*/*.csv")):
        scenario = management_file.parts[-3]
        cadence = management_file.parts[-2]
        indicator = management_file.stem

        climate_file = climate_root / scenario / cadence / f"{indicator}.csv"

        if not climate_file.exists():
            logger.warning(f"No climate-only pair for {management_file}")
            continue

        climate = read_timeseries(climate_file).rename(columns={"value": "climate"})
        management = read_timeseries(management_file).rename(columns={"value": "management"})

        paired = climate.merge(management, on="date", how="inner")

        if paired.empty:
            logger.warning(f"No overlapping dates for {management_file}")
            continue

        valid = paired["climate"].notna()
        valid = valid & paired["management"].notna()
        valid = valid & (paired["climate"] != 0)

        if not valid.all():
            logger.warning(
                f"Excluded {len(paired) - valid.sum()} invalid rows for "
                f"relative intervention: {scenario}/{cadence}/{indicator}"
            )

        paired = paired[valid].copy()

        if paired.empty:
            logger.warning(
                f"No valid rows left for relative intervention: "
                f"{scenario}/{cadence}/{indicator}"
            )
            continue

        out_df = pd.DataFrame()
        out_df["date"] = paired["date"]
        out_df["value"] = 100.0 * ((paired["management"] / paired["climate"]) - 1.0)

        out_file = output_root / RELATIVE_INTERVENTION / scenario / cadence / f"{indicator}.csv"
        write_timeseries(out_df, out_file)
        logger.info(f"Wrote relative intervention product: {out_file}")


# ------------------------------------------------------------
# Entrypoint
# ------------------------------------------------------------

def standardise_osmose_med(
    input_root: str | Path,
    output_root: str | Path,
    clean_output: bool = True,
) -> None:
    input_root = Path(input_root)
    output_root = Path(output_root)

    if clean_output and output_root.exists():
        shutil.rmtree(output_root)

    logger = setup_logger(output_root)

    logger.info(f"Input root: {input_root}")
    logger.info(f"Output root: {output_root}")
    logger.info(
        f"ActNow intercomparison reference period: "
        f"{HISTORICAL_REFERENCE_START_YEAR}-{HISTORICAL_REFERENCE_END_YEAR}"
    )

    write_reference_context(input_root, output_root, logger)
    write_absolute_products(input_root, output_root, logger)
    generate_relative_historical(output_root, logger)
    generate_relative_intervention(output_root, logger)

    logger.info("Done.")


if __name__ == "__main__":
    standardise_osmose_med(
        input_root=r"cs11-med-osmose\raw",
        output_root=r"cs11-med-osmose\for_analysis",
        clean_output=True,
    )
