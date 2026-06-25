from pathlib import Path
import logging
import shutil
import pandas as pd
from helpers.ewe_helpers import read_ewe_timeseries_csv, enum_value
from helpers.actnow_helpers import (
    detect_cadence_from_dates,
    FolderTypeEnum,
    IndicatorEnum,
    ScenarioEnum,
    INTERCOMPARISON_REFERENCE_PERIOD,
)
SCENARIO_MAP = {
    "GS": ScenarioEnum.GS,
    "IQ": ScenarioEnum.IQ,
    "RR": ScenarioEnum.RR,
    "WM": ScenarioEnum.WM,
}

RUN_TYPE_TO_PRODUCT = {
    "base": "climate_only",
    "intervention": "climate_management",
}

INDICATOR_MAP = {
    "evenness_q": IndicatorEnum.EVENNESS_Q,
}

INTERCOMPARISON_REFERENCE_PERIOD = (
    INTERCOMPARISON_REFERENCE_PERIOD[0],
    INTERCOMPARISON_REFERENCE_PERIOD[1]
)


def setup_logger(output_root: Path) -> logging.Logger:
    output_root.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger("standardise-osmose-med")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")

    console = logging.StreamHandler()
    console.setFormatter(formatter)
    logger.addHandler(console)

    file_handler = logging.FileHandler(output_root / "standardization.log", mode="w", encoding="utf-8")
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    return logger


def normalise_date(value: str) -> str:
    value = str(value).strip()

    if len(value) == 7:
        return f"{value}-01"

    return value


def read_timeseries(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)

    if {"date", "value"}.issubset(df.columns):
        out = df[["date", "value"]].copy()
    else:
        out = df.iloc[:, 0:2].copy()
        out.columns = ["date", "value"]

    out["date"] = out["date"].apply(normalise_date)
    out["date"] = pd.to_datetime(out["date"], errors="coerce")
    out["value"] = pd.to_numeric(out["value"], errors="coerce")

    out = out.dropna(subset=["date"])
    out["date"] = out["date"].dt.strftime("%Y-%m-%d")

    return out[["date", "value"]]


def detect_cadence(df: pd.DataFrame) -> str:
    dates = pd.to_datetime(df["date"])

    if len(dates) < 2:
        return "unknown"

    diffs = dates.sort_values().diff().dropna().dt.days
    median_diff = diffs.median()

    if 27 <= median_diff <= 32:
        return "monthly"

    if 360 <= median_diff <= 370:
        return "annual"

    return "irregular"


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


def standardise_absolute_products(input_root: Path, output_root: Path, logger: logging.Logger) -> None:
    for scenario_folder in input_root.iterdir():
        if not scenario_folder.is_dir():
            continue

        scenario_name = scenario_folder.name

        if scenario_name.lower() == "hindcast":
            run_folder = scenario_folder / "base"
            data_folder = find_data_folder(run_folder)

            if data_folder is None:
                logger.warning(f"No hindcast data found in {run_folder}")
                continue

            for file in find_indicator_files(data_folder):
                indicator = INDICATOR_MAP.get(file.stem, file.stem)
                df = read_timeseries(file)
                cadence = detect_cadence(df)

                out_file = (
                    output_root
                    / "historical_context"
                    / "hindcast"
                    / cadence
                    / f"{indicator}.csv"
                )

                write_timeseries(df, out_file)
                logger.info(f"Wrote {out_file}")

            continue

        if scenario_name not in SCENARIO_MAP:
            logger.warning(f"Skipping unmapped scenario folder: {scenario_name}")
            continue

        scenario = enum_value(SCENARIO_MAP[scenario_name])

        for run_type, product in RUN_TYPE_TO_PRODUCT.items():
            run_folder = scenario_folder / run_type
            data_folder = find_data_folder(run_folder)

            if data_folder is None:
                logger.warning(f"No data found for {scenario_name}/{run_type}")
                continue

            for file in find_indicator_files(data_folder):
                indicator = INDICATOR_MAP.get(file.stem, file.stem)
                df = read_timeseries(file)
                cadence = detect_cadence(df)

                years = pd.to_datetime(df["date"]).dt.year
                if years.min() > 2050:
                    logger.warning(
                        f"Suspicious forecast start year in {file}: "
                        f"{years.min()}-{years.max()}"
                    )

                out_file = (
                    output_root
                    / product
                    / scenario
                    / cadence
                    / f"{indicator}.csv"
                )

                write_timeseries(df, out_file)
                logger.info(f"Wrote {out_file}")


def reference_mean(historical_df: pd.DataFrame) -> float | None:
    start_year, end_year = INTERCOMPARISON_REFERENCE_PERIOD

    df = historical_df.copy()
    df["year"] = pd.to_datetime(df["date"]).dt.year

    ref = df[
        (df["year"] >= start_year)
        & (df["year"] <= end_year)
    ]

    if ref.empty:
        return None

    return ref["value"].mean()


def generate_relative_historical(output_root: Path, logger: logging.Logger) -> None:
    historical_root = output_root / "historical_context" / "hindcast"

    for product in ["climate_only", "climate_management"]:
        product_root = output_root / product

        if not product_root.exists():
            continue

        target_product = (
            "relative_historical_climate"
            if product == "climate_only"
            else "relative_historical_intervention"
        )

        for input_file in product_root.glob("*/*/*.csv"):
            scenario = input_file.parts[-3]
            cadence = input_file.parts[-2]
            indicator = input_file.stem

            hist_file = historical_root / cadence / f"{indicator}.csv"

            if not hist_file.exists():
                logger.warning(f"No historical reference for {input_file}")
                continue

            hist_df = read_timeseries(hist_file)
            ref_value = reference_mean(hist_df)

            if ref_value is None or ref_value == 0:
                logger.warning(f"Invalid reference value for {hist_file}")
                continue

            df = read_timeseries(input_file)
            df["value"] = 100.0 * ((df["value"] / ref_value) - 1.0)

            out_file = (
                output_root
                / target_product
                / scenario
                / cadence
                / f"{indicator}.csv"
            )

            write_timeseries(df, out_file)
            logger.info(f"Wrote {out_file}")


def generate_relative_intervention(output_root: Path, logger: logging.Logger) -> None:
    climate_root = output_root / "climate_only"
    management_root = output_root / "climate_management"

    if not climate_root.exists() or not management_root.exists():
        return

    for management_file in management_root.glob("*/*/*.csv"):
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

        paired["value"] = 100.0 * ((paired["management"] / paired["climate"]) - 1.0)
        paired.loc[paired["climate"] == 0, "value"] = pd.NA

        out_df = paired[["date", "value"]]

        out_file = (
            output_root
            / "relative_intervention"
            / scenario
            / cadence
            / f"{indicator}.csv"
        )

        write_timeseries(out_df, out_file)
        logger.info(f"Wrote {out_file}")


def standardise_osmose_black_sea(
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

    standardise_absolute_products(input_root, output_root, logger)
    generate_relative_historical(output_root, logger)
    generate_relative_intervention(output_root, logger)

    logger.info("Done.")


if __name__ == "__main__":
    standardise_osmose_black_sea(
        input_root=r"cs07_black-sea_osmose\raw",
        output_root=r"cs07_black-sea_osmose\for_analysis",
        clean_output=True,
    )