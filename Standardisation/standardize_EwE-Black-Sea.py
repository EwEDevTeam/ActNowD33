from pathlib import Path
import logging
import shutil
import pandas as pd

from helpers.ewe_helpers import (
    read_ewe_timeseries_csv,
    read_ewe_timeseries_csv_that_isnt_properly_aggregated,
)
from helpers.actnow_helpers import IndicatorEnum, ScenarioEnum, FolderTypeEnum, INTERCOMPARISON_REFERENCE_PERIOD


# Shared ActNow intercomparison reference period.
# Used to anchor all historical-relative products across models.
HISTORICAL_REFERENCE_START_YEAR = INTERCOMPARISON_REFERENCE_PERIOD[0]
HISTORICAL_REFERENCE_END_YEAR = INTERCOMPARISON_REFERENCE_PERIOD[1]


INDICATOR_FILE_MAP = {
    "total_biomass.csv": IndicatorEnum.TOTAL_BIOMASS,
    "consumer_biomass.csv": IndicatorEnum.CONSUMER_BIOMASS,
    "pelagic_biomass.csv": IndicatorEnum.PELAGIC_BIOMASS,
    "demersal_biomass.csv": IndicatorEnum.DEMERSAL_BIOMASS,
    "phytoplankton_biomass.csv": IndicatorEnum.PHYTOPLANKTON_BIOMASS,
    "zooplankton_biomass.csv": IndicatorEnum.ZOOPLANKTON_BIOMASS,
    "total_catch.csv": IndicatorEnum.TOTAL_CATCH,
    "consumer_catch.csv": IndicatorEnum.CONSUMER_CATCH,
    "pelagic_catch.csv": IndicatorEnum.PELAGIC_CATCH,
    "demersal_catch.csv": IndicatorEnum.DEMERSAL_CATCH,
    "catch_trophic_level.csv": IndicatorEnum.CATCH_TL,
    "mean_trophic_level.csv": IndicatorEnum.MEAN_TL,
    "evenness_q.csv": IndicatorEnum.EVENNESS_Q,
    "demersal_pelagic_ratio.csv": IndicatorEnum.DEMERSAL_PELAGIC_RATIO,
}


# The Black Sea IBER source data currently does not provide the Inequality scenario.
SCENARIO_MAP = {
    "gs": ScenarioEnum.GS,
    "rr": ScenarioEnum.RR,
    "wm": ScenarioEnum.WM,
}


RUN_TYPE_MAP = {
    "base": FolderTypeEnum.CLIMATE_ONLY,
    "intervention": FolderTypeEnum.CLIMATE_MANAGEMENT,
}


CADENCE_FOLDERS = [
    "Annual",
    "Monthly",
]


CADENCE_IDS = [
    "annual",
    "monthly",
]


def setup_logging(output_root: str | Path) -> logging.Logger:
    output_root = Path(output_root)
    output_root.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger("black-sea-iber-standardizer")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")

    console_handler = logging.StreamHandler()
    console_handler.setFormatter(formatter)

    file_handler = logging.FileHandler(
        output_root / "standardization.log",
        mode="w",
        encoding="utf-8",
    )
    file_handler.setFormatter(formatter)

    logger.addHandler(console_handler)
    logger.addHandler(file_handler)

    return logger


def normalise_filename(filename: str) -> str:
    return filename.strip()


def read_black_sea_indicator(
    source_file: Path,
    indicator: IndicatorEnum,
    logger: logging.Logger,
) -> pd.DataFrame:
    if indicator == IndicatorEnum.CONSUMER_CATCH:
        # Whooot! Whooot! Whooot! <Python air raid sirens blaring>
        return read_ewe_timeseries_csv_that_isnt_properly_aggregated(
            source_file,
            logger,
        )

    return read_ewe_timeseries_csv(source_file)


def standardise_black_sea_iber(
    input_root: str | Path,
    output_root: str | Path,
    historical_reference_start_year: int = HISTORICAL_REFERENCE_START_YEAR,
    historical_reference_end_year: int = HISTORICAL_REFERENCE_END_YEAR,
) -> None:
    input_root = Path(input_root)
    output_root = Path(output_root)
    output_root.mkdir(parents=True, exist_ok=True)

    logger = setup_logging(output_root)

    if historical_reference_start_year > historical_reference_end_year:
        msg = (
            "historical_reference_start_year must be <= "
            "historical_reference_end_year"
        )
        logger.error(msg)
        raise ValueError(msg)

    logger.info(f"Input root: {input_root}")
    logger.info(
        "Historical reference window: "
        f"{historical_reference_start_year}-{historical_reference_end_year}"
    )

    for scenario_folder in input_root.iterdir():
        if not scenario_folder.is_dir():
            continue

        scenario_key = scenario_folder.name.lower()

        if scenario_key not in SCENARIO_MAP:
            logger.warning(f"Skipping unknown scenario folder: {scenario_folder}")
            continue

        scenario_id = SCENARIO_MAP[scenario_key].value

        for run_folder in scenario_folder.iterdir():
            if not run_folder.is_dir():
                continue

            run_key = run_folder.name.lower()

            if run_key not in RUN_TYPE_MAP:
                logger.warning(f"Skipping unknown run folder: {run_folder}")
                continue

            series_type = RUN_TYPE_MAP[run_key].value

            for cadence_folder_name in CADENCE_FOLDERS:
                cadence_folder = run_folder / cadence_folder_name

                if not cadence_folder.exists():
                    logger.info(f"Missing cadence folder: {cadence_folder}")
                    continue

                cadence_id = cadence_folder_name.lower()
                target_folder = output_root / series_type / scenario_id / cadence_id
                target_folder.mkdir(parents=True, exist_ok=True)

                for source_file in cadence_folder.glob("*.csv"):
                    canonical_filename = normalise_filename(source_file.name)

                    if canonical_filename not in INDICATOR_FILE_MAP:
                        logger.info(f"Skipping non-target file: {source_file}")
                        continue

                    indicator = INDICATOR_FILE_MAP[canonical_filename]

                    try:
                        out_df = read_black_sea_indicator(
                            source_file=source_file,
                            indicator=indicator,
                            logger=logger,
                        )
                    except Exception as ex:
                        logger.warning(f"Could not read {source_file}: {ex}")
                        continue

                    if out_df.empty:
                        logger.warning(f"No valid rows in {source_file}")
                        continue

                    out_file = target_folder / f"{indicator.value}.csv"
                    out_df.to_csv(out_file, index=False)

                    logger.info(f"Wrote absolute series: {out_file}")

    write_relative_climate_intervention_time_series(
        case_folder=output_root,
        logger=logger,
    )

    write_relative_historical_climate_time_series(
        case_folder=output_root,
        historical_reference_start_year=historical_reference_start_year,
        historical_reference_end_year=historical_reference_end_year,
        logger=logger,
    )

    write_relative_historical_intervention_time_series(
        case_folder=output_root,
        historical_reference_start_year=historical_reference_start_year,
        historical_reference_end_year=historical_reference_end_year,
        logger=logger,
    )

    logger.info("Black Sea IBER standardisation complete.")


def write_relative_climate_intervention_time_series(
    case_folder: Path,
    logger: logging.Logger,
) -> None:
    for scenario in SCENARIO_MAP.values():
        scenario_id = scenario.value

        for cadence in CADENCE_IDS:
            climate_folder = (
                case_folder
                / FolderTypeEnum.CLIMATE_ONLY.value
                / scenario_id
                / cadence
            )
            management_folder = (
                case_folder
                / FolderTypeEnum.CLIMATE_MANAGEMENT.value
                / scenario_id
                / cadence
            )
            effect_folder = (
                case_folder
                / "relative_climate_intervention"
                / scenario_id
                / cadence
            )

            if not climate_folder.exists():
                logger.info(f"Missing climate_only folder: {climate_folder}")
                continue

            if not management_folder.exists():
                logger.info(f"Missing climate_management folder: {management_folder}")
                continue

            effect_folder.mkdir(parents=True, exist_ok=True)

            for climate_file in climate_folder.glob("*.csv"):
                management_file = management_folder / climate_file.name

                if not management_file.exists():
                    logger.warning(f"Missing paired management file: {management_file}")
                    continue

                write_relative_pair(
                    numerator_file=management_file,
                    denominator_file=climate_file,
                    out_file=effect_folder / climate_file.name,
                    logger=logger,
                    context=f"relative_climate_intervention / {scenario_id} / {cadence}",
                )


def write_relative_historical_climate_time_series(
    case_folder: Path,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    write_relative_to_historical_anchor(
        case_folder=case_folder,
        source_product=FolderTypeEnum.CLIMATE_ONLY.value,
        output_product="relative_historical_climate",
        historical_reference_start_year=historical_reference_start_year,
        historical_reference_end_year=historical_reference_end_year,
        logger=logger,
    )


def write_relative_to_historical_anchor(
    case_folder: Path,
    source_product: str,
    output_product: str,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    for scenario in SCENARIO_MAP.values():
        scenario_id = scenario.value

        for cadence in CADENCE_IDS:
            source_folder = case_folder / source_product / scenario_id / cadence
            output_folder = case_folder / output_product / scenario_id / cadence

            if not source_folder.exists():
                logger.info(
                    f"Missing {source_product} folder for historical comparison: "
                    f"{source_folder}"
                )
                continue

            output_folder.mkdir(parents=True, exist_ok=True)

            for source_file in source_folder.glob("*.csv"):
                source_df = pd.read_csv(source_file)
                source_df["date_parsed"] = pd.to_datetime(
                    source_df["date"],
                    errors="coerce",
                )
                source_df["year"] = source_df["date_parsed"].dt.year

                anchor_mask = source_df["year"] >= historical_reference_start_year
                anchor_mask = anchor_mask & (
                    source_df["year"] <= historical_reference_end_year
                )
                anchor_df = source_df[anchor_mask].copy()

                if anchor_df.empty:
                    logger.warning(
                        f"No rows in historical reference window "
                        f"{historical_reference_start_year}-"
                        f"{historical_reference_end_year}: {source_file}"
                    )
                    continue

                anchor_values = pd.to_numeric(anchor_df["value"], errors="coerce")
                anchor_values = anchor_values.dropna()

                if anchor_values.empty:
                    logger.warning(
                        f"No valid anchor values in historical reference window: "
                        f"{source_file}"
                    )
                    continue

                anchor = float(anchor_values.mean())

                if anchor == 0:
                    logger.warning(
                        f"Historical anchor is zero; cannot compute relative series: "
                        f"{source_file}"
                    )
                    continue

                valid = source_df["value"].notna()
                valid = valid & source_df["date_parsed"].notna()

                invalid_count = len(source_df) - int(valid.sum())
                if invalid_count > 0:
                    logger.warning(
                        f"Excluded {invalid_count} invalid rows from "
                        f"{output_product}: {source_file}"
                    )

                valid_df = source_df[valid].copy()

                if valid_df.empty:
                    logger.warning(
                        f"No valid rows left for {output_product}: {source_file}"
                    )
                    continue

                out_df = pd.DataFrame()
                out_df["date"] = valid_df["date"]
                out_df["value"] = 100.0 * ((valid_df["value"] / anchor) - 1.0)

                out_file = output_folder / source_file.name
                out_df.to_csv(out_file, index=False)

                logger.info(
                    f"Wrote {output_product} time series: {out_file} "
                    f"(anchor={anchor}, years="
                    f"{historical_reference_start_year}-"
                    f"{historical_reference_end_year})"
                )


def write_relative_pair(
    numerator_file: Path,
    denominator_file: Path,
    out_file: Path,
    logger: logging.Logger,
    context: str,
) -> None:
    numerator_df = pd.read_csv(numerator_file)
    denominator_df = pd.read_csv(denominator_file)

    merged = pd.merge(
        numerator_df,
        denominator_df,
        on="date",
        how="inner",
        suffixes=("_numerator", "_denominator"),
    )

    if merged.empty:
        logger.warning(f"No overlapping dates for {context}: {numerator_file}")
        return

    valid = merged["value_denominator"].notna()
    valid = valid & merged["value_numerator"].notna()
    valid = valid & (merged["value_denominator"] != 0)

    invalid_count = len(merged) - int(valid.sum())
    if invalid_count > 0:
        logger.warning(f"Excluded {invalid_count} invalid paired rows: {context}")

    merged = merged[valid].copy()

    if merged.empty:
        logger.warning(f"No valid paired rows for {context}: {numerator_file}")
        return

    out_df = pd.DataFrame()
    out_df["date"] = merged["date"]
    out_df["value"] = 100.0 * (
        (merged["value_numerator"] / merged["value_denominator"]) - 1.0
    )

    out_file.parent.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(out_file, index=False)

    logger.info(f"Wrote {context} time series: {out_file}")


def write_relative_historical_intervention_time_series(
    case_folder: Path,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    for scenario in SCENARIO_MAP.values():
        scenario_id = scenario.value

        for cadence in CADENCE_IDS:
            management_folder = (
                case_folder
                / FolderTypeEnum.CLIMATE_MANAGEMENT.value
                / scenario_id
                / cadence
            )
            output_folder = (
                case_folder
                / "relative_historical_intervention"
                / scenario_id
                / cadence
            )

            if not management_folder.exists():
                logger.info(
                    f"Missing climate_management folder for historical comparison: "
                    f"{management_folder}"
                )
                continue

            output_folder.mkdir(parents=True, exist_ok=True)

            for management_file in management_folder.glob("*.csv"):
                management_df = pd.read_csv(management_file)
                management_df["date_parsed"] = pd.to_datetime(
                    management_df["date"],
                    errors="coerce",
                )
                management_df["year"] = management_df["date_parsed"].dt.year

                anchor_mask = management_df["year"] >= historical_reference_start_year
                anchor_mask = anchor_mask & (
                    management_df["year"] <= historical_reference_end_year
                )
                anchor_df = management_df[anchor_mask].copy()

                if anchor_df.empty:
                    logger.warning(
                        f"No rows in historical reference window "
                        f"{historical_reference_start_year}-"
                        f"{historical_reference_end_year}: {management_file}"
                    )
                    continue

                anchor_values = pd.to_numeric(anchor_df["value"], errors="coerce")
                anchor_values = anchor_values.dropna()

                if anchor_values.empty:
                    logger.warning(
                        f"No valid anchor values in historical reference window: "
                        f"{management_file}"
                    )
                    continue

                anchor = float(anchor_values.mean())

                if anchor == 0:
                    logger.warning(
                        f"Historical anchor is zero; cannot compute relative series: "
                        f"{management_file}"
                    )
                    continue

                valid = management_df["value"].notna()
                valid = valid & management_df["date_parsed"].notna()

                invalid_count = len(management_df) - int(valid.sum())
                if invalid_count > 0:
                    logger.warning(
                        f"Excluded {invalid_count} invalid rows from "
                        f"relative_historical_intervention: {management_file}"
                    )

                valid_df = management_df[valid].copy()

                if valid_df.empty:
                    logger.warning(
                        f"No valid rows left for relative_historical_intervention: "
                        f"{management_file}"
                    )
                    continue

                out_df = pd.DataFrame()
                out_df["date"] = valid_df["date"]
                out_df["value"] = 100.0 * ((valid_df["value"] / anchor) - 1.0)

                out_file = output_folder / management_file.name
                out_df.to_csv(out_file, index=False)

                logger.info(
                    f"Wrote relative_historical_intervention time series: {out_file} "
                    f"(anchor={anchor}, years="
                    f"{historical_reference_start_year}-"
                    f"{historical_reference_end_year})"
                )


if __name__ == "__main__":
    standardise_black_sea_iber(
        input_root=r"cs07_black-sea_ewe/raw",
        output_root=r"cs07_black-sea_ewe/for_analysis",
    )
