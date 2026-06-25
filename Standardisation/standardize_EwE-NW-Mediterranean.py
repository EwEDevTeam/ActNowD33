from pathlib import Path
import logging
import shutil
import pandas as pd

from helpers.ewe_helpers import read_ewe_timeseries_csv
from helpers.actnow_helpers import (
    detect_cadence_from_dates,
    FolderTypeEnum,
    IndicatorEnum,
    ScenarioEnum,
    INTERCOMPARISON_REFERENCE_PERIOD,
)


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
    "fish_biomass.csv": IndicatorEnum.FISH_BIOMASS,
    "total_catch.csv": IndicatorEnum.TOTAL_CATCH,
    "consumer_catch.csv": IndicatorEnum.CONSUMER_CATCH,
    "pelagic_catch.csv": IndicatorEnum.PELAGIC_CATCH,
    "demersal_catch.csv": IndicatorEnum.DEMERSAL_CATCH,
    "catch_trophic_level.csv": IndicatorEnum.CATCH_TL,
    "mean_trophic_level.csv": IndicatorEnum.MEAN_TL,
    "evenness_q.csv": IndicatorEnum.EVENNESS_Q,
    "higher_trophic_groups_biomass.csv": IndicatorEnum.HTL_GROUPS_BIOMASS,
    "benthic_invertebrates_biomass.csv": IndicatorEnum.BENTHIC_INVERTEBRATES_BIOMASS,
    "demersal_pelagic_ratio.csv": IndicatorEnum.DEMERSAL_PELAGIC_RATIO,
}


TYPO_FILE_MAP = {
}


SCENARIO_MAP = {
    "gs": ScenarioEnum.GS,
    "in": ScenarioEnum.IQ,
    "iq": ScenarioEnum.IQ,
    "rr": ScenarioEnum.RR,
    "wm": ScenarioEnum.WM,
}


RUN_TYPE_MAP = {
    "base": FolderTypeEnum.CLIMATE_ONLY,
    "intervention": FolderTypeEnum.CLIMATE_MANAGEMENT,
}


def setup_logging(output_root: str | Path) -> logging.Logger:
    output_root = Path(output_root)
    output_root.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger("ewe-ns-standardizer")
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
    filename = filename.strip()

    if filename in TYPO_FILE_MAP:
        return TYPO_FILE_MAP[filename]

    return filename


def standardise_ewe_ns(
    input_root: str | Path,
    output_root: str | Path,
    case_study: str = "",
    area: str = "",
    historical_reference_start_year: int = HISTORICAL_REFERENCE_START_YEAR,
    historical_reference_end_year: int = HISTORICAL_REFERENCE_END_YEAR,
) -> None:
    input_root = Path(input_root)
    output_root = Path(output_root)

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

    case_folder = output_root
    case_folder.mkdir(parents=True, exist_ok=True)

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

            annual_folder = run_folder / "annual"

            if annual_folder.exists():
                data_folder = annual_folder
            else:
                data_folder = run_folder

            for source_file in data_folder.glob("*.csv"):
                canonical_filename = normalise_filename(source_file.name)

                if canonical_filename not in INDICATOR_FILE_MAP:
                    logger.info(f"Skipping non-target file: {source_file}")
                    continue

                indicator = INDICATOR_FILE_MAP[canonical_filename].value

                try:
                    out_df = read_ewe_timeseries_csv(source_file)
                except Exception as ex:
                    logger.warning(f"Could not read {source_file}: {ex}")
                    continue

                if out_df.empty:
                    logger.warning(f"No valid rows in {source_file}")
                    continue

                cadence = detect_cadence_from_dates(out_df)

                if cadence in ["unknown", "irregular"]:
                    logger.warning(
                        f"Detected cadence '{cadence}' for {source_file}. "
                        "Writing to that cadence folder; please verify."
                    )

                target_folder = case_folder / series_type / scenario_id / cadence
                target_folder.mkdir(parents=True, exist_ok=True)

                out_file = target_folder / f"{indicator}.csv"
                out_df.to_csv(out_file, index=False)

                logger.info(f"Wrote absolute series: {out_file}")

    write_relative_climate_intervention_time_series(
        case_folder=case_folder,
        logger=logger,
    )

    write_relative_historical_climate_time_series(
        case_folder=case_folder,
        historical_reference_start_year=historical_reference_start_year,
        historical_reference_end_year=historical_reference_end_year,
        logger=logger,
    )

    write_relative_historical_intervention_time_series(
        case_folder=case_folder,
        historical_reference_start_year=historical_reference_start_year,
        historical_reference_end_year=historical_reference_end_year,
        logger=logger,
    )

    logger.info(f"{case_study} {area} standardisation complete.")



def write_relative_historical_climate_time_series(
    case_folder: Path,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    write_relative_to_own_historical_anchor(
        case_folder=case_folder,
        source_product=FolderTypeEnum.CLIMATE_ONLY.value,
        output_product="relative_historical_climate",
        historical_reference_start_year=historical_reference_start_year,
        historical_reference_end_year=historical_reference_end_year,
        logger=logger,
    )


def write_relative_to_own_historical_anchor(
    case_folder: Path,
    source_product: str,
    output_product: str,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    for scenario in SCENARIO_MAP.values():
        scenario_id = scenario.value

        source_scenario_folder = case_folder / source_product / scenario_id
        output_scenario_folder = case_folder / output_product / scenario_id

        if not source_scenario_folder.exists():
            logger.warning(
                f"Missing {source_product} folder for historical comparison: "
                f"{source_scenario_folder}"
            )
            continue

        for cadence_folder in source_scenario_folder.iterdir():
            if not cadence_folder.is_dir():
                continue

            cadence = cadence_folder.name
            output_folder = output_scenario_folder / cadence
            output_folder.mkdir(parents=True, exist_ok=True)

            for source_file in cadence_folder.glob("*.csv"):
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
                        "No valid anchor values in historical reference window: "
                        f"{source_file}"
                    )
                    continue

                anchor = float(anchor_values.mean())

                if anchor == 0:
                    logger.warning(
                        "Historical anchor is zero; cannot compute relative series: "
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
                    f"Wrote {output_product} series: {out_file} "
                    f"(anchor={anchor}, years="
                    f"{historical_reference_start_year}-"
                    f"{historical_reference_end_year})"
                )

def write_relative_climate_intervention_time_series(
    case_folder: Path,
    logger: logging.Logger,
) -> None:
    for scenario in SCENARIO_MAP.values():
        scenario_id = scenario.value

        climate_only_folder = (
            case_folder / FolderTypeEnum.CLIMATE_ONLY.value / scenario_id
        )
        climate_management_folder = (
            case_folder / FolderTypeEnum.CLIMATE_MANAGEMENT.value / scenario_id
        )
        output_scenario_folder = (
            case_folder / "relative_climate_intervention" / scenario_id
        )

        if not climate_only_folder.exists():
            logger.warning(f"Missing climate_only folder: {climate_only_folder}")
            continue

        if not climate_management_folder.exists():
            logger.warning(
                f"Missing climate_management folder: {climate_management_folder}"
            )
            continue

        for cadence_folder in climate_only_folder.iterdir():
            if not cadence_folder.is_dir():
                continue

            cadence = cadence_folder.name
            climate_management_cadence_folder = climate_management_folder / cadence
            output_folder = output_scenario_folder / cadence

            if not climate_management_cadence_folder.exists():
                logger.warning(
                    "Missing climate_management cadence folder: "
                    f"{climate_management_cadence_folder}"
                )
                continue

            output_folder.mkdir(parents=True, exist_ok=True)

            for base_file in cadence_folder.glob("*.csv"):
                management_file = climate_management_cadence_folder / base_file.name

                if not management_file.exists():
                    logger.warning(
                        f"Missing paired intervention file: {management_file}"
                    )
                    continue

                base_df = pd.read_csv(base_file)
                management_df = pd.read_csv(management_file)

                merged = pd.merge(
                    management_df,
                    base_df,
                    on="date",
                    how="inner",
                    suffixes=("_management", "_climate_only"),
                )

                if merged.empty:
                    logger.warning(
                        "No overlapping dates for intervention pairing: "
                        f"{management_file}"
                    )
                    continue

                valid = merged["value_climate_only"].notna()
                valid = valid & merged["value_management"].notna()
                valid = valid & (merged["value_climate_only"] != 0)

                invalid_count = len(merged) - int(valid.sum())
                if invalid_count > 0:
                    logger.warning(
                        f"Excluded {invalid_count} invalid paired rows: "
                        f"{management_file}"
                    )

                merged = merged[valid].copy()

                if merged.empty:
                    logger.warning(f"No valid paired rows for {management_file}")
                    continue

                out_df = pd.DataFrame()
                out_df["date"] = merged["date"]
                out_df["value"] = 100.0 * (
                    (merged["value_management"] / merged["value_climate_only"]) - 1.0
                )

                out_file = output_folder / base_file.name
                out_df.to_csv(out_file, index=False)

                logger.info(f"Wrote relative_climate_intervention series: {out_file}")


def write_relative_historical_intervention_time_series(
    case_folder: Path,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    for scenario in SCENARIO_MAP.values():
        scenario_id = scenario.value

        climate_management_folder = (
            case_folder / FolderTypeEnum.CLIMATE_MANAGEMENT.value / scenario_id
        )
        output_scenario_folder = (
            case_folder / "relative_historical_intervention" / scenario_id
        )

        if not climate_management_folder.exists():
            logger.warning(
                "Missing climate_management folder for historical comparison: "
                f"{climate_management_folder}"
            )
            continue

        for cadence_folder in climate_management_folder.iterdir():
            if not cadence_folder.is_dir():
                continue

            cadence = cadence_folder.name
            output_folder = output_scenario_folder / cadence
            output_folder.mkdir(parents=True, exist_ok=True)

            for management_file in cadence_folder.glob("*.csv"):
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
                        "No valid anchor values in historical reference window: "
                        f"{management_file}"
                    )
                    continue

                anchor = float(anchor_values.mean())

                if anchor == 0:
                    logger.warning(
                        "Historical anchor is zero; cannot compute relative series: "
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
                        "No valid rows left for relative_historical_intervention: "
                        f"{management_file}"
                    )
                    continue

                out_df = pd.DataFrame()
                out_df["date"] = valid_df["date"]
                out_df["value"] = 100.0 * ((valid_df["value"] / anchor) - 1.0)

                out_file = output_folder / management_file.name
                out_df.to_csv(out_file, index=False)

                logger.info(
                    f"Wrote relative_historical_intervention series: {out_file} "
                    f"(anchor={anchor}, years="
                    f"{historical_reference_start_year}-"
                    f"{historical_reference_end_year})"
                )


if __name__ == "__main__":
    standardise_ewe_ns(
        input_root=r"cs11_nw-med_ecospace/raw",
        output_root=r"cs11_nw-med_ecospace/for_analysis",
        case_study="cs11",
        area="NW-Med"
    )
