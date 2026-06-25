from pathlib import Path
import logging
import shutil
import pandas as pd

from helpers.actnow_helpers import (
    IndicatorEnum,
    ScenarioEnum,
    FolderTypeEnum,
    detect_cadence_from_dates,
    INTERCOMPARISON_REFERENCE_PERIOD,
)

# Shared ActNow intercomparison reference period.
# Used to anchor all historical-relative products across models.
HISTORICAL_REFERENCE_START_YEAR = INTERCOMPARISON_REFERENCE_PERIOD[0]
HISTORICAL_REFERENCE_END_YEAR =  INTERCOMPARISON_REFERENCE_PERIOD[1]


INDICATOR_MAP = {
    "total_biomass": IndicatorEnum.TOTAL_BIOMASS,
    "consumer_biomass": IndicatorEnum.CONSUMER_BIOMASS,
    "pelagic_fish_biomass": IndicatorEnum.PELAGIC_BIOMASS,
    "demersal_fish_biomass": IndicatorEnum.DEMERSAL_BIOMASS,
    "phytoplankton_biomass": IndicatorEnum.PHYTOPLANKTON_BIOMASS,
    "zooplankton_biomass": IndicatorEnum.ZOOPLANKTON_BIOMASS,
    "fish_biomass": IndicatorEnum.FISH_BIOMASS,
    "total_catch": IndicatorEnum.TOTAL_CATCH,
    "total_consumer_catch": IndicatorEnum.CONSUMER_CATCH,
    "total_pelagic_fish_catch": IndicatorEnum.PELAGIC_CATCH,
    "total_demersal_fish_catch": IndicatorEnum.DEMERSAL_CATCH,
    "mtl_catch": IndicatorEnum.CATCH_TL,
    "mtl": IndicatorEnum.MEAN_TL,
    "kempton_Q": IndicatorEnum.EVENNESS_Q,
    "benthic_inv_biomass": IndicatorEnum.BENTHIC_INVERTEBRATES_BIOMASS,
}


CLIMATE_ONLY_SCENARIO_MAP = {
    "Baseline_RCP2.6": [ScenarioEnum.GS],
    "Baseline_RCP4.5": [ScenarioEnum.IQ],
    "Baseline_RCP8.5": [ScenarioEnum.RR, ScenarioEnum.WM],
}


CLIMATE_MANAGEMENT_SCENARIO_MAP = {
    "Int_Sustainability": ScenarioEnum.GS,
    "Int_Inequality": ScenarioEnum.IQ,
    "Int_RegionalRivalry": ScenarioEnum.RR,
    "Int_WorldMarkets": ScenarioEnum.WM,
}


CANONICAL_SCENARIOS = [
    ScenarioEnum.GS,
    ScenarioEnum.IQ,
    ScenarioEnum.RR,
    ScenarioEnum.WM,
]


def setup_logging(output_root: str | Path) -> logging.Logger:
    output_root = Path(output_root)
    output_root.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger("norwegian-fjord-standardizer")
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


def standardise_fjord_csv(
    input_csv: str | Path,
    output_root: str | Path,
    case_study: str,
    area: str,
    model: str = "ewe-ecosim",
    historical_reference_start_year: int = HISTORICAL_REFERENCE_START_YEAR,
    historical_reference_end_year: int = HISTORICAL_REFERENCE_END_YEAR,
) -> None:
    input_csv = Path(input_csv)
    output_root = Path(output_root)

    logger = setup_logging(output_root)

    if historical_reference_start_year > historical_reference_end_year:
        msg = (
            "historical_reference_start_year must be <= "
            "historical_reference_end_year"
        )
        logger.error(msg)
        raise ValueError(msg)

    logger.info(f"Reading {input_csv}")
    logger.info(
        "Historical reference window: "
        f"{historical_reference_start_year}-{historical_reference_end_year}"
    )

    df = pd.read_csv(input_csv)

    required = {"scenario", "indicator", "year", "value"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {sorted(missing)}")

    known_scenarios = set(CLIMATE_ONLY_SCENARIO_MAP)
    known_scenarios = known_scenarios | set(CLIMATE_MANAGEMENT_SCENARIO_MAP)
    unknown_scenarios = sorted(set(df["scenario"]) - known_scenarios)
    if unknown_scenarios:
        raise ValueError(f"Unknown scenarios: {unknown_scenarios}")

    write_absolute_series(
        df=df,
        scenario_map=CLIMATE_ONLY_SCENARIO_MAP,
        series_type=FolderTypeEnum.CLIMATE_ONLY,
        case_folder=output_root,
        logger=logger,
    )

    write_absolute_series(
        df=df,
        scenario_map=CLIMATE_MANAGEMENT_SCENARIO_MAP,
        series_type=FolderTypeEnum.CLIMATE_MANAGEMENT,
        case_folder=output_root,
        logger=logger,
    )

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

    logger.info(f"{case_study} {area} standardisation complete.")


def write_absolute_series(
    df: pd.DataFrame,
    scenario_map: dict,
    series_type: FolderTypeEnum,
    case_folder: Path,
    logger: logging.Logger,
) -> None:
    for scenario_name, output_scenarios in scenario_map.items():
        if isinstance(output_scenarios, ScenarioEnum):
            output_scenarios = [output_scenarios]

        scenario_df = df[df["scenario"] == scenario_name]
        if scenario_df.empty:
            logger.warning(f"No rows found for scenario: {scenario_name}")
            continue

        for output_scenario in output_scenarios:
            for source_indicator, canonical_indicator in INDICATOR_MAP.items():
                indicator_df = scenario_df[
                    scenario_df["indicator"] == source_indicator
                ].copy()
                if indicator_df.empty:
                    logger.info(
                        f"Missing indicator: {scenario_name} / {source_indicator}"
                    )
                    continue

                out_df = pd.DataFrame()
                out_df["date"] = indicator_df["year"].astype(int).astype(str) + "-01-01"
                out_df["value"] = pd.to_numeric(
                    indicator_df["value"],
                    errors="coerce",
                )
                out_df = out_df.dropna(subset=["date", "value"])
                out_df = out_df.sort_values("date")

                if out_df.empty:
                    logger.warning(
                        f"No valid rows for {scenario_name} / {source_indicator}"
                    )
                    continue

                cadence = detect_cadence_from_dates(out_df)
                target_folder = (
                    case_folder
                    / series_type.value
                    / output_scenario.value
                    / cadence
                )
                target_folder.mkdir(parents=True, exist_ok=True)

                out_file = target_folder / f"{canonical_indicator.value}.csv"
                out_df.to_csv(out_file, index=False)

                logger.info(f"Wrote absolute series: {out_file}")


def write_relative_climate_intervention_time_series(
    case_folder: Path,
    logger: logging.Logger,
) -> None:
    for scenario in CANONICAL_SCENARIOS:
        climate_only_scenario_folder = (
            case_folder / FolderTypeEnum.CLIMATE_ONLY.value / scenario.value
        )
        climate_management_scenario_folder = (
            case_folder / FolderTypeEnum.CLIMATE_MANAGEMENT.value / scenario.value
        )

        if not climate_only_scenario_folder.exists():
            logger.warning(
                f"Missing climate_only folder: {climate_only_scenario_folder}"
            )
            continue

        if not climate_management_scenario_folder.exists():
            logger.warning(
                "Missing climate_management folder: "
                f"{climate_management_scenario_folder}"
            )
            continue

        for climate_only_cadence_folder in climate_only_scenario_folder.iterdir():
            if not climate_only_cadence_folder.is_dir():
                continue

            cadence = climate_only_cadence_folder.name
            climate_management_cadence_folder = (
                climate_management_scenario_folder / cadence
            )
            output_folder = (
                case_folder / "relative_climate_intervention" / scenario.value / cadence
            )

            if not climate_management_cadence_folder.exists():
                logger.warning(
                    "Missing climate_management cadence folder: "
                    f"{climate_management_cadence_folder}"
                )
                continue

            output_folder.mkdir(parents=True, exist_ok=True)

            for climate_only_file in climate_only_cadence_folder.glob("*.csv"):
                management_file = (
                    climate_management_cadence_folder / climate_only_file.name
                )

                if not management_file.exists():
                    logger.warning(
                        f"Missing paired intervention file: {management_file}"
                    )
                    continue

                write_relative_pair(
                    numerator_file=management_file,
                    denominator_file=climate_only_file,
                    out_file=output_folder / climate_only_file.name,
                    logger=logger,
                    context=(
                        "relative_climate_intervention / "
                        f"{scenario.value} / {cadence}"
                    ),
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


def write_relative_historical_intervention_time_series(
    case_folder: Path,
    historical_reference_start_year: int,
    historical_reference_end_year: int,
    logger: logging.Logger,
) -> None:
    write_relative_to_own_historical_anchor(
        case_folder=case_folder,
        source_product=FolderTypeEnum.CLIMATE_MANAGEMENT.value,
        output_product="relative_historical_intervention",
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
    for scenario in CANONICAL_SCENARIOS:
        source_scenario_folder = case_folder / source_product / scenario.value
        output_scenario_folder = case_folder / output_product / scenario.value

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



if __name__ == "__main__":
    standardise_fjord_csv(
        input_csv="cs02_norwegian-fjord_ewe/raw/indicator_time_series_porsangerfjord.csv",
        output_root="cs02_norwegian-fjord_ewe/for_analysis",
        case_study="cs02",
        area="norwegian-fjord",
        model="ewe-ecosim",
    )
