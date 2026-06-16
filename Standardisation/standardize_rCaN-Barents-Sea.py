from pathlib import Path
import logging
import pandas as pd
from os import path

from helpers.actnow_helpers import (
    IndicatorEnum,
    ScenarioEnum,
    FolderTypeEnum,
    detect_cadence_from_dates,
    INTERCOMPARISON_REFERENCE_PERIOD,
)


INDICATOR_COLUMN_MAP = {
    "total_biomass": IndicatorEnum.TOTAL_BIOMASS,
    "consumer_biomass": IndicatorEnum.CONSUMER_BIOMASS,
    "pelagic_biomass": IndicatorEnum.PELAGIC_BIOMASS,
    "demersal_biomass": IndicatorEnum.DEMERSAL_BIOMASS,
    "total_catch": IndicatorEnum.TOTAL_CATCH,
    "consumer_catch": IndicatorEnum.CONSUMER_CATCH,
    "pelagic_catch": IndicatorEnum.PELAGIC_CATCH,
    "demersal_catch": IndicatorEnum.DEMERSAL_CATCH,
    "demersal_pelagic_ratio": IndicatorEnum.DEMERSAL_PELAGIC_RATIO,
    "mean_trophic_level": IndicatorEnum.MEAN_TL,
    "catch_trophic_level": IndicatorEnum.CATCH_TL,
    "evenness_q": IndicatorEnum.EVENNESS_Q,
}


# Shared ActNow intercomparison reference period.
# Used to anchor all historical-relative products across models.
HISTORICAL_REFERENCE_YEARS = INTERCOMPARISON_REFERENCE_PERIOD


SCENARIO_MAP = {
    # Historical context only. Used for continuity checks, not as the
    # historical/control reference for relative products.
    "B-past": {
        "scenario_folder": "historical",
        "series_type": "historical_context",
        "is_historical": False,
    },

    # Baseline is the historical/control reference used for normalisation.
    # B-past is retained as historical context only and must not be used as
    # the historical baseline for relative products.
    "Baseline": {
        "scenario_folder": "baseline",
        "series_type": "reference",
        "is_historical": True,
    },
    "BaselineRieu": {
        "scenario_folder": "baseline-rieu",
        "series_type": "reference",
        "is_historical": False,
    },

    # Global Sustainability.
    "GS-b": {
        "scenario_folder": ScenarioEnum.GS,
        "series_type": FolderTypeEnum.CLIMATE_ONLY,
        "is_historical": False,
    },
    "GS-i": {
        "scenario_folder": ScenarioEnum.GS,
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "GS-b",
        "is_historical": False,
    },

    # Regional Rivalry. The hot-war and cold-war intervention variants are
    # model-specific canonical variants and are intentionally written as
    # separate scenario folders.
    "RR-b": {
        "scenario_folder": ScenarioEnum.RR,
        "series_type": FolderTypeEnum.CLIMATE_ONLY,
        "is_historical": False,
    },
    "RRCW-i": {
        "scenario_folder": "rr-cw",
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "RR-b",
        "is_historical": False,
    },
    "RRHW-i": {
        "scenario_folder": "rr-hw",
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "RR-b",
        "is_historical": False,
    },

    # World Markets.
    "WM-b": {
        "scenario_folder": ScenarioEnum.WM,
        "series_type": FolderTypeEnum.CLIMATE_ONLY,
        "is_historical": False,
    },
    "WM-i": {
        "scenario_folder": ScenarioEnum.WM,
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "WM-b",
        "is_historical": False,
    },
}

CANONICAL_SERIES_TYPES = {
    FolderTypeEnum.CLIMATE_ONLY.value,
    FolderTypeEnum.CLIMATE_MANAGEMENT.value,
}

RELATIVE_HISTORICAL_CLIMATE = "relative_historical_climate"
RELATIVE_CLIMATE_INTERVENTION = "relative_climate_intervention"
RELATIVE_HISTORICAL_INTERVENTION = "relative_historical_intervention"

# Phase-1 compatibility aliases. These duplicate the new canonical products
# under the legacy folder names expected by the current plotting utilities.
LEGACY_PRODUCT_ALIASES = {
    "relative_intervention": RELATIVE_CLIMATE_INTERVENTION,
    "relative_historical": RELATIVE_HISTORICAL_INTERVENTION,
}


def enum_value(value) -> str:
    if hasattr(value, "value"):
        return value.value

    return str(value)


def setup_logging(output_root: str | Path) -> logging.Logger:
    output_root = Path(output_root)
    output_root.mkdir(parents=True, exist_ok=True)

    log_file = output_root / "standardization.log"

    logger = logging.getLogger("actnow-standardizer")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")

    console_handler = logging.StreamHandler()
    console_handler.setFormatter(formatter)

    file_handler = logging.FileHandler(log_file, mode="w", encoding="utf-8")
    file_handler.setFormatter(formatter)

    logger.addHandler(console_handler)
    logger.addHandler(file_handler)

    return logger


def year_to_date(year: int) -> str:
    return f"{int(year)}-01-01"


def relative_percent(numerator: pd.Series, denominator: pd.Series) -> pd.Series:
    return 100.0 * ((numerator / denominator) - 1.0)


def check_historical_gap(df: pd.DataFrame, logger: logging.Logger) -> None:
    historical_mask = df["scenario"].map(lambda s: SCENARIO_MAP[s]["is_historical"])

    historical_df = df[historical_mask].copy()
    scenario_df = df[~historical_mask].copy()

    if historical_df.empty:
        logger.warning("No historical data found for continuity check.")
        return

    if scenario_df.empty:
        logger.warning("No scenario data found for continuity check.")
        return

    historical_last_year = int(historical_df["Year"].max())

    grouped = scenario_df.groupby(["scenario", "area", "model"])

    for (scenario, area, model), group_df in grouped:
        scenario_first_year = int(group_df["Year"].min())
        year_gap = scenario_first_year - historical_last_year

        if year_gap > 1:
            logger.warning(
                f"Gap detected between historical and scenario data: "
                f"{scenario} / {area} / {model} "
                f"(historical ends {historical_last_year}, "
                f"scenario starts {scenario_first_year}, "
                f"gap = {year_gap} years)"
            )


def normalise_rcan_output(
    input_csv: str | Path,
    output_root: str | Path,
    historical_reference_years: list[int] | None = None,
    skip_all_na: bool = True,
) -> None:
    input_csv = Path(input_csv)
    output_root = Path(output_root)

    if historical_reference_years is None:
        historical_reference_years = HISTORICAL_REFERENCE_YEARS

    logger = setup_logging(output_root)

    logger.info(f"Reading input CSV: {input_csv}")
    logger.info(f"Historical reference years: {historical_reference_years}")

    df = pd.read_csv(input_csv)

    required = {"scenario", "Year", "area", "model"}
    missing = required - set(df.columns)

    if missing:
        msg = f"Missing required columns: {sorted(missing)}"
        logger.error(msg)
        raise ValueError(msg)

    unknown_scenarios = sorted(set(df["scenario"]) - set(SCENARIO_MAP))

    if unknown_scenarios:
        msg = "Unmapped scenarios found. Add these to SCENARIO_MAP: "
        msg += ", ".join(unknown_scenarios)
        logger.error(msg)
        raise ValueError(msg)

    available_indicators = []

    for source_column in INDICATOR_COLUMN_MAP:
        if source_column in df.columns:
            available_indicators.append(source_column)

    if not available_indicators:
        msg = "No recognised indicator columns found."
        logger.error(msg)
        raise ValueError(msg)

    logger.info(f"Recognised indicators: {', '.join(available_indicators)}")

    check_historical_gap(df=df, logger=logger)

    write_absolute_time_series(
        df=df,
        output_root=output_root,
        available_indicators=available_indicators,
        skip_all_na=skip_all_na,
        logger=logger,
    )

    write_relative_climate_intervention_time_series(
        output_root=output_root,
        available_indicators=available_indicators,
        logger=logger,
    )

    write_relative_historical_time_series(
        df=df,
        output_root=output_root,
        available_indicators=available_indicators,
        historical_reference_years=historical_reference_years,
        logger=logger,
    )

    logger.info("Standardisation complete.")


def write_absolute_time_series(
    df: pd.DataFrame,
    output_root: Path,
    available_indicators: list[str],
    skip_all_na: bool,
    logger: logging.Logger,
) -> None:

    for scenario, scenario_df in df.groupby("scenario"):
        scenario_info = SCENARIO_MAP[scenario]
        scenario_folder = enum_value(scenario_info["scenario_folder"])
        series_type = enum_value(scenario_info["series_type"])

        if series_type not in CANONICAL_SERIES_TYPES:
            logger.info(f"Skipping non-canonical scenario: {scenario}")
            continue

        for area, area_df in scenario_df.groupby("area"):
            for model, model_df in area_df.groupby("model"):
                for indicator in available_indicators:
                    indicator_name = enum_value(INDICATOR_COLUMN_MAP[indicator])

                    out_df = model_df[["Year", indicator]].copy()
                    out_df = out_df.rename(columns={indicator: "value"})

                    if skip_all_na and out_df["value"].isna().all():
                        logger.info(
                            f"Skipping all-NA indicator: "
                            f"{scenario} / {area} / {model} / {indicator}"
                        )
                        continue

                    out_df["date"] = out_df["Year"].map(year_to_date)
                    out_df = out_df[["date", "value"]]
                    out_df = out_df.sort_values("date")

                    cadence = detect_cadence_from_dates(out_df)
                    folder = output_root / series_type / scenario_folder / cadence
                    folder.mkdir(parents=True, exist_ok=True)

                    out_file = folder / f"{indicator_name}.csv"
                    out_df.to_csv(out_file, index=False)

                    logger.info(f"Wrote absolute series: {out_file}")


def write_relative_climate_intervention_time_series(
    output_root: Path,
    available_indicators: list[str],
    logger: logging.Logger,
) -> None:
    for scenario_name, scenario_info in SCENARIO_MAP.items():
        if enum_value(scenario_info["series_type"]) != FolderTypeEnum.CLIMATE_MANAGEMENT.value:
            continue

        paired_scenario_name = scenario_info["paired_with"]
        paired_info = SCENARIO_MAP[paired_scenario_name]

        management_folder_name = enum_value(scenario_info["scenario_folder"])
        climate_only_folder_name = enum_value(paired_info["scenario_folder"])

        climate_management_root = (
            output_root / FolderTypeEnum.CLIMATE_MANAGEMENT.value / management_folder_name
        )

        climate_only_root = (
            output_root / FolderTypeEnum.CLIMATE_ONLY.value / climate_only_folder_name
        )

        if not climate_management_root.exists():
            logger.warning(
                f"Missing climate_management folder: {climate_management_root}"
            )
            continue

        if not climate_only_root.exists():
            logger.warning(
                f"Missing climate_only folder for pairing: {climate_only_root}"
            )
            continue

        for management_cadence_folder in sorted(climate_management_root.iterdir()):
            if not management_cadence_folder.is_dir():
                continue

            cadence = management_cadence_folder.name
            climate_only_folder = climate_only_root / cadence

            if not climate_only_folder.exists():
                logger.warning(
                    f"Missing climate_only cadence folder for pairing: "
                    f"{climate_only_folder}"
                )
                continue

            for indicator in available_indicators:
                indicator_name = enum_value(INDICATOR_COLUMN_MAP[indicator])
                management_file = management_cadence_folder / f"{indicator_name}.csv"
                climate_file = climate_only_folder / f"{indicator_name}.csv"

                if not management_file.exists():
                    logger.warning(f"Missing climate_management file: {management_file}")
                    continue

                if not climate_file.exists():
                    logger.warning(f"Missing climate_only file: {climate_file}")
                    continue

                management_df = pd.read_csv(management_file)
                climate_df = pd.read_csv(climate_file)

                merged = pd.merge(
                    management_df,
                    climate_df,
                    on="date",
                    how="inner",
                    suffixes=("_management", "_climate_only"),
                )

                if merged.empty:
                    logger.warning(
                        f"No overlapping dates for intervention pairing: "
                        f"{management_folder_name} / {cadence} / {indicator_name}"
                    )
                    continue

                missing_management_dates = set(management_df["date"]) - set(merged["date"])
                missing_climate_dates = set(climate_df["date"]) - set(merged["date"])

                if missing_management_dates:
                    logger.warning(
                        f"Dropped management dates without climate_only pair: "
                        f"{management_folder_name} / {cadence} / {indicator_name}: "
                        f"{sorted(missing_management_dates)}"
                    )

                if missing_climate_dates:
                    logger.warning(
                        f"Dropped climate_only dates without management pair: "
                        f"{management_folder_name} / {cadence} / {indicator_name}: "
                        f"{sorted(missing_climate_dates)}"
                    )

                valid = merged["value_climate_only"].notna()
                valid = valid & merged["value_management"].notna()
                valid = valid & (merged["value_climate_only"] != 0)

                if not valid.all():
                    logger.warning(
                        f"Excluded {len(merged) - valid.sum()} invalid paired rows "
                        f"because of NA or zero climate_only values: "
                        f"{management_folder_name} / {cadence} / {indicator_name}"
                    )

                merged = merged[valid].copy()

                if merged.empty:
                    logger.warning(
                        f"No valid rows left after filtering: "
                        f"{management_folder_name} / {cadence} / {indicator_name}"
                    )
                    continue

                out_df = pd.DataFrame()
                out_df["date"] = merged["date"]
                out_df["value"] = relative_percent(
                    merged["value_management"],
                    merged["value_climate_only"],
                )

                write_relative_dataframe(
                    out_df=out_df,
                    output_root=output_root,
                    product_name=RELATIVE_CLIMATE_INTERVENTION,
                    scenario_folder=management_folder_name,
                    indicator_name=indicator_name,
                    logger=logger,
                )


def get_historical_anchor_rows(
    df: pd.DataFrame,
    area: str,
    model: str,
    historical_reference_years: list[int],
) -> pd.DataFrame:
    historical_scenarios = []

    for scenario_name, scenario_info in SCENARIO_MAP.items():
        if scenario_info["is_historical"]:
            historical_scenarios.append(scenario_name)

    return df[
        (df["scenario"].isin(historical_scenarios))
        & (df["area"] == area)
        & (df["model"] == model)
        & (df["Year"].isin(historical_reference_years))
    ].copy()


def write_relative_dataframe(
    out_df: pd.DataFrame,
    output_root: Path,
    product_name: str,
    scenario_folder: str,
    indicator_name: str,
    logger: logging.Logger,
) -> None:
    out_df = out_df.sort_values("date")
    cadence = detect_cadence_from_dates(out_df)

    output_folder = output_root / product_name / scenario_folder / cadence
    output_folder.mkdir(parents=True, exist_ok=True)

    out_file = output_folder / f"{indicator_name}.csv"
    out_df.to_csv(out_file, index=False)

    logger.info(f"Wrote {product_name} time series: {out_file}")

    for legacy_product_name, canonical_product_name in LEGACY_PRODUCT_ALIASES.items():
        if canonical_product_name != product_name:
            continue

        legacy_folder = output_root / legacy_product_name / scenario_folder / cadence
        legacy_folder.mkdir(parents=True, exist_ok=True)

        legacy_file = legacy_folder / f"{indicator_name}.csv"
        out_df.to_csv(legacy_file, index=False)

        logger.info(
            f"Wrote compatibility alias for {product_name}: {legacy_file}"
        )


def write_relative_historical_time_series(
    df: pd.DataFrame,
    output_root: Path,
    available_indicators: list[str],
    historical_reference_years: list[int],
    logger: logging.Logger,
) -> None:
    """
    Write explicit relative products against the historical/control Baseline.

    Products written by this function:

    - relative_historical_climate:
      climate-only / historical endpoint

    - relative_historical_intervention:
      climate+management / historical endpoint

    During Phase 1, relative_historical_intervention is also written to the
    legacy relative_historical folder for backwards compatibility.
    """
    grouped = df.groupby(["scenario", "area", "model"])

    for (scenario, area, model), scenario_df in grouped:
        scenario_info = SCENARIO_MAP[scenario]
        series_type = enum_value(scenario_info["series_type"])

        if series_type == FolderTypeEnum.CLIMATE_ONLY.value:
            product_name = RELATIVE_HISTORICAL_CLIMATE
        elif series_type == FolderTypeEnum.CLIMATE_MANAGEMENT.value:
            product_name = RELATIVE_HISTORICAL_INTERVENTION
        else:
            continue

        scenario_folder = enum_value(scenario_info["scenario_folder"])

        anchor_df = get_historical_anchor_rows(
            df=df,
            area=area,
            model=model,
            historical_reference_years=historical_reference_years,
        )

        if anchor_df.empty:
            logger.warning(
                f"Missing Baseline historical anchor rows for {area} / {model}; "
                f"skipping {product_name} / {scenario_folder}."
            )
            continue

        for indicator in available_indicators:
            indicator_name = enum_value(INDICATOR_COLUMN_MAP[indicator])
            anchor_values = anchor_df[indicator].dropna()

            if anchor_values.empty:
                logger.warning(
                    f"Missing Baseline historical anchor value for {area} / "
                    f"{model} / {indicator_name}; skipping {product_name} / "
                    f"{scenario_folder}."
                )
                continue

            anchor_value = anchor_values.mean()

            if anchor_value == 0:
                logger.warning(
                    f"Baseline historical anchor is zero for {area} / {model} / "
                    f"{indicator_name}; skipping {product_name} / "
                    f"{scenario_folder}."
                )
                continue

            out_df = scenario_df[["Year", indicator]].copy()
            out_df = out_df.rename(columns={indicator: "value"})

            valid = out_df["value"].notna()

            if not valid.all():
                logger.warning(
                    f"Excluded {len(out_df) - valid.sum()} rows with NA values "
                    f"from {product_name}: {scenario_folder} / {indicator_name}"
                )

            out_df = out_df[valid].copy()

            if out_df.empty:
                logger.warning(
                    f"No valid rows left for {product_name}: "
                    f"{scenario_folder} / {indicator_name}"
                )
                continue

            out_df["date"] = out_df["Year"].map(year_to_date)
            out_df["value"] = relative_percent(out_df["value"], anchor_value)
            out_df = out_df[["date", "value"]]

            write_relative_dataframe(
                out_df=out_df,
                output_root=output_root,
                product_name=product_name,
                scenario_folder=scenario_folder,
                indicator_name=indicator_name,
                logger=logger,
            )

            logger.info(
                f"{product_name} anchor: scenario=Baseline, "
                f"years={historical_reference_years}, value={anchor_value}"
            )


if __name__ == "__main__":
    root = "cs01_barents-sea_rcan"

    normalise_rcan_output(
        input_csv=path.join(root, "RCaN_output_4_ACTNOW.csv"),
        output_root=path.join(root, "for_analysis"),
        historical_reference_years=HISTORICAL_REFERENCE_YEARS,
    )
