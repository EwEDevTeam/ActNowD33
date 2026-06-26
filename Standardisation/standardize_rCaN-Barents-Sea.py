from pathlib import Path
import logging
import pandas as pd
from os import path

from helpers.actnow_helpers import (
    IndicatorEnum,
    ScenarioEnum,
    FolderTypeEnum,
    detect_cadence_from_dates,
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


# rCaN does not represent the full lower trophic spectrum used by some other MEMs.
# Benjamin Planque confirmed that, for this model output, consumer_biomass and
# total_biomass should be treated as equivalent ActNow indicators.
#
# The raw rCaN file therefore does not necessarily contain a total_biomass
# column. This adapter creates one from consumer_biomass before the list of
# available indicators is detected, so total_biomass is written to all absolute
# and relative products exactly like any other indicator.
TOTAL_BIOMASS_FALLBACK_SOURCE = "consumer_biomass"


CONTROL_SCENARIO = "Baseline"


SCENARIO_MAP = {
    # Historical context only. Used for continuity checks / inspection only.
    # It is not used as a reference for relative products.
    "B-past": {
        "scenario_folder": "historical",
        "series_type": "historical_context",
    },

    # Benjamin Planque requested that rCaN scenarios be compared to the
    # corresponding years of the Baseline scenario, rather than to a fixed
    # historical reference period. In the common ActNow terminology this is
    # therefore treated as a relative_control_* product.
    #
    # Note that the rCaN scenario named "Baseline" is not used here as a
    # historical baseline slice. It is a year-matched control trajectory.
    CONTROL_SCENARIO: {
        "scenario_folder": "control",
        "series_type": "control",
    },

    "BaselineRieu": {
        "scenario_folder": "baseline-rieu",
        "series_type": "reference",
    },

    # Global Sustainability.
    "GS-b": {
        "scenario_folder": ScenarioEnum.GS,
        "series_type": FolderTypeEnum.CLIMATE_ONLY,
    },
    "GS-i": {
        "scenario_folder": ScenarioEnum.GS,
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "GS-b",
    },

    # Regional Rivalry. The hot-war and cold-war intervention variants are
    # model-specific canonical variants and are intentionally written as
    # separate scenario folders.
    "RR-b": {
        "scenario_folder": ScenarioEnum.RR,
        "series_type": FolderTypeEnum.CLIMATE_ONLY,
    },
    "RRCW-i": {
        "scenario_folder": "rr-cw",
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "RR-b",
    },
    "RRHW-i": {
        "scenario_folder": "rr-hw",
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "RR-b",
    },

    # World Markets.
    "WM-b": {
        "scenario_folder": ScenarioEnum.WM,
        "series_type": FolderTypeEnum.CLIMATE_ONLY,
    },
    "WM-i": {
        "scenario_folder": ScenarioEnum.WM,
        "series_type": FolderTypeEnum.CLIMATE_MANAGEMENT,
        "paired_with": "WM-b",
    },
}


CANONICAL_SERIES_TYPES = {
    FolderTypeEnum.CLIMATE_ONLY.value,
    FolderTypeEnum.CLIMATE_MANAGEMENT.value,
}

RELATIVE_CLIMATE_INTERVENTION = "relative_climate_intervention"
RELATIVE_CONTROL_CLIMATE = "relative_control_climate"
RELATIVE_CONTROL_INTERVENTION = "relative_control_intervention"


def enum_value(value) -> str:
    if hasattr(value, "value"):
        return value.value

    return str(value)


def setup_logging(output_root: str | Path) -> logging.Logger:
    output_root = Path(output_root)
    output_root.mkdir(parents=True, exist_ok=True)

    log_file = output_root / "standardization.log"

    logger = logging.getLogger("actnow-rcan-standardizer")
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


def ensure_total_biomass_indicator(
    df: pd.DataFrame,
    logger: logging.Logger,
) -> pd.DataFrame:
    """Ensure that rCaN provides the ActNow total_biomass indicator.

    rCaN does not provide an independent total_biomass estimate because, for
    this model output, total biomass and consumer biomass are equivalent.
    Benjamin Planque confirmed that consumer_biomass should therefore be used
    as the ActNow total_biomass indicator.

    This is applied to the full input table, including the Baseline control
    trajectory, before available indicators are detected and before relative
    products are calculated.
    """

    if TOTAL_BIOMASS_FALLBACK_SOURCE not in df.columns:
        logger.warning(
            "Input file does not contain consumer_biomass. "
            "The total_biomass indicator cannot be derived for rCaN."
        )
        return df

    df = df.copy()

    if "total_biomass" in df.columns:
        missing_before = df["total_biomass"].isna().sum()

        # For rCaN, total_biomass is defined as consumer_biomass. Overwrite
        # rather than only filling gaps to avoid mixed provenance.
        df["total_biomass"] = df[TOTAL_BIOMASS_FALLBACK_SOURCE]

        logger.info(
            "Replaced total_biomass with consumer_biomass for all rCaN rows. "
            f"Missing total_biomass values before replacement: {missing_before}."
        )
    else:
        df["total_biomass"] = df[TOTAL_BIOMASS_FALLBACK_SOURCE]

        logger.info(
            "Added total_biomass from consumer_biomass for all rCaN rows."
        )

    return df


def normalise_rcan_output(
    input_csv: str | Path,
    output_root: str | Path,
    skip_all_na: bool = True,
    validation_csv: str | Path | None = None,
) -> None:
    input_csv = Path(input_csv)
    output_root = Path(output_root)

    logger = setup_logging(output_root)

    logger.info(f"Reading input CSV: {input_csv}")
    logger.info(
        "rCaN uses a year-matched control trajectory rather than a fixed "
        "historical reference period. Writing relative_control_* products "
        "instead of relative_historical_* products."
    )

    df = pd.read_csv(input_csv)

    required = {"scenario", "Year", "area", "model"}
    missing = required - set(df.columns)

    if missing:
        msg = f"Missing required columns: {sorted(missing)}"
        logger.error(msg)
        raise ValueError(msg)

    df = ensure_total_biomass_indicator(df=df, logger=logger)

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

    write_relative_control_time_series(
        df=df,
        output_root=output_root,
        available_indicators=available_indicators,
        logger=logger,
    )

    if validation_csv is not None:
        validate_against_expected_output(
            validation_csv=validation_csv,
            output_root=output_root,
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
            output_root
            / FolderTypeEnum.CLIMATE_MANAGEMENT.value
            / management_folder_name
        )

        climate_only_root = (
            output_root
            / FolderTypeEnum.CLIMATE_ONLY.value
            / climate_only_folder_name
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


def write_relative_control_time_series(
    df: pd.DataFrame,
    output_root: Path,
    available_indicators: list[str],
    logger: logging.Logger,
) -> None:
    control_df = df[df["scenario"] == CONTROL_SCENARIO].copy()

    if control_df.empty:
        logger.warning(
            f"Missing rCaN control scenario '{CONTROL_SCENARIO}'. "
            "Cannot write relative_control products."
        )
        return

    grouped = df.groupby(["scenario", "area", "model"])

    for (scenario, area, model), scenario_df in grouped:
        scenario_info = SCENARIO_MAP[scenario]
        series_type = enum_value(scenario_info["series_type"])

        if series_type == FolderTypeEnum.CLIMATE_ONLY.value:
            product_name = RELATIVE_CONTROL_CLIMATE
        elif series_type == FolderTypeEnum.CLIMATE_MANAGEMENT.value:
            product_name = RELATIVE_CONTROL_INTERVENTION
        else:
            continue

        scenario_folder = enum_value(scenario_info["scenario_folder"])

        matching_control_df = control_df[
            (control_df["area"] == area)
            & (control_df["model"] == model)
        ].copy()

        if matching_control_df.empty:
            logger.warning(
                f"Missing control rows for {area} / {model}; "
                f"skipping {product_name} / {scenario_folder}."
            )
            continue

        for indicator in available_indicators:
            indicator_name = enum_value(INDICATOR_COLUMN_MAP[indicator])

            scenario_values = scenario_df[["Year", indicator]].copy()
            scenario_values = scenario_values.rename(
                columns={indicator: "value_scenario"}
            )

            control_values = matching_control_df[["Year", indicator]].copy()
            control_values = control_values.rename(
                columns={indicator: "value_control"}
            )

            merged = pd.merge(
                scenario_values,
                control_values,
                on="Year",
                how="inner",
            )

            if merged.empty:
                logger.warning(
                    f"No overlapping years for control comparison: "
                    f"{scenario} / {area} / {model} / {indicator_name}"
                )
                continue

            missing_scenario_years = set(scenario_values["Year"]) - set(merged["Year"])
            missing_control_years = set(control_values["Year"]) - set(merged["Year"])

            if missing_scenario_years:
                logger.warning(
                    f"Dropped scenario years without control pair: "
                    f"{scenario_folder} / {indicator_name}: "
                    f"{sorted(missing_scenario_years)}"
                )

            if missing_control_years:
                logger.warning(
                    f"Dropped control years without scenario pair: "
                    f"{scenario_folder} / {indicator_name}: "
                    f"{sorted(missing_control_years)}"
                )

            valid = merged["value_control"].notna()
            valid = valid & merged["value_scenario"].notna()
            valid = valid & (merged["value_control"] != 0)

            if not valid.all():
                logger.warning(
                    f"Excluded {len(merged) - valid.sum()} invalid control-paired rows "
                    f"because of NA or zero control values: "
                    f"{scenario_folder} / {indicator_name}"
                )

            merged = merged[valid].copy()

            if merged.empty:
                logger.warning(
                    f"No valid rows left for control comparison: "
                    f"{scenario_folder} / {indicator_name}"
                )
                continue

            out_df = pd.DataFrame()
            out_df["date"] = merged["Year"].map(year_to_date)
            out_df["value"] = relative_percent(
                merged["value_scenario"],
                merged["value_control"],
            )
            out_df = out_df.sort_values("date")

            write_relative_dataframe(
                out_df=out_df,
                output_root=output_root,
                product_name=product_name,
                scenario_folder=scenario_folder,
                indicator_name=indicator_name,
                logger=logger,
            )

            logger.info(
                f"{product_name} control comparison: "
                f"scenario={scenario}, control={CONTROL_SCENARIO}, "
                f"area={area}, model={model}, indicator={indicator_name}"
            )


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


def parse_validation_percent(value) -> float | None:
    if pd.isna(value):
        return None

    text = str(value).strip()

    if not text:
        return None

    if text.endswith("%"):
        text = text[:-1].strip()

    try:
        return float(text)
    except ValueError:
        return None


def parse_period_years(period_value) -> tuple[int, int] | None:
    if pd.isna(period_value):
        return None

    text = str(period_value).strip()

    if "-" not in text:
        try:
            year = int(float(text))
            return year, year
        except ValueError:
            return None

    parts = text.split("-")

    if len(parts) != 2:
        return None

    try:
        start_year = int(parts[0])
        end_year = int(parts[1])
        return start_year, end_year
    except ValueError:
        return None


def expected_product_and_scenario(raw_scenario: str) -> tuple[str, str] | None:
    if raw_scenario not in SCENARIO_MAP:
        return None

    info = SCENARIO_MAP[raw_scenario]
    series_type = enum_value(info["series_type"])
    scenario_folder = enum_value(info["scenario_folder"])

    if series_type == FolderTypeEnum.CLIMATE_ONLY.value:
        return RELATIVE_CONTROL_CLIMATE, scenario_folder

    if series_type == FolderTypeEnum.CLIMATE_MANAGEMENT.value:
        return RELATIVE_CONTROL_INTERVENTION, scenario_folder

    return None


def validate_against_expected_output(
    validation_csv: str | Path,
    output_root: Path,
    logger: logging.Logger,
    tolerance_percentage_points: float = 1.0,
) -> None:
    """Validate rCaN relative-control products against Benjamin's summary CSV.

    The validation CSV currently contains an absolute block and a
    "Relative to baseline" block. This validator uses only the latter and
    checks the mean of the standardised relative-control output over the
    period stated in the CSV, allowing for rounding in the provided values.
    """

    validation_csv = Path(validation_csv)

    if not validation_csv.exists():
        logger.warning(f"Validation CSV not found: {validation_csv}")
        return

    logger.info(f"Validating against expected output: {validation_csv}")

    expected = pd.read_csv(validation_csv)
    relative_header_rows = expected.index[
        expected["scenario"].astype(str).str.strip().str.lower()
        == "relative to baseline"
    ].tolist()

    if not relative_header_rows:
        logger.warning(
            "Validation CSV does not contain a 'Relative to baseline' block."
        )
        return

    relative_start = relative_header_rows[0] + 1
    relative_rows = expected.iloc[relative_start:].copy()
    relative_rows = relative_rows.dropna(subset=["scenario", "Year"], how="any")

    checked = 0
    failed = 0

    for _, row in relative_rows.iterrows():
        raw_scenario = str(row["scenario"]).strip()

        if raw_scenario.lower() == "baseline":
            continue

        mapped = expected_product_and_scenario(raw_scenario)

        if mapped is None:
            logger.warning(
                f"Skipping validation row for unmapped/non-canonical scenario: "
                f"{raw_scenario}"
            )
            continue

        product_name, scenario_folder = mapped
        period = parse_period_years(row["Year"])

        if period is None:
            logger.warning(
                f"Skipping validation row with unparseable period: "
                f"{raw_scenario} / {row['Year']}"
            )
            continue

        start_year, end_year = period

        for source_indicator, indicator_enum in INDICATOR_COLUMN_MAP.items():
            if source_indicator not in relative_rows.columns:
                continue

            expected_value = parse_validation_percent(row.get(source_indicator))

            if expected_value is None:
                continue

            indicator_name = enum_value(indicator_enum)
            product_root = output_root / product_name / scenario_folder

            candidate_files = sorted(product_root.glob(f"*/{indicator_name}.csv"))

            if not candidate_files:
                logger.warning(
                    f"Validation missing standardised file: "
                    f"{product_name} / {scenario_folder} / {indicator_name}"
                )
                continue

            # rCaN is annual, but use the first cadence found to keep the
            # validator robust to future cadence changes.
            output_file = candidate_files[0]
            out_df = pd.read_csv(output_file)
            out_df["year"] = pd.to_datetime(out_df["date"]).dt.year
            period_df = out_df[
                (out_df["year"] >= start_year)
                & (out_df["year"] <= end_year)
            ]

            if period_df.empty:
                logger.warning(
                    f"Validation period not present in {output_file}: "
                    f"{start_year}-{end_year}"
                )
                continue

            actual_value = float(period_df["value"].mean())
            difference = actual_value - expected_value
            checked += 1

            if abs(difference) > tolerance_percentage_points:
                failed += 1
                logger.warning(
                    f"Validation mismatch: {raw_scenario} / {indicator_name} / "
                    f"{start_year}-{end_year}: expected {expected_value:.3f} %, "
                    f"actual {actual_value:.3f} %, diff {difference:.3f} pp"
                )
            else:
                logger.info(
                    f"Validation OK: {raw_scenario} / {indicator_name} / "
                    f"{start_year}-{end_year}: expected {expected_value:.3f} %, "
                    f"actual {actual_value:.3f} %"
                )

    logger.info(
        f"Validation complete: {checked} checks, {failed} mismatches "
        f"> {tolerance_percentage_points} percentage points."
    )


if __name__ == "__main__":
    root = "cs01_barents-sea_rcan"

    normalise_rcan_output(
        input_csv=path.join(root, "raw", "RCaN_output_4_ACTNOW.csv"),
        output_root=path.join(root, "for_analysis"),
        # Optional: pass Benjamin's summary CSV here to validate the
        # relative-control outputs over the reported period.
        validation_csv=path.join(root, "RCaN_validation_ACTNOW.csv"),
    )