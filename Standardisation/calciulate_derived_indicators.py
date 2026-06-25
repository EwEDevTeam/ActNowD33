from pathlib import Path
import pandas as pd
import logging


PRODUCT_FOLDERS = {
    "climate_only",
    "climate_management",
    "relative_historical",
    "relative_intervention",
}


TOTAL_BIOMASS_PARTS = [
    "consumer_biomass",
    "phytoplankton_biomass",
    "zooplankton_biomass",
    "benthic_invert_biomass",
]


def setup_logger(root: Path) -> logging.Logger:
    logger = logging.getLogger("actnow-derived-indicators")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")

    console = logging.StreamHandler()
    console.setFormatter(formatter)
    logger.addHandler(console)

    file_handler = logging.FileHandler(root / "derived_indicators.log", mode="w", encoding="utf-8")
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    return logger


def read_indicator(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)

    required = {"date", "value"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"{path} is missing columns: {sorted(missing)}")

    return df[["date", "value"]].copy()


def write_indicator(path: Path, df: pd.DataFrame, overwrite: bool, logger: logging.Logger) -> None:
    if path.exists() and not overwrite:
        logger.info(f"Keeping existing file: {path}")
        return

    df.to_csv(path, index=False)
    logger.info(f"Wrote derived indicator: {path}")


def derive_total_biomass(folder: Path, overwrite: bool, logger: logging.Logger) -> None:
    out_file = folder / "total_biomass.csv"

    if out_file.exists() and not overwrite:
        return

    missing = [
        indicator for indicator in TOTAL_BIOMASS_PARTS
        if not (folder / f"{indicator}.csv").exists()
    ]

    if missing:
        logger.warning(
            f"Cannot derive total_biomass in {folder}: missing {missing}"
        )
        return

    available = []
    for indicator in TOTAL_BIOMASS_PARTS:
        path = folder / f"{indicator}.csv"
        df = read_indicator(path)
        df = df.rename(columns={"value": indicator})
        available.append(df)

    merged = available[0]
    for df in available[1:]:
        merged = merged.merge(df, on="date", how="outer")

    value_columns = [col for col in merged.columns if col != "date"]
    merged["value"] = merged[value_columns].sum(axis=1, skipna=True)

    out_df = merged[["date", "value"]]
    write_indicator(out_file, out_df, overwrite, logger)


def derive_demersal_pelagic_ratio(folder: Path, overwrite: bool, logger: logging.Logger) -> None:
    out_file = folder / "demersal_pelagic_ratio.csv"

    if out_file.exists() and not overwrite:
        return

    demersal_file = folder / "demersal_biomass.csv"
    pelagic_file = folder / "pelagic_biomass.csv"

    if not demersal_file.exists() or not pelagic_file.exists():
        logger.warning(f"Cannot derive demersal_pelagic_ratio in {folder}: missing demersal or pelagic biomass")
        return

    demersal = read_indicator(demersal_file).rename(columns={"value": "demersal"})
    pelagic = read_indicator(pelagic_file).rename(columns={"value": "pelagic"})

    merged = demersal.merge(pelagic, on="date", how="inner")
    merged["value"] = merged["demersal"] / merged["pelagic"]
    merged.loc[merged["pelagic"] == 0, "value"] = pd.NA

    out_df = merged[["date", "value"]]
    write_indicator(out_file, out_df, overwrite, logger)


def enrich_standardised_output(model_root: str | Path, overwrite: bool = False) -> None:
    model_root = Path(model_root)
    logger = setup_logger(model_root)

    if not model_root.exists():
        raise FileNotFoundError(model_root)

    logger.info(f"Enriching derived indicators under: {model_root}")

    for product_folder in model_root.iterdir():
        if not product_folder.is_dir():
            continue

        if product_folder.name not in PRODUCT_FOLDERS:
            continue

        for scenario_folder in product_folder.iterdir():
            if not scenario_folder.is_dir():
                continue

            for cadence_folder in scenario_folder.iterdir():
                if not cadence_folder.is_dir():
                    continue

                derive_total_biomass(cadence_folder, overwrite, logger)
                derive_demersal_pelagic_ratio(cadence_folder, overwrite, logger)

    logger.info("Done.")


if __name__ == "__main__":
    enrich_standardised_output(
        model_root=r"cs07_bulgaria-coast_ewe/for_analysis/cs07_bulgaria-coast_ewe-ecospace",
        overwrite=False,
    )