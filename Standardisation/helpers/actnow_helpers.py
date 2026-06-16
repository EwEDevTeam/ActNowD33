#
# This file provides the standards for homogenized model output for the
# collaborative modelling approach in EU project ActNow, task 3.3, D3.3
#

import pandas as pd
from enum import Enum

#
# The historical reference period for all models
#
INTERCOMPARISON_REFERENCE_PERIOD = (2005, 2015)

#
# All commonly produced indicators
#
class IndicatorEnum(Enum):
    TOTAL_BIOMASS = "total_biomass"
    CONSUMER_BIOMASS = "consumer_biomass"
    PELAGIC_BIOMASS = "pelagic_biomass"
    DEMERSAL_BIOMASS = "demersal_biomass"
    PHYTOPLANKTON_BIOMASS = "phytoplankton_biomass"
    ZOOPLANKTON_BIOMASS = "zooplankton_biomass"
    FISH_BIOMASS = "fish_biomass"
    TOTAL_CATCH = "total_catch"
    CONSUMER_CATCH = "consumer_catch"
    PELAGIC_CATCH = "pelagic_catch"
    DEMERSAL_CATCH = "demersal_catch"
    CATCH_TL = "catch_trophic_level"
    MEAN_TL = "mean_trophic_level"
    EVENNESS_Q = "richness_proxy"
    HTL_GROUPS_BIOMASS = "high_trophic_level_biomass"
    BENTHIC_INVERTEBRATES_BIOMASS = "benthic_invert_biomass"
    DEMERSAL_PELAGIC_RATIO = "demersal_pelagic_ratio"


#
# All commonly addressed global narratives, based on ActNow deliverable D1.2
#
class ScenarioEnum(Enum):
    GS = "gs" # Global sustainability
    IN = "in" # Inequality
    RR = "rr" # Regional rivaly
    WM = "wm" # World market


#
# The four output folders, extracted from the model output, for downstream
# plotting and analysis
#
class FolderTypeEnum(Enum):
    CLIMATE_ONLY = "climate_only"
    CLIMATE_MANAGEMENT = "climate_management"
    RELATIVE_INTERVENTION = "relative_intervention"
    RELATIVE_HISTORICAL = "relative_historical"


#
# Utility function, detects whether model output is annual or monthly
#
def detect_cadence_from_dates(df: pd.DataFrame) -> str:
    dates = pd.to_datetime(df["date"], errors="coerce").dropna()
    dates = dates.sort_values()

    if dates.empty:
        return "unknown"

    if len(dates) < 2:
        return "annual"

    day_steps = dates.diff().dropna().dt.days
    median_step = day_steps.median()

    if median_step > 300:
        return "annual"

    if 25 <= median_step <= 35:
        return "monthly"

    return "irregular"