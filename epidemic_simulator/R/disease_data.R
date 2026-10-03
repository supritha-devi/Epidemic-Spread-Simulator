# =============================================================================
# disease_data.R
# -----------------------------------------------------------------------------
# All static reference data for the simulator: per-disease epidemiological
# parameters (used by the SIMULATED pages), per-disease regional hotspot data
# (used by the map + case ranking list), and the AIDS/HIV real-world reference
# figures (used instead of a simulation, since AIDS/HIV does not fit the
# SIR/SEIR/SIRS short-infection-then-recovery assumption).
#
# These numbers are reused unchanged from the project's HTML mockup so the
# RStudio app and the mockup stay consistent with each other.
# =============================================================================

# Fixed simulation constants (kept identical across all diseases so results
# are comparable to one another).
POPULATION       <- 100000
INITIAL_INFECTED <- 50
SIM_DAYS         <- 130

# ---- Per-disease SIR/SEIR/SIRS parameters ----------------------------------
# beta   = transmission rate (per day)
# gamma  = recovery rate (per day)
# sigma  = incubation rate for SEIR (1 / incubation period in days)
# xi     = immunity waning rate for SIRS (1 / average immunity duration in days)
# vaccination = assumed current coverage, percent
# demographic = illustrative [0-18, 19-65, 65+] impact percentages
#
# Relative ordering of beta/gamma (R0) intentionally follows known relative
# contagiousness: measles > chickenpox > covid/dengue > TB/malaria > flu.
# These are TOY MODEL values for a 100,000-person, 130-day simulation window,
# not literature-exact epidemiological estimates -- the app labels every
# number from this table as "Simulated", never as a real-world statistic.
DISEASE_PARAMS <- list(
  "Measles" = list(
    beta = 0.55, gamma = 0.10, sigma = 1/10,  xi = 1/100000,
    vaccination = 72, demographic = c(54, 34, 18)
  ),
  "Seasonal Flu" = list(
    beta = 0.40, gamma = 0.21, sigma = 1/2,   xi = 1/180,
    vaccination = 30, demographic = c(30, 45, 68)
  ),
  "COVID-19" = list(
    beta = 0.45, gamma = 0.15, sigma = 1/5,   xi = 1/270,
    vaccination = 55, demographic = c(18, 55, 74)
  ),
  "Chickenpox" = list(
    beta = 0.50, gamma = 0.11, sigma = 1/14,  xi = 1/100000,
    vaccination = 70, demographic = c(81, 24, 9)
  ),
  "Malaria" = list(
    beta = 0.35, gamma = 0.14, sigma = 1/12,  xi = 1/365,
    vaccination = 8,  demographic = c(58, 50, 27)
  ),
  "Tuberculosis" = list(
    beta = 0.28, gamma = 0.09, sigma = 1/21,  xi = 1/100000,
    vaccination = 50, demographic = c(16, 62, 55)
  ),
  "Dengue" = list(
    beta = 0.38, gamma = 0.13, sigma = 1/5,   xi = 1/1000,
    vaccination = 5,  demographic = c(42, 57, 24)
  )
)

# The six diseases that have a running simulation. AIDS/HIV is deliberately
# excluded from this list -- see aids_reference below.
SIMULATED_DISEASES <- names(DISEASE_PARAMS)

# All selectable diseases, in the order shown on the Dashboard page.
ALL_DISEASES <- c(SIMULATED_DISEASES, "AIDS/HIV")

# ---- Regional hotspot data (map + case-ranking list) ------------------------
# level: one of "low" / "moderate" / "high" / "peak"
# count: illustrative simulated case count for that region, this run
DISEASE_MAP_DATA <- list(
  "Measles" = list(
    us = list(level = "high",     count = 4920),
    `in` = list(level = "peak",   count = 6140),
    eu = list(level = "moderate", count = 2310),
    af = list(level = "moderate", count = 1860),
    sa = list(level = "low",      count = 640)
  ),
  "Seasonal Flu" = list(
    us = list(level = "peak",     count = 7200),
    `in` = list(level = "moderate", count = 2100),
    eu = list(level = "high",     count = 5100),
    af = list(level = "low",      count = 380),
    sa = list(level = "moderate", count = 1450)
  ),
  "COVID-19" = list(
    us = list(level = "peak",     count = 9800),
    `in` = list(level = "high",   count = 6600),
    eu = list(level = "high",     count = 5900),
    af = list(level = "moderate", count = 2200),
    sa = list(level = "high",     count = 4700)
  ),
  "Chickenpox" = list(
    us = list(level = "low",      count = 520),
    `in` = list(level = "moderate", count = 1900),
    eu = list(level = "low",      count = 410),
    af = list(level = "low",      count = 300),
    sa = list(level = "low",      count = 260)
  ),
  "Malaria" = list(
    us = list(level = "low",      count = 90),
    `in` = list(level = "high",   count = 5300),
    eu = list(level = "low",      count = 60),
    af = list(level = "peak",     count = 8900),
    sa = list(level = "moderate", count = 2100)
  ),
  "Tuberculosis" = list(
    us = list(level = "low",      count = 680),
    `in` = list(level = "peak",   count = 7100),
    eu = list(level = "moderate", count = 1600),
    af = list(level = "high",     count = 4800),
    sa = list(level = "moderate", count = 1300)
  ),
  "Dengue" = list(
    us = list(level = "low",      count = 210),
    `in` = list(level = "peak",   count = 6800),
    eu = list(level = "low",      count = 40),
    af = list(level = "moderate", count = 1700),
    sa = list(level = "high",     count = 5200)
  )
)

# Region label + the real ISO3 country codes that each region group colors on
# the choropleth map (resolved to `maps` package country names in map_helpers.R).
REGION_LABELS <- c(
  us = "N. America", `in` = "S. Asia", eu = "E. Europe",
  af = "C. Africa",  sa = "S. America"
)
REGION_COUNTRIES <- list(
  us = c("USA"),
  `in` = c("IND"),
  eu = c("POL", "UKR", "ROU", "BLR", "MDA"),
  af = c("NGA", "COD", "CAF", "CMR", "GAB", "COG"),
  sa = c("BRA")
)

# Fill colors per intensity level (same pastel palette as the HTML mockup).
INTENSITY_COLORS <- c(
  low      = "#FFF9F2",
  moderate = "#FFE4D6",
  high     = "#FFCDCD",
  peak     = "#FFAAAA"
)
INTENSITY_DOT <- c(
  low = "#F6E05E", moderate = "#ED8936", high = "#E53E3E", peak = "#E53E3E"
)
DEFAULT_COUNTRY_FILL <- list(light = "#FFF9F2", dark = "#3A4454")

# ---- AIDS/HIV real-world reference figures ----------------------------------
# Source: UNAIDS Global HIV & AIDS Statistics Fact Sheet (2025 data),
# https://www.unaids.org/en/resources/fact-sheet
# These are NOT simulated and NOT live-fetched in this codebase -- they are a
# static reference snapshot. live_fetch.R shows how a genuinely live pull from
# aidsinfo.unaids.org would replace this constant.
AIDS_REFERENCE <- list(
  people_living_with_hiv = "41.0M",
  on_treatment_pct = 78,
  source = "UNAIDS Global HIV & AIDS Statistics Fact Sheet, 2025 data",
  source_url = "https://www.unaids.org/en/resources/fact-sheet",
  note = paste(
    "AIDS/HIV is shown with real-world reference data only.",
    "Its multi-year staged progression (acute -> chronic -> AIDS) does not",
    "fit the SIR/SEIR/SIRS assumption of short-term infection-then-recovery,",
    "so no simulated curve is generated for it."
  )
)

# Real-world higher-burden regions for AIDS/HIV (Southern & Eastern Africa),
# used only for the map on the Real Data page -- labeled as real regional
# burden, never as "simulated intensity".
AIDS_MAP_REGIONS <- list(
  southern_africa = list(
    countries = c("ZAF", "MOZ", "ZWE", "ZMB", "BWA", "NAM", "SWZ", "LSO"),
    level = "peak", label = "Southern Africa"
  ),
  eastern_africa = list(
    countries = c("KEN", "UGA", "TZA", "ETH"),
    level = "high", label = "Eastern Africa"
  ),
  rest_default = list(level = "low")
)
