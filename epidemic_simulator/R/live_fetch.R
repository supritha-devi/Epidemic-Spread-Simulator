# =============================================================================
# live_fetch.R
# -----------------------------------------------------------------------------
# This is the ONLY file in the whole app that is meant to reach the internet.
# Everything else (simulation, map, chatbot) is computed locally.
#
# IMPORTANT HONESTY NOTE FOR THE DEVELOPER (you):
# I could not test these URLs from the sandbox this code was written in --
# it has no internet access. The URL patterns below match what WHO / Our
# World in Data publish as of early 2026 (as discussed while planning this
# app), but data providers occasionally restructure their export paths.
# If fetch_real_data() falls back to the cached copy and prints a warning,
# open the URL in a browser first to confirm it still points at a raw CSV.
#
# Every disease falls back to a bundled offline CSV in data/cache/ so the
# app still runs end-to-end (e.g. for an offline viva demo) even if a fetch
# fails or there's no internet at all.
# =============================================================================

# Real endpoints to attempt, per disease. These are plain CSV files (not an
# authenticated REST API), so a simple read.csv() on the URL is enough --
# no API key required.
#
# Verification status (checked by web search while writing this file,
# since this sandbox has no internet to test a live request directly):
#   Measles, Tuberculosis : CONFIRMED -- OWID's "Quick download" CSV link
#                            for these exact charts was found and matches
#                            the URL below.
#   COVID-19               : CONFIRMED -- this is OWID's well-known, stable,
#                            whole-dataset CSV (used very widely).
#   Malaria                 : UNVERIFIED BEST GUESS -- OWID does publish a
#                            WHO-sourced malaria-cases indicator, but the
#                            exact public chart slug wasn't confirmed. It's
#                            included because it's safe to *try* (the
#                            tryCatch fallback below handles a wrong/changed
#                            slug gracefully) -- just don't present this one
#                            as "double-checked" the way the first three are.
#   Dengue, AIDS/HIV        : CONFIRMED NOT DIRECTLY DOWNLOADABLE -- OWID's
#                            own page for both says "The data in this chart
#                            is not available to download" because the
#                            underlying data is IHME Global Burden of Disease
#                            data, which requires separate IHME registration
#                            to export. These are intentionally left as NA
#                            so the app is honest about using the cached
#                            copy, instead of silently failing against a URL
#                            that was never going to work.
#   Seasonal Flu            : WHO FluNet has no simple CSV export (its data
#                            portal is interactive), so this also always
#                            falls back to the cache.
LIVE_DATA_URLS <- list(
  "Measles"      = "https://ourworldindata.org/grapher/reported-cases-of-measles.csv?v=1&csvType=full&useColumnShortNames=false",
  "Seasonal Flu" = NA,  # WHO FluNet export is interactive-only; see README
  "COVID-19"     = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
  "Chickenpox"   = NA,  # no standardized global open dataset -- always uses cache; see README
  "Malaria"      = "https://ourworldindata.org/grapher/estimated-number-of-malaria-cases.csv?v=1&csvType=full&useColumnShortNames=false",
  "Tuberculosis" = "https://ourworldindata.org/grapher/number-of-tuberculosis-cases.csv?v=1&csvType=full&useColumnShortNames=false",
  "Dengue"       = NA,  # OWID: IHME-sourced, "not available to download" -- see README
  "AIDS/HIV"     = NA   # OWID: IHME-sourced, "not available to download" -- see README
)

# Local filename (under data/cache/) each disease falls back to.
CACHE_FILES <- list(
  "Measles"      = "measles.csv",
  "Seasonal Flu" = "seasonal_flu.csv",
  "COVID-19"     = "covid19.csv",
  "Chickenpox"   = "chickenpox.csv",
  "Malaria"      = "malaria.csv",
  "Tuberculosis" = "tuberculosis.csv",
  "Dengue"       = "dengue.csv",
  "AIDS/HIV"     = "aids.csv"
)

#' Fetch real-world reference data for a disease.
#'
#' Tries the live URL first (when one exists and looks like a direct CSV
#' endpoint). On any failure -- no internet, a changed URL, a timeout, or a
#' disease with no reliable global source (Chickenpox) -- falls back to the
#' bundled cached CSV and clearly marks the result as "cached" rather than
#' "live" so the UI can be honest about which one the user is looking at.
#'
#' @param disease One of the names in ALL_DISEASES.
#' @param cache_dir Folder containing the fallback CSVs.
#' @param timeout_sec Max seconds to wait for the live request.
#' @return list(data = data.frame, source = "live"|"cached",
#'              fetched_at = POSIXct, note = character)
fetch_real_data <- function(disease, cache_dir = "data/cache", timeout_sec = 8) {

  cache_path <- file.path(cache_dir, CACHE_FILES[[disease]])
  url <- LIVE_DATA_URLS[[disease]]

  use_cache <- function(note) {
    df <- tryCatch(
      read.csv(cache_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    list(
      data = df,
      source = "cached",
      fetched_at = Sys.time(),
      note = note
    )
  }

  # Diseases with no direct CSV endpoint (interactive portals only) always
  # use the cache and say so honestly, rather than pretending to fetch.
  if (is.na(url) || !grepl("\\.csv($|\\?)", url)) {
    return(use_cache(paste(
      "No direct CSV endpoint is available for", disease,
      "-- using the bundled reference copy. See README for how to pull a",
      "fresh export manually from the source portal."
    )))
  }

  result <- tryCatch({
    old_timeout <- getOption("timeout")
    options(timeout = timeout_sec)
    on.exit(options(timeout = old_timeout), add = TRUE)

    df <- read.csv(url, stringsAsFactors = FALSE)
    if (nrow(df) == 0) stop("Live source returned zero rows.")

    list(
      data = df,
      source = "live",
      fetched_at = Sys.time(),
      note = paste("Fetched live from", url)
    )
  }, error = function(e) {
    use_cache(paste0(
      "Live fetch failed (", conditionMessage(e), ") -- showing the cached ",
      "reference copy instead. Check that the URL in live_fetch.R still ",
      "points at a valid CSV export."
    ))
  })

  result
}

#' Fetch real-world data for every disease at once (used to warm a cache on
#' app startup, if you want that -- not called automatically by default
#' since it would slow down app launch; call manually if desired).
fetch_all_real_data <- function(cache_dir = "data/cache") {
  setNames(
    lapply(ALL_DISEASES, fetch_real_data, cache_dir = cache_dir),
    ALL_DISEASES
  )
}
