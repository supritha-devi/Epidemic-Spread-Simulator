# =============================================================================
# global.R
# -----------------------------------------------------------------------------
# Loads required packages and sources every R/ file, once, when the app
# starts. Shiny automatically runs global.R before ui.R and server.R.
# =============================================================================

required_packages <- c(
  "shiny", "deSolve", "ggplot2", "maps", "jsonlite", "digest"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them first, e.g.:\n",
    "install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))"
  )
}

library(shiny)
library(deSolve)
library(ggplot2)
library(maps)
library(jsonlite)
library(digest)

# Source order matters: disease_data.R defines the constants/tables that
# several other files rely on as function-argument defaults.
source("R/disease_data.R")
source("R/sir_model.R")
source("R/map_helpers.R")
source("R/live_fetch.R")
source("R/chatbot_engine.R")
source("R/auth.R")
source("R/report_export.R")

# Make sure the local user store exists before the app starts.
init_user_store()
