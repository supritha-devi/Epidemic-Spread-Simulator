# =============================================================================
# report_export.R
# -----------------------------------------------------------------------------
# Builds one consistent report_data structure from the current simulation run,
# then exports it as CSV, JSON, or PDF (via R Markdown). All three formats
# come from the same data, so the numbers always match across formats.
# =============================================================================

library(jsonlite)

#' Assemble the report content for the current run.
#'
#' @param disease Disease name.
#' @param model_type "SIR" / "SEIR" / "SIRS" (ignored for AIDS/HIV).
#' @param sim Result of run_model(), or NULL for AIDS/HIV.
#' @param real_fetch Result of fetch_real_data(disease) (optional).
build_report_data <- function(disease, model_type, sim, real_fetch = NULL) {
  is_aids <- identical(disease, "AIDS/HIV")

  metadata <- list(
    disease = disease,
    model = if (is_aids) "Not modeled (real data only)" else model_type,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )

  if (!is_aids) {
    params <- DISEASE_PARAMS[[disease]]
    parameters <- list(
      transmission_rate_beta = params$beta,
      recovery_rate_gamma = params$gamma,
      population = POPULATION,
      vaccination_coverage_pct = params$vaccination,
      initial_infected = INITIAL_INFECTED,
      basic_reproduction_number_R0 = round(params$beta / params$gamma, 2)
    )
    results <- list(
      peak_infection_day = sim$peak_day,
      peak_infected_count = round(sim$peak_value),
      outbreak_duration_days = SIM_DAYS,
      total_recovered = round(sim$recovered_end),
      peak_icu_bed_demand_est = round(sim$peak_value * 0.05)  # illustrative placeholder ratio, not clinical
    )
  } else {
    parameters <- list(note = "AIDS/HIV is not modeled with SIR/SEIR/SIRS -- see disease note.")
    results <- list(
      people_living_with_hiv = AIDS_REFERENCE$people_living_with_hiv,
      on_treatment_pct = AIDS_REFERENCE$on_treatment_pct,
      source = AIDS_REFERENCE$source
    )
  }

  real_world_reference <- if (!is.null(real_fetch)) {
    list(
      source = if (real_fetch$source == "live") "Live fetch" else "Cached reference copy",
      fetched_at = format(real_fetch$fetched_at, "%Y-%m-%dT%H:%M:%S"),
      note = real_fetch$note
    )
  } else {
    list(note = "Not fetched for this report.")
  }

  list(
    metadata = metadata,
    parameters = parameters,
    results = results,
    real_world_reference = real_world_reference,
    sim_data = if (!is_aids) sim$data else NULL  # full day-by-day table, used by the PDF chart
  )
}

#' Write the report as a flat CSV (category, field, value rows) -- mirrors
#' the structure used in the project's sample exports.
write_csv_report <- function(report_data, path) {
  flatten <- function(lst, category) {
    keep <- lst[!vapply(lst, is.null, logical(1))]
    keep <- keep[!vapply(keep, is.data.frame, logical(1))]  # skip sim_data table here
    if (length(keep) == 0) return(NULL)
    data.frame(
      category = category,
      field = names(keep),
      value = vapply(keep, function(v) paste(v, collapse = "; "), character(1)),
      stringsAsFactors = FALSE
    )
  }
  rows <- rbind(
    flatten(report_data$metadata, "metadata"),
    flatten(report_data$parameters, "parameter"),
    flatten(report_data$results, "result"),
    flatten(report_data$real_world_reference, "real_world_reference")
  )
  write.csv(rows, path, row.names = FALSE)
  invisible(path)
}

#' Write the report as nested JSON.
write_json_report <- function(report_data, path) {
  export <- report_data
  export$sim_data <- NULL  # keep the JSON summary-sized; full curve stays in the PDF/app only
  jsonlite::write_json(export, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' Render the report as a PDF via R Markdown.
#' Requires the `rmarkdown` package and a working PDF engine (TinyTeX or a
#' full LaTeX install). If that's not set up, this will error with a clear
#' message rather than silently failing -- install with:
#'   install.packages("rmarkdown"); tinytex::install_tinytex()
render_pdf_report <- function(report_data, output_path, template = "report_template.Rmd") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The 'rmarkdown' package is required for PDF export. Install it with install.packages('rmarkdown').")
  }
  rmarkdown::render(
    input = template,
    output_file = output_path,
    params = list(report = report_data),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  invisible(output_path)
}
