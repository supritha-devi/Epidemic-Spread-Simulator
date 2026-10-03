# =============================================================================
# chatbot_engine.R
# -----------------------------------------------------------------------------
# A RULE-BASED chatbot: keyword lists, regex patterns, and simple dictionary
# lookups only. No AI, no ML model, no LLM call of any kind -- every response
# is produced by matching the user's text against the tables below and
# filling a template with numbers from the CURRENT simulation state.
# =============================================================================

# ---- Disease name detection (handles aliases + common misspellings) --------
DISEASE_ALIASES <- list(
  "Measles"      = c("measles", "measels", "rubeola"),
  "Seasonal Flu" = c("flu", "influenza", "influenzaa", "seasonal flu"),
  "COVID-19"     = c("covid", "covid-19", "covid19", "coronavirus", "corona"),
  "Chickenpox"   = c("chickenpox", "chicken pox", "varicella"),
  "Malaria"      = c("malaria", "maleria"),
  "Tuberculosis" = c("tuberculosis", "tb"),
  "Dengue"       = c("dengue", "dengu"),
  "AIDS/HIV"     = c("aids", "hiv", "hiv/aids", "aids/hiv")
)

#' Detect which disease (if any) the user's message refers to.
#' Falls back to approximate (fuzzy) matching for simple typos.
detect_disease <- function(text) {
  text_l <- tolower(text)
  for (canonical in names(DISEASE_ALIASES)) {
    for (alias in DISEASE_ALIASES[[canonical]]) {
      if (grepl(alias, text_l, fixed = TRUE)) return(canonical)
    }
  }
  # fuzzy fallback for small typos (e.g. "maleria", "covidd")
  words <- unique(unlist(strsplit(text_l, "[^a-z]+")))
  words <- words[nchar(words) >= 4]
  for (canonical in names(DISEASE_ALIASES)) {
    for (alias in DISEASE_ALIASES[[canonical]]) {
      hits <- words[sapply(words, function(w) {
        length(agrep(alias, w, max.distance = 0.3)) > 0
      })]
      if (length(hits) > 0) return(canonical)
    }
  }
  NULL
}

detect_percent <- function(text) {
  m <- regmatches(text, regexpr("\\d{1,3}\\s*%", text))
  if (length(m) == 0) return(NULL)
  as.numeric(gsub("[^0-9]", "", m))
}

detect_year <- function(text) {
  m <- regmatches(text, regexpr("\\b(19|20)\\d{2}\\b", text))
  if (length(m) == 0) return(NULL)
  as.integer(m)
}

normalize_text <- function(text) {
  text <- tolower(trimws(text))
  # expand a few very common chat abbreviations before matching
  repl <- c("\\bthx\\b"="thanks", "\\btnx\\b"="thanks", "\\bpls\\b"="please",
            "\\bplz\\b"="please", "\\bhii+\\b"="hi", "\\bhelo\\b"="hello")
  for (pattern in names(repl)) text <- gsub(pattern, repl[[pattern]], text)
  text
}

# ---- Intent table ------------------------------------------------------
# Checked in order; first match wins. Each entry has keyword substrings
# (simple containment check) and/or regex patterns.
INTENTS <- list(
  list(name = "GREETING",   keywords = c("hi", "hello", "hey", "good morning", "good evening", "good afternoon")),
  list(name = "BOT_IDENTITY", keywords = c("who are you", "what are you", "are you a bot", "are you ai", "your name")),
  list(name = "THANKS",     keywords = c("thanks", "thank you", "appreciate it")),
  list(name = "GOODBYE",    keywords = c("bye", "goodbye", "see you", "i'm done", "that's all")),
  list(name = "HELP",       keywords = c("help", "what can you", "what can i ask", "give me examples")),
  list(name = "LIST_DISEASES", keywords = c("which diseases", "what diseases", "list diseases", "diseases can you")),
  list(name = "REAL_WORLD_DATA", keywords = c("real data", "is this real", "actual data", "live data", "fetched live", "is that real")),
  list(name = "LIMITATIONS", keywords = c("limitation", "can i trust", "accurate", "can this predict", "is this a real forecast")),
  list(name = "SIR_MODEL",  keywords = c("what is sir", "explain sir", "how does sir work")),
  list(name = "SEIR_MODEL", keywords = c("what is seir", "explain seir", "what does e mean")),
  list(name = "SIRS_MODEL", keywords = c("what is sirs", "explain sirs", "reinfection")),
  list(name = "R0_INFO",    keywords = c("r0", "reproduction number", "basic reproduction")),
  list(name = "TRANSMISSION", keywords = c("how does it spread", "how is it transmitted", "contagious", "transmission")),
  list(name = "VACCINATION", keywords = c("vaccin", "vaccination coverage", "herd immunity")),
  list(name = "PEAK_INFECTION", keywords = c("peak", "highest", "maximum infected", "when will cases be highest")),
  list(name = "OUTBREAK_DURATION", keywords = c("how long", "when will it end", "duration", "outbreak end")),
  list(name = "HOSPITAL_ICU", keywords = c("icu", "hospital bed", "beds needed", "hospital capacity")),
  list(name = "MORTALITY",  keywords = c("death", "mortality", "fatality", "how many died", "deaths")),
  list(name = "PARAMETERS", keywords = c("what parameters", "show parameters", "current settings", "what is beta", "what is gamma")),
  list(name = "DISEASE_INFO", keywords = c("what is", "tell me about", "explain"))
)

#' Match the user's text to a single intent name.
match_intent <- function(text) {
  text_n <- normalize_text(text)
  for (intent in INTENTS) {
    if (any(sapply(intent$keywords, function(k) grepl(k, text_n, fixed = TRUE)))) {
      return(intent$name)
    }
  }
  "FALLBACK"
}

# ---- Static, non-prescriptive disease blurbs (education only, no dosing/treatment advice) ----
DISEASE_BLURBS <- list(
  "Measles"      = "Measles is a highly contagious airborne viral disease. It spreads very easily between unvaccinated people, which is why its simulated R0 here is the highest of all the diseases in this app.",
  "Seasonal Flu" = "Seasonal influenza is a respiratory virus that circulates every year, with new strains causing re-infection even in previously immune people.",
  "COVID-19"     = "COVID-19 is a respiratory viral disease that spreads through respiratory droplets and can vary widely in severity.",
  "Chickenpox"   = "Chickenpox (varicella) is a highly contagious childhood viral illness, now largely preventable through routine vaccination in many countries.",
  "Malaria"      = "Malaria is a mosquito-borne parasitic disease, not person-to-person contagious in the way airborne diseases are -- its 'transmission' in this simulation is a simplified stand-in for vector-borne spread.",
  "Tuberculosis" = "Tuberculosis (TB) is a bacterial infection, usually affecting the lungs, that spreads through prolonged close contact and can remain latent for years.",
  "Dengue"       = "Dengue is a mosquito-borne viral disease common in tropical and subtropical regions, with periodic large outbreaks.",
  "AIDS/HIV"     = AIDS_REFERENCE$note
)

#' Produce the chatbot's reply text.
#'
#' @param user_text What the user typed.
#' @param state A list describing the CURRENT app state:
#'   disease, model_type, sim (result of run_model(), or NULL for AIDS/HIV).
chat_respond <- function(user_text, state) {
  intent <- match_intent(user_text)
  mentioned_disease <- detect_disease(user_text)
  disease <- if (!is.null(mentioned_disease)) mentioned_disease else state$disease
  is_aids <- identical(disease, "AIDS/HIV")
  sim <- if (!is_aids) state$sim else NULL
  params <- if (!is_aids) DISEASE_PARAMS[[disease]] else NULL

  switch(intent,
    "GREETING" = "Hello! I can help with disease info, infection trends, hospital demand, vaccination, and simulation results. Ask me anything about the current run.",
    "BOT_IDENTITY" = "I'm a rule-based assistant built for this epidemic simulator -- no AI model, just keyword and pattern matching against the current simulation state.",
    "THANKS" = "You're welcome! Let me know if you'd like to explore another disease or scenario.",
    "GOODBYE" = "Goodbye! Come back anytime to explore another scenario.",
    "HELP" = "You can ask about: disease info, transmission, R0, peak infections, outbreak duration, vaccination, hospital/ICU demand, or how the SIR/SEIR/SIRS models work.",
    "LIST_DISEASES" = paste("This app covers:", paste(ALL_DISEASES, collapse = ", ")),
    "REAL_WORLD_DATA" = "The epidemic curve, dashboard cards, and map intensities are SIMULATED -- computed from this disease's parameters, not fetched. Only the Real Data page pulls from a live source (WHO/OWID/UNAIDS), with a cached fallback if the live fetch fails.",
    "LIMITATIONS" = "This is a mathematical scenario model, not a validated real-world forecaster. Its output depends entirely on the parameters chosen, and should not be treated as a guaranteed prediction.",
    "SIR_MODEL" = "SIR splits the population into Susceptible, Infected, and Recovered. People move S -> I -> R and don't go back, which fits diseases with lasting immunity.",
    "SEIR_MODEL" = "SEIR adds an Exposed stage between Susceptible and Infected, for diseases with an incubation period where someone is infected but not yet contagious.",
    "SIRS_MODEL" = "SIRS adds a path back from Recovered to Susceptible, for diseases where immunity fades over time and reinfection is possible.",
    "R0_INFO" = if (!is.null(params))
      sprintf("For %s, the simulated R0 (beta/gamma) is %.2f -- roughly how many new people one infected person infects, on average, in a fully susceptible population.", disease, params$beta/params$gamma)
      else "AIDS/HIV isn't modeled with SIR/SEIR/SIRS, so no simulated R0 is calculated for it -- see the note on the Dashboard.",
    "TRANSMISSION" = DISEASE_BLURBS[[disease]],
    "VACCINATION" = if (!is.null(params)) {
      asked_pct <- detect_percent(user_text)
      if (!is.null(asked_pct)) {
        # entity extraction in action: re-run the simulation at the asked
        # vaccination level so the answer reflects that specific scenario,
        # not just the current default
        model_for_whatif <- if (!is.null(state$model_type)) state$model_type else "SIR"
        what_if <- run_model(model_type = model_for_whatif,
                              beta = params$beta, gamma = params$gamma,
                              vaccination_pct = asked_pct,
                              sigma = params$sigma, xi = params$xi)
        sprintf(paste("At %d%% vaccination coverage for %s, the simulated peak would be about",
                       "%s infections around day %d (compare to this run's current %d%% coverage).",
                       sep = " "),
                asked_pct, disease, format(round(what_if$peak_value), big.mark = ","),
                what_if$peak_day, params$vaccination)
      } else {
        sprintf("This run assumes %d%% vaccination coverage for %s. Higher coverage reduces the susceptible pool and can push the effective reproduction number below 1, preventing a large outbreak. Ask \"what if vaccination were 80%%\" to try a different level.", params$vaccination, disease)
      }
    } else "AIDS/HIV has no vaccination parameter modeled in this simulator; see the real reference figures on the Dashboard instead.",
    "PEAK_INFECTION" = if (!is.null(sim))
      sprintf("For the current %s run, infections peak around day %d, with about %s people infected at once.", disease, sim$peak_day, format(round(sim$peak_value), big.mark = ","))
      else "AIDS/HIV has no simulated peak -- its real-world trajectory is shown on the Real Data page instead.",
    "OUTBREAK_DURATION" = sprintf("This simulation runs for a fixed %d-day window; check the epidemic curve to see how far the infected count has fallen by the end of that window for %s.", SIM_DAYS, disease),
    "HOSPITAL_ICU" = if (!is.null(sim))
      sprintf("Using a simple placeholder ratio (not a clinical estimate), roughly %s%% of peak infections might need hospital care -- about %s people at the peak for %s.", "5", format(round(sim$peak_value*0.05), big.mark=","), disease)
      else "Hospital demand isn't simulated for AIDS/HIV in this app.",
    "MORTALITY" = "This simplified SIR-family model doesn't track deaths separately from the Recovered compartment, so it can't report a mortality figure -- only real-world sources (the Real Data page) carry death statistics here.",
    "PARAMETERS" = if (!is.null(params))
      sprintf("Current %s parameters: beta=%.2f, gamma=%.2f, vaccination=%d%%, population=%s, initial infected=%d.", disease, params$beta, params$gamma, params$vaccination, format(POPULATION, big.mark=","), INITIAL_INFECTED)
      else "AIDS/HIV has no SIR-style parameters in this app.",
    "DISEASE_INFO" = DISEASE_BLURBS[[disease]],
    # FALLBACK
    "I'm focused on this epidemic simulation project. Try asking about a disease, its transmission, R0, peak infections, vaccination, hospital demand, or the SIR/SEIR/SIRS models."
  )
}
