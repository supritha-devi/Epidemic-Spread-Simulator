# =============================================================================
# sir_model.R
# -----------------------------------------------------------------------------
# Compartmental epidemic models (SIR, SEIR, SIRS), solved numerically with
# deSolve::ode(). This is the SIMULATED half of the app -- every number these
# functions produce is computed from the parameters passed in, never fetched
# from the internet.
# =============================================================================

library(deSolve)

# ---- SIR ---------------------------------------------------------------
# dS/dt = -beta*S*I/N
# dI/dt =  beta*S*I/N - gamma*I
# dR/dt =  gamma*I
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    N <- S + I + R
    dS <- -beta * S * I / N
    dI <-  beta * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}

# ---- SEIR --------------------------------------------------------------
# Adds an Exposed (incubating, not yet infectious) compartment.
# dS/dt = -beta*S*I/N
# dE/dt =  beta*S*I/N - sigma*E
# dI/dt =  sigma*E - gamma*I
# dR/dt =  gamma*I
seir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    N <- S + E + I + R
    dS <- -beta * S * I / N
    dE <-  beta * S * I / N - sigma * E
    dI <-  sigma * E - gamma * I
    dR <-  gamma * I
    list(c(dS, dE, dI, dR))
  })
}

# ---- SIRS --------------------------------------------------------------
# Adds waning immunity: recovered individuals return to Susceptible at rate xi.
# dS/dt = -beta*S*I/N + xi*R
# dI/dt =  beta*S*I/N - gamma*I
# dR/dt =  gamma*I - xi*R
sirs_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    N <- S + I + R
    dS <- -beta * S * I / N + xi * R
    dI <-  beta * S * I / N - gamma * I
    dR <-  gamma * I - xi * R
    list(c(dS, dI, dR))
  })
}

#' Run an epidemic simulation
#'
#' @param model_type One of "SIR", "SEIR", "SIRS".
#' @param beta Transmission rate (per day).
#' @param gamma Recovery rate (per day).
#' @param vaccination_pct Percent of the population already immune at day 0.
#' @param sigma Incubation rate (only used for SEIR).
#' @param xi Immunity waning rate (only used for SIRS).
#' @param population Total population size.
#' @param initial_infected Infected count at day 0.
#' @param days Number of days to simulate.
#'
#' @return A list with:
#'   - data: data.frame(day, S, [E,] I, R)
#'   - peak_day, peak_value: day and infected-count at the epidemic's peak
#'   - recovered_end: recovered count at the end of the simulation window
#'   - r0: basic reproduction number (beta / gamma)
run_model <- function(model_type = "SIR",
                       beta, gamma,
                       vaccination_pct = 0,
                       sigma = NULL, xi = NULL,
                       population = POPULATION,
                       initial_infected = INITIAL_INFECTED,
                       days = SIM_DAYS) {

  model_type <- toupper(model_type)
  stopifnot(model_type %in% c("SIR", "SEIR", "SIRS"))

  immune0 <- population * (vaccination_pct / 100)
  susceptible0 <- population * (1 - vaccination_pct / 100) - initial_infected
  if (susceptible0 < 0) susceptible0 <- 0

  times <- seq(0, days, by = 1)

  if (model_type == "SIR") {
    init <- c(S = susceptible0, I = initial_infected, R = immune0)
    params <- c(beta = beta, gamma = gamma)
    out <- as.data.frame(ode(y = init, times = times, func = sir_equations, parms = params))

  } else if (model_type == "SEIR") {
    if (is.null(sigma)) stop("SEIR requires a sigma (incubation rate) parameter.")
    init <- c(S = susceptible0, E = 0, I = initial_infected, R = immune0)
    params <- c(beta = beta, gamma = gamma, sigma = sigma)
    out <- as.data.frame(ode(y = init, times = times, func = seir_equations, parms = params))

  } else { # SIRS
    if (is.null(xi)) stop("SIRS requires an xi (immunity waning rate) parameter.")
    init <- c(S = susceptible0, I = initial_infected, R = immune0)
    params <- c(beta = beta, gamma = gamma, xi = xi)
    out <- as.data.frame(ode(y = init, times = times, func = sirs_equations, parms = params))
  }

  names(out)[names(out) == "time"] <- "day"
  # clamp tiny negative numerical-integration artifacts to zero
  num_cols <- setdiff(names(out), "day")
  out[num_cols] <- lapply(out[num_cols], function(col) pmax(col, 0))

  peak_idx   <- which.max(out$I)
  peak_day   <- out$day[peak_idx]
  peak_value <- out$I[peak_idx]
  recovered_end <- out$R[nrow(out)]

  list(
    data = out,
    peak_day = peak_day,
    peak_value = peak_value,
    recovered_end = recovered_end,
    r0 = round(beta / gamma, 2)
  )
}

#' Convenience wrapper: run a disease's default simulation using the
#' parameters stored in DISEASE_PARAMS (disease_data.R).
run_disease_simulation <- function(disease, model_type = "SIR") {
  p <- DISEASE_PARAMS[[disease]]
  if (is.null(p)) stop(paste("No simulation parameters found for disease:", disease))
  run_model(
    model_type = model_type,
    beta = p$beta, gamma = p$gamma,
    vaccination_pct = p$vaccination,
    sigma = p$sigma, xi = p$xi
  )
}
