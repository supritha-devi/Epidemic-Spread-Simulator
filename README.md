# Epidemic Spread Simulator

An interactive R Shiny platform for simulating, visualizing, and comparing infectious disease outbreaks against real-world data.

Built with compartmental epidemiological models (SIR / SEIR / SIRS), a rule-based conversational assistant, and a live/cached real-world data comparison layer — covering eight diseases from measles to HIV/AIDS.

---

#Live Demo

https://supritha-devi.shinyapps.io/epidemic_simulator/

## Overview

Epidemic Spread Simulator lets you model how an infectious disease spreads through a population, tune the parameters that drive an outbreak (transmission rate, recovery rate, vaccination coverage), and see the results as an interactive dashboard: epidemic curves, a world map of simulated hotspots, demographic impact, and regional case rankings.

A dedicated Real Data page pulls published figures from WHO and Our World in Data and plots them alongside the simulation, so the two are never conflated — every number on screen is clearly labeled Simulated or Real. A built-in rule-based chatbot (no AI/LLM involved) answers questions about the current run using keyword and pattern matching with live entity extraction (e.g. "what if vaccination were 80%?" re-runs the model on the spot).

## Features

| Area | What it does |
|---|---|
| Simulation Engine | SIR, SEIR, and SIRS models solved numerically with `deSolve`, across 8 diseases (Measles, Seasonal Flu, COVID-19, Chickenpox, Malaria, Tuberculosis, Dengue, AIDS/HIV) |
| Outbreak Map | World map colored by simulated regional intensity, with pulsing hotspot markers |
| Visual Dashboard | Epidemic curves, demographic impact breakdown, region-by-region case ranking |
| Real-World Comparison | Live fetch from WHO / Our World in Data with an offline cached fallback — always labeled by source |
| Rule-Based Chatbot | ~20 intents with keyword/regex matching, disease + percentage + year entity extraction, and simulation-aware answers |
| Local Accounts | Sign-up/login with a custom emoji-based avatar builder, hashed passwords |
| Report Export | CSV, JSON, and PDF — generated from one shared data structure, so all three always agree |
| Design | Cohesive pastel UI theme with light/dark map modes |

## Important design note: HIV/AIDS

HIV/AIDS is included, but is not modeled with SIR/SEIR/SIRS. Its multi-year, staged disease progression doesn't fit a short-term infection-then-recovery framework, so the app deliberately shows real UNAIDS reference figures only for this disease, with an on-screen explanation of why — rather than forcing it into a model that doesn't apply.

## Tech Stack

- Framework: Shiny (R)
- Modeling: deSolve (numerical ODE solver)
- Visualization: ggplot2, base R graphics
- Mapping: maps + ggplot2 world polygon data — deliberately dependency-light (see below)
- Data: WHO, Our World in Data, UNAIDS
- Reports: jsonlite, R Markdown + rmarkdown (PDF)
- Auth: digest (SHA-256 password hashing)

Why no leaflet/sf? An interactive map would normally use sf + terra + rnaturalearth + leaflet, but that stack requires compiled GDAL/PROJ system libraries that frequently fail to install on Windows and can break deployment tooling. This project uses ggplot2 + the base maps package instead — zero compiled dependencies, installs and deploys reliably on any machine.

## Getting Started

### Prerequisites
- R >= 4.0
- RStudio (recommended)
- Shiny >= 1.6.0 (required for the app's hidden-tab navigation)

### Installation

```r
install.packages(c("shiny", "deSolve", "ggplot2", "maps", "jsonlite", "digest"))
