# =============================================================================
# server.R
# =============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    logged_in = FALSE,
    username = NULL
  )

  # ---------------------------------------------------------------------
  # NAVIGATION (sidebar links + Back/Next buttons on every page)
  # ---------------------------------------------------------------------
  go_to <- function(page) updateTabsetPanel(session, "main_tabs", selected = page)

  observeEvent(input$nav_auth,       go_to("auth"))
  observeEvent(input$nav_dashboard,  go_to("dashboard"))
  observeEvent(input$nav_simulation, go_to("simulation"))
  observeEvent(input$nav_visual,     go_to("visual"))
  observeEvent(input$nav_realdata,   go_to("realdata"))
  observeEvent(input$nav_reports,    go_to("reports"))
  observeEvent(input$nav_chatbot,    go_to("chatbot"))

  observeEvent(input$enter_app,      go_to("dashboard"))

  observeEvent(input$back_dashboard, go_to("auth"))
  observeEvent(input$next_dashboard, go_to("simulation"))

  observeEvent(input$back_simulation, go_to("dashboard"))
  observeEvent(input$next_simulation, go_to("visual"))

  observeEvent(input$back_visual, go_to("simulation"))
  observeEvent(input$next_visual, go_to("realdata"))

  observeEvent(input$back_realdata, go_to("visual"))
  observeEvent(input$next_realdata, go_to("reports"))

  observeEvent(input$back_reports, go_to("realdata"))
  observeEvent(input$next_reports, go_to("chatbot"))

  observeEvent(input$back_chatbot, go_to("reports"))
  observeEvent(input$next_chatbot, go_to("dashboard"))

  # ---------------------------------------------------------------------
  # AUTH + AVATAR
  # ---------------------------------------------------------------------
  output$avatar_preview_ui <- renderUI({
    div(
      class = "avatar-preview",
      style = paste0("background:", input$avatar_bg, ";"),
      input$avatar_face,
      if (nzchar(input$avatar_accessory)) span(class = "avatar-accessory", input$avatar_accessory)
    )
  })

  output$auth_message <- renderText({ "" })

  observeEvent(input$do_signup, {
    result <- create_user(
      username = input$signup_username,
      email = input$signup_email,
      password = input$signup_password,
      avatar_face = input$avatar_face,
      avatar_bg = input$avatar_bg,
      avatar_accessory = input$avatar_accessory
    )
    output$auth_message <- renderText(result$message)
    if (result$success) {
      rv$logged_in <- TRUE
      rv$username <- input$signup_username
      showNotification("Account created -- you're logged in.", type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  observeEvent(input$do_login, {
    result <- verify_login(input$login_identifier, input$login_password)
    output$auth_message <- renderText(result$message)
    if (result$success) {
      rv$logged_in <- TRUE
      rv$username <- result$user$username
      updateSelectInput(session, "avatar_face", selected = result$user$avatar_face)
      updateSelectInput(session, "avatar_bg", selected = result$user$avatar_bg)
      updateSelectInput(session, "avatar_accessory", selected = result$user$avatar_accessory)
      showNotification(paste("Welcome back,", result$user$username), type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # ---------------------------------------------------------------------
  # DISEASE SELECTION (drives everything downstream)
  # ---------------------------------------------------------------------
  current_disease <- reactive({ req(input$disease_select); input$disease_select })
  is_aids <- reactive({ identical(current_disease(), "AIDS/HIV") })
  current_params <- reactive({
    if (is_aids()) NULL else DISEASE_PARAMS[[current_disease()]]
  })

  # When the disease changes, reset the simulation sliders to that disease's
  # preset values so the curve looks sensible by default (user can still
  # drag the sliders afterwards to explore other scenarios).
  observeEvent(current_disease(), {
    p <- current_params()
    if (!is.null(p)) {
      updateSliderInput(session, "beta", value = p$beta)
      updateSliderInput(session, "gamma", value = p$gamma)
      updateSliderInput(session, "vaccination", value = p$vaccination)
      updateSliderInput(session, "initial_infected", value = INITIAL_INFECTED)
    }
  })

  # ---------------------------------------------------------------------
  # DASHBOARD
  # ---------------------------------------------------------------------
  output$dashboard_cards_ui <- renderUI({
    if (is_aids()) {
      tagList(
        fluidRow(
          column(3, div(class = "status-card card-infected",
            div(class = "label", "People Living with HIV (real reference)"),
            div(class = "value", AIDS_REFERENCE$people_living_with_hiv),
            div(class = "sub", "UNAIDS, global, 2025")
          )),
          column(3, div(class = "status-card card-recovered",
            div(class = "label", "On Treatment (real reference)"),
            div(class = "value", paste0(AIDS_REFERENCE$on_treatment_pct, "%")),
            div(class = "sub", "of people living with HIV, global")
          )),
          column(3, div(class = "status-card card-vaccination",
            div(class = "label", "Vaccination Coverage"), div(class = "value", "N/A"),
            div(class = "sub", "not applicable")
          )),
          column(3, div(class = "status-card card-r0",
            div(class = "label", "R\u2080 (not modeled)"), div(class = "value", "\u2014"),
            div(class = "sub", "see note above")
          ))
        )
      )
    } else {
      sim <- sim_result()
      p <- current_params()
      req(sim, p)
      active_today <- sim$data$I[min(26, nrow(sim$data))]
      tagList(
        fluidRow(
          column(3, div(class = "status-card card-infected",
            div(class = "label", "Active Cases (simulated)"),
            div(class = "value", format(round(active_today), big.mark = ",")),
            div(class = "sub", "day 25 snapshot, this run")
          )),
          column(3, div(class = "status-card card-recovered",
            div(class = "label", "Recovered (simulated)"),
            div(class = "value", format(round(sim$recovered_end), big.mark = ",")),
            div(class = "sub", paste("by day", SIM_DAYS))
          )),
          column(3, div(class = "status-card card-vaccination",
            div(class = "label", "Vaccination Coverage"),
            div(class = "value", paste0(p$vaccination, "%")),
            div(class = "sub", "slider input")
          )),
          column(3, div(class = "status-card card-r0",
            div(class = "label", "Simulated R\u2080"),
            div(class = "value", sim$r0),
            div(class = "sub", "\u03B2 / \u03B3")
          ))
        )
      )
    }
  })

  # ---------------------------------------------------------------------
  # SIMULATION
  # ---------------------------------------------------------------------
  output$sim_header_text <- renderText({
    paste("Simulation parameters --", current_disease())
  })

  sim_result <- reactive({
    req(!is_aids())
    req(input$beta, input$gamma, input$vaccination, input$model_type)
    p <- current_params()
    req(p)
    run_model(
      model_type = input$model_type,
      beta = input$beta, gamma = input$gamma,
      vaccination_pct = input$vaccination,
      sigma = p$sigma, xi = p$xi,
      initial_infected = if (!is.null(input$initial_infected)) input$initial_infected else INITIAL_INFECTED
    )
  })

  output$sim_summary_ui <- renderUI({
    sim <- sim_result()
    req(sim)
    tagList(
      div(style = "font-size:13px; margin-bottom:6px;", "Computed instantly from the sliders"),
      div(style = "font-size:13px; margin-bottom:6px;", HTML(paste0("R\u2080 = \u03B2/\u03B3 = <b>", sim$r0, "</b>"))),
      div(style = "font-size:13px; margin-bottom:6px;", HTML(paste0("Peak day: <b>Day ", sim$peak_day, "</b>"))),
      div(style = "font-size:13px;", HTML(paste0("Peak infected: <b>", format(round(sim$peak_value), big.mark = ","), "</b>")))
    )
  })

  # ---------------------------------------------------------------------
  # VISUALIZATIONS
  # ---------------------------------------------------------------------
  output$epidemic_curve_plot <- renderPlot({
    sim <- sim_result()
    req(sim)
    df <- sim$data
    plot(df$day, df$S, type = "l", col = "#B7791F", lwd = 2.5,
         ylim = c(0, max(df$S, df$I, df$R)),
         xlab = "Day", ylab = "People",
         main = paste("Simulated Epidemic Curve --", current_disease(), paste0("(", input$model_type, ")")))
    lines(df$day, df$I, col = "#3182CE", lwd = 2.5)
    lines(df$day, df$R, col = "#2E7D4E", lwd = 2.5)
    points(sim$peak_day, sim$peak_value, pch = 19, col = "#3182CE")
    text(sim$peak_day, sim$peak_value, labels = paste0("Peak: Day ", sim$peak_day), pos = 3, cex = 0.8)
    legend("topright", legend = c("Susceptible", "Infected", "Recovered"),
           col = c("#B7791F", "#3182CE", "#2E7D4E"), lwd = 2.5, bty = "n", cex = 0.85)
  })

  output$hotspot_map <- renderPlot({
    theme <- if (!is.null(input$map_theme)) input$map_theme else "light"
    if (is_aids()) {
      build_aids_map(theme)
    } else {
      build_hotspot_map(current_disease(), theme)
    }
  }, bg = "transparent")

  output$demographic_plot <- renderPlot({
    p <- current_params()
    req(p)
    vals <- p$demographic
    bp <- barplot(vals, names.arg = c("0-18", "19-65", "65+"),
                  col = c("#D2E6FF", "#E2F0D9", "#FFF3C4"),
                  ylim = c(0, 100), ylab = "% impact", border = NA)
    text(bp, vals + 4, labels = paste0(vals, "%"), cex = 0.9)
  })

  output$region_ranking_ui <- renderUI({
    if (is_aids()) {
      return(div(class = "data-source-note",
        "AIDS/HIV regions are shown on the map as real-world UNAIDS burden data (Southern & Eastern Africa), not a simulated ranking."))
    }
    region_data <- DISEASE_MAP_DATA[[current_disease()]]
    req(region_data)
    rows <- lapply(names(region_data), function(r) {
      info <- region_data[[r]]
      list(region = REGION_LABELS[[r]], level = info$level, count = info$count)
    })
    rows <- rows[order(-sapply(rows, function(r) r$count))]
    max_count <- rows[[1]]$count

    tagList(lapply(rows, function(r) {
      pct <- max(8, round(r$count / max_count * 90))
      div(class = "region-row",
        span(style = paste0("width:10px;height:10px;border-radius:50%;background:", INTENSITY_DOT[[r$level]], ";display:inline-block;")),
        span(class = "region-name", r$region),
        span(class = "region-bar-track",
          span(class = "region-bar-fill", style = paste0("width:", pct, "%; background:", INTENSITY_COLORS[[r$level]], ";"))),
        span(class = "region-count", format(r$count, big.mark = ","))
      )
    }))
  })

  # ---------------------------------------------------------------------
  # REAL DATA PAGE
  # ---------------------------------------------------------------------
  real_fetch_result <- eventReactive(
    list(input$fetch_real_data_btn, current_disease()),
    { fetch_real_data(current_disease()) },
    ignoreNULL = FALSE
  )

  output$realdata_header_text <- renderText({
    paste("Real-world comparison --", current_disease())
  })

  output$realdata_source_text <- renderText({
    rf <- real_fetch_result()
    req(rf)
    paste0(
      if (rf$source == "live") "Live source" else "Cached reference copy",
      " \u00b7 fetched ", format(rf$fetched_at, "%Y-%m-%d %H:%M")
    )
  })

  output$aids_realdata_note_ui <- renderUI({
    if (is_aids()) {
      div(class = "insight-box", AIDS_REFERENCE$note,
          " Source: ", AIDS_REFERENCE$source, ".")
    } else NULL
  })

  output$realdata_plot <- renderPlot({
    rf <- real_fetch_result()
    req(rf, rf$data)

    if (is_aids()) {
      df <- rf$data
      plot(df$year, df$people_living_with_hiv_millions, type = "o", col = "#C53030", lwd = 2.5, pch = 19,
           xlab = "Year", ylab = "People living with HIV (millions)",
           main = "Real-world reference -- AIDS/HIV (UNAIDS)")
    } else {
      real_df <- rf$data
      # the bundled/fetched real reference uses whichever numeric case column
      # is present (OWID/WHO exports name this column differently per source)
      case_col <- intersect(c("reported_cases", "cases", "new_cases", "total_cases"), names(real_df))
      if (length(case_col) == 0) case_col <- names(real_df)[sapply(real_df, is.numeric)][1]
      req(length(case_col) >= 1)

      sim <- sim_result()
      req(sim)
      sim_df <- sim$data

      y_real <- real_df[[case_col[1]]]
      x_real <- seq_along(y_real) - 1  # align with sim_df$day, which starts at 0

      plot(sim_df$day, sim_df$I, type = "l", col = "#3182CE", lwd = 2, lty = 2,
           xlab = "Day", ylab = "Cases",
           ylim = c(0, max(sim_df$I, y_real, na.rm = TRUE)),
           main = paste("Simulated vs. Real --", current_disease()))
      lines(x_real, y_real, col = "#C53030", lwd = 2.5)
      legend("topright", legend = c("Simulated infected", "Real reference"),
             col = c("#3182CE", "#C53030"), lty = c(2, 1), lwd = 2, bty = "n", cex = 0.85)
    }
  })

  # ---------------------------------------------------------------------
  # REPORTS
  # ---------------------------------------------------------------------
  current_report_data <- reactive({
    rf <- tryCatch(real_fetch_result(), error = function(e) NULL)
    build_report_data(
      disease = current_disease(),
      model_type = if (!is_aids()) input$model_type else NULL,
      sim = if (!is_aids()) sim_result() else NULL,
      real_fetch = rf
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", current_disease()), "_report.csv"),
    content = function(file) write_csv_report(current_report_data(), file)
  )

  output$download_json <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", current_disease()), "_report.json"),
    content = function(file) write_json_report(current_report_data(), file)
  )

  output$download_pdf <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", current_disease()), "_report.pdf"),
    content = function(file) {
      result <- tryCatch(
        render_pdf_report(current_report_data(), file),
        error = function(e) {
          showNotification(paste("PDF export failed:", conditionMessage(e)), type = "error", duration = 10)
          NULL
        }
      )
      req(result)
    }
  )

  # ---------------------------------------------------------------------
  # CHATBOT
  # ---------------------------------------------------------------------
  chat_history <- reactiveVal(list(
    list(role = "bot", text = "Hello! I can help with disease info, infection trends, hospital demand, vaccination, and simulation results. Ask me anything about the current run.")
  ))

  output$chat_history_ui <- renderUI({
    msgs <- chat_history()
    tagList(lapply(msgs, function(m) {
      div(class = if (m$role == "user") "bubble bubble-user" else "bubble bubble-bot", m$text)
    }))
  })

  observeEvent(input$chat_send, {
    req(nzchar(trimws(input$chat_input)))
    user_text <- input$chat_input

    state <- list(
      disease = current_disease(),
      model_type = if (!is_aids()) input$model_type else NULL,
      sim = if (!is_aids()) tryCatch(sim_result(), error = function(e) NULL) else NULL
    )
    bot_reply <- chat_respond(user_text, state)

    chat_history(c(chat_history(),
      list(list(role = "user", text = user_text)),
      list(list(role = "bot", text = bot_reply))
    ))
    updateTextInput(session, "chat_input", value = "")
  })
}
