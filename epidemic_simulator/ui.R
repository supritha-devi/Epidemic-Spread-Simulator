# =============================================================================
# ui.R
# -----------------------------------------------------------------------------
# Seven pages wired together with a single hidden tabsetPanel + Back/Next
# buttons, mirroring the project's HTML mockup navigation. global.R has
# already run by the time this file is evaluated, so DISEASE_PARAMS,
# ALL_DISEASES, etc. are all available here.
# =============================================================================

ui <- fluidPage(
  title = "Epidemic Spread Simulator",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  div(
    style = "display:flex; align-items:center; padding:14px 6px;",
    span(class = "app-title", "Epidemic Spread Simulator"),
    span(class = "badge-live", "\U0001F310 Live WHO/OWID/UNAIDS data (offline fallback included)")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      div(
        class = "sidebar-nav",
        actionLink("nav_auth",       "\U0001F510 Login",     class = "nav-item"),
        actionLink("nav_dashboard",  "\U0001F3E0 Dashboard", class = "nav-item"),
        actionLink("nav_simulation", "\U0001F6E0\UFE0F Simulate", class = "nav-item"),
        actionLink("nav_visual",     "\U0001F4CA Visuals",   class = "nav-item"),
        actionLink("nav_realdata",   "\U0001F310 Real Data", class = "nav-item"),
        actionLink("nav_reports",    "\U0001F4C4 Reports",   class = "nav-item"),
        actionLink("nav_chatbot",    "\U0001F4AC Bot",       class = "nav-item")
      )
    ),

    mainPanel(
      width = 10,
      tabsetPanel(
        id = "main_tabs", type = "hidden",

        # ---------------------------------------------------------------
        # PAGE 1: AUTH / ACCOUNT
        # ---------------------------------------------------------------
        tabPanelBody("auth",
          fluidRow(
            column(6,
              div(class = "panel-card",
                radioButtons("auth_mode", NULL, choices = c("Sign up" = "signup", "Log in" = "login"),
                             selected = "signup", inline = TRUE),

                conditionalPanel("input.auth_mode == 'signup'",
                  textInput("signup_email", "Email", placeholder = "you@example.com"),
                  textInput("signup_username", "Unique username", placeholder = "unique_username"),
                  passwordInput("signup_password", "Password", placeholder = "At least 6 characters"),
                  actionButton("do_signup", "Create account", class = "btn-pastel-primary"),
                  div(class = "data-source-note", "Passwords are hashed before storage. Your simulation history and avatar are saved to your account.")
                ),
                conditionalPanel("input.auth_mode == 'login'",
                  textInput("login_identifier", "Email or username"),
                  passwordInput("login_password", "Password"),
                  actionButton("do_login", "Log in", class = "btn-pastel-primary")
                ),

                br(), br(),
                textOutput("auth_message")
              )
            ),
            column(6,
              div(class = "panel-card",
                h2(class = "panel-title", "Build your avatar"),
                uiOutput("avatar_preview_ui"),
                selectInput("avatar_face", "Face", choices = c(
                  "\U0001F642" = "\U0001F642", "\U0001F60E" = "\U0001F60E", "\U0001F9D1" = "\U0001F9D1",
                  "\U0001F469" = "\U0001F469", "\U0001F9D4" = "\U0001F9D4", "\U0001F467" = "\U0001F467"
                )),
                selectInput("avatar_bg", "Background color", choices = c(
                  "Sky Blue" = "#D2E6FF", "Lavender" = "#E6D8FF", "Celadon Green" = "#E2F0D9",
                  "Butter Yellow" = "#FFF3C4", "Pale Peach" = "#FFE4D6"
                )),
                selectInput("avatar_accessory", "Accessory", choices = c(
                  "None" = "", "Glasses \U0001F453" = "\U0001F453",
                  "Headphones \U0001F3A7" = "\U0001F3A7", "Cap \U0001F9E2" = "\U0001F9E2"
                )),
                div(class = "data-source-note", "Custom-built, stored to your profile -- like a Bitmoji/Snapchat-style picker, no photo upload required.")
              )
            )
          ),
          div(style = "text-align:right; margin-top:10px;",
            actionButton("enter_app", "Enter App \U2192", class = "btn-pastel-primary")
          )
        ),

        # ---------------------------------------------------------------
        # PAGE 2: DASHBOARD
        # ---------------------------------------------------------------
        tabPanelBody("dashboard",
          div(class = "panel-card",
            h2(class = "panel-title", "Select a disease"),
            radioButtons("disease_select", NULL, choices = ALL_DISEASES,
                         selected = "Measles", inline = TRUE),
            div(class = "insight-box", AIDS_REFERENCE$note)
          ),
          uiOutput("dashboard_cards_ui"),
          div(style = "text-align:right;",
            actionButton("back_dashboard", "\U2190 Back", class = "btn-pastel-ghost"),
            actionButton("next_dashboard", "Next: Set Parameters \U2192", class = "btn-pastel-primary")
          )
        ),

        # ---------------------------------------------------------------
        # PAGE 3: SIMULATION ENGINE
        # ---------------------------------------------------------------
        tabPanelBody("simulation",
          div(class = "panel-card",
            h2(class = "panel-title", textOutput("sim_header_text", inline = TRUE)),
            conditionalPanel("input.disease_select != 'AIDS/HIV'",
              fluidRow(
                column(6,
                  selectInput("model_type", "Model type", choices = c("SIR", "SEIR", "SIRS"), selected = "SIR"),
                  sliderInput("beta", "Transmission rate (\u03B2)", min = 0.05, max = 0.9, value = 0.4, step = 0.01),
                  sliderInput("gamma", "Recovery rate (\u03B3)", min = 0.05, max = 0.5, value = 0.15, step = 0.01),
                  sliderInput("vaccination", "Vaccination coverage (%)", min = 0, max = 100, value = 50, step = 1),
                  sliderInput("initial_infected", "Initial infected", min = 10, max = 2000, value = 50, step = 10)
                ),
                column(6,
                  div(class = "panel-card", style = "background:#EBF8FF; border:none;",
                    uiOutput("sim_summary_ui")
                  )
                )
              )
            ),
            conditionalPanel("input.disease_select == 'AIDS/HIV'",
              div(class = "insight-box", AIDS_REFERENCE$note)
            )
          ),
          div(style = "text-align:right;",
            actionButton("back_simulation", "\U2190 Back", class = "btn-pastel-ghost"),
            actionButton("next_simulation", "Next: View Charts \U2192", class = "btn-pastel-primary")
          )
        ),

        # ---------------------------------------------------------------
        # PAGE 4: VISUALIZATIONS
        # ---------------------------------------------------------------
        tabPanelBody("visual",
          div(class = "panel-card",
            h2(class = "panel-title", "Epidemic curve"),
            conditionalPanel("input.disease_select != 'AIDS/HIV'", plotOutput("epidemic_curve_plot", height = "280px")),
            conditionalPanel("input.disease_select == 'AIDS/HIV'", div(class = "insight-box", "No simulated curve for AIDS/HIV -- see Real Data page."))
          ),
          div(class = "panel-card",
            fluidRow(
              column(9, h2(class = "panel-title", "Outbreak hotspots", style = "display:inline-block;")),
              column(3, radioButtons("map_theme", NULL, choices = c("\u2600\uFE0F Light" = "light", "\U0001F319 Dark" = "dark"),
                                      selected = "light", inline = TRUE))
            ),
            plotOutput("hotspot_map", height = "380px")
          ),
          fluidRow(
            column(6,
              div(class = "panel-card",
                h2(class = "panel-title", "Case ranking by region"),
                uiOutput("region_ranking_ui")
              )
            ),
            column(6,
              div(class = "panel-card",
                h2(class = "panel-title", "Demographic impact"),
                conditionalPanel("input.disease_select != 'AIDS/HIV'", plotOutput("demographic_plot", height = "220px"))
              )
            )
          ),
          div(style = "text-align:right;",
            actionButton("back_visual", "\U2190 Back", class = "btn-pastel-ghost"),
            actionButton("next_visual", "Next: Real vs Simulated \U2192", class = "btn-pastel-primary")
          )
        ),

        # ---------------------------------------------------------------
        # PAGE 5: REAL DATA
        # ---------------------------------------------------------------
        tabPanelBody("realdata",
          div(class = "panel-card",
            h2(class = "panel-title", textOutput("realdata_header_text", inline = TRUE)),
            div(class = "data-source-note", textOutput("realdata_source_text", inline = TRUE)),
            actionButton("fetch_real_data_btn", "Refresh now", class = "btn-pastel-ghost", style = "margin-bottom:10px;"),
            plotOutput("realdata_plot", height = "280px"),
            uiOutput("aids_realdata_note_ui")
          ),
          div(style = "text-align:right;",
            actionButton("back_realdata", "\U2190 Back", class = "btn-pastel-ghost"),
            actionButton("next_realdata", "Next: Reports \U2192", class = "btn-pastel-primary")
          )
        ),

        # ---------------------------------------------------------------
        # PAGE 6: REPORTS
        # ---------------------------------------------------------------
        tabPanelBody("reports",
          div(class = "panel-card",
            h2(class = "panel-title", "Export this run"),
            downloadButton("download_csv", "\u2B07 CSV", class = "btn-pastel-ghost"),
            downloadButton("download_json", "\u2B07 JSON", class = "btn-pastel-ghost"),
            downloadButton("download_pdf", "\u2B07 PDF Report", class = "btn-pastel-primary"),
            div(class = "insight-box", style = "margin-top:16px;",
              "Every export is built from the SAME run, so the numbers always match across CSV, JSON, and PDF. ",
              "PDF export requires the 'rmarkdown' package and a working PDF engine (TinyTeX) -- see README."
            )
          ),
          div(style = "text-align:right;",
            actionButton("back_reports", "\U2190 Back", class = "btn-pastel-ghost"),
            actionButton("next_reports", "Next: Ask the Chatbot \U2192", class = "btn-pastel-primary")
          )
        ),

        # ---------------------------------------------------------------
        # PAGE 7: CHATBOT
        # ---------------------------------------------------------------
        tabPanelBody("chatbot",
          div(class = "chat-window",
            div(class = "chat-header", "\U0001FA7A Epidemic Assistant (rule-based, no AI/ML)"),
            div(class = "chat-body", uiOutput("chat_history_ui")),
            div(style = "display:flex; gap:8px; padding:14px; border-top:1px solid #EDF2F7;",
              textInput("chat_input", NULL, placeholder = "Type a question...", width = "100%"),
              actionButton("chat_send", "\u27A4", class = "btn-pastel-primary")
            )
          ),
          div(style = "text-align:right; margin-top:10px;",
            actionButton("back_chatbot", "\U2190 Back", class = "btn-pastel-ghost"),
            actionButton("next_chatbot", "\U2190 Back to Dashboard", class = "btn-pastel-primary")
          )
        )
      )
    )
  )
)
