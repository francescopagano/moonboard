ui <- dashboardPage(
  dashboardHeader(title = "MoonBoard 2016 Builder: "),
  dashboardSidebar(
    minimal_theme,
    div(style = "display: flex; flex-direction: column; align-items: center; gap: 5px; margin-top: 10px;",
        actionButton("reset", "Reset selezione", class = "action-btn btn-block", style = "width: 80%;"),
        actionButton("compute_features", "Calcola Feature", class = "action-btn btn-block", style = "width: 80%;"),
        actionButton("predict", "Predici difficoltà", class = "action-btn btn-block", style = "width: 80%;"),
        actionButton("debug_button", "Debug Modelli", class = "btn-block action-btn", style = "width: 80%; margin-top: 10px;")
    ),
    hr(),
    div(style = "padding: 0 15px;",
        radioButtons("is_benchmark", "È un Benchmark:",
                     choices = c("False" = "False", "True" = "True"),
                     selected = "False"),
        selectInput("holdset", "Hold Set:",
                    choices = c("Hold Set A", "Hold Set A | Hold Set B", "Hold Set B", 
                                "Original School Holds", "Original School Holds | Hold Set A", 
                                "Original School Holds | Hold Set A | Hold Set B",
                                "Original School Holds | Hold Set B"),
                    selected = "Hold Set A")
    ),
    hr(),
    div(style = "padding: 0 15px;",
        h4("Prese selezionate:"),
        verbatimTextOutput("selected_holds")
    )
  ),
  dashboardBody(
    fluidRow(class = "equal-height",
             # Colonna sinistra: Moonboard
             column(width = 4,
                    box(
                      title = "Moonboard:",
                      width = NULL,
                      status = "primary",
                      solidHeader = TRUE,
                      div(style = "overflow-x: auto;", uiOutput("moonboard_grid"))
                    )
             ),
             # Colonna destra: solo download + predizione
             column(width = 6,
                    box(
                      title = "Predizione della Difficoltà",
                      width = NULL,
                      status = "success",
                      solidHeader = TRUE,
                      uiOutput("prediction_ui"),
                      h4("Dettaglio Predizioni dei Modelli:"),
                      uiOutput("model_cards"),
                      verbatimTextOutput("prediction_details")
                    ),
                    box(
                      title = "Download Statistiche",
                      width = NULL,
                      status = "info",
                      solidHeader = TRUE,
                      downloadButton("download_features", "Scarica feature", class = "action-btn")
                    )
             )
    ),
    # Debug
    conditionalPanel(
      condition = "input.debug_button > 0",
      box(
        title = "Debug Informazioni",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        verbatimTextOutput("debug_info")
      )
    ),
    # CSS per allineare le colonne in altezza
    tags$style(HTML("
      .equal-height {
        display: flex;
        align-items: stretch;
      }
      .equal-height > div {
        display: flex;
        flex-direction: column;
      }
    "))
  )
)