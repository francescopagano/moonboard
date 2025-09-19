library(shiny)
library(shinydashboard)
library(reticulate)
library(caret)
library(dplyr)

# Carica i pacchetti necessari per i modelli
library_list <- c("xgboost", "lightgbm", "nnet", "catboost")
for (lib in library_list) {
  if (requireNamespace(lib, quietly = TRUE)) {
    library(lib, character.only = TRUE)
  } else {
    warning(paste("Pacchetto", lib, "non disponibile. Alcuni modelli potrebbero non funzionare."))
  }
}

source_python("C:/Users/Francesco/Documents/uni/tesi/shiny/feature_engineering.py")  # File Python con il codice di feature engineering

# Carica i parametri di pre-processing
preprocessing_params <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/preprocessing_params.rds")

# Estrai i parametri necessari
var_quantitative <- preprocessing_params$var_quantitative
media_stima <- preprocessing_params$media_stima
sd_stima <- preprocessing_params$sd_stima
levels_map <- preprocessing_params$levels_map
single_level_train <- preprocessing_params$single_level_train

# Definisci la mappatura dei valori numerici ai gradi V
scale_vals <- c(4.61, 5.77, 6.92, 8.08, 9.23, 10.38, 11.54, 12.69, 13.84, 15.00, 16.15)
scale_codes <- c("V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14")

# Funzione per convertire valori numerici in gradi V
convert_to_v_grade <- function(numeric_val) {
  if (is.na(numeric_val) || is.null(numeric_val)) return(NA)
  # Trova l'indice del valore più vicino in scale_vals
  closest_idx <- which.min(abs(scale_vals - numeric_val))
  # Restituisci il corrispondente codice V
  return(scale_codes[closest_idx])
}

# Carica tutti i modelli con gestione errori migliorata

# PPR 1 -----------------------------
tryCatch({
  m.ppr1 <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/modelli/model_ppr1.rds")
  message("Modello PPR 1 caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_ppr1.rds: ", e$message)
  m.ppr1 <- NULL
})

# PPR 2 -----------------------------
tryCatch({
  m.ppr2 <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/modelli/model_ppr2.rds")
  message("Modello PPR 2 caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_ppr2.rds: ", e$message)
  m.ppr2 <- NULL
})

# NNET -------------------------------
tryCatch({
  m.net <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/modelli/model_nn.rds") 
  message("Modello NNET caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_nn.rds: ", e$message)
  m.net <- NULL
})

# Extreme Boosting (XGBoost) --------------------------
tryCatch({
  m.xgb <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/modelli/model_xgb.rds")
  message("Modello XGBoost caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_xgb.rds: ", e$message)
  m.xgb <- NULL
})

# Light Boosting (LightGBM) -----------------------------
tryCatch({
  m.light <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/modelli/model_lightboost.rds")
  message("Modello LightGBM caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_lightboost.rds: ", e$message)
  m.light <- NULL
})

# Cat Boosting (CatBoost) -------------------------------
tryCatch({
  m.cat <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/modelli/model_catboost.rds")
  message("Modello CatBoost caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_catboost.rds: ", e$message)
  m.cat <- NULL
})

tryCatch({
  model_colnames <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/model_colnames.rds")
  message("Nomi delle colonne del modello caricati con successo.")
}, error = function(e) {
  model_colnames <- NULL
  message("ATTENZIONE: File model_colnames.rds non trovato. Potrebbero esserci errori di predizione.")
})

model_df_colnames <- readRDS("C:/Users/Francesco/Documents/uni/tesi/shiny/model_df_colnames.rds")

align_dataframe <- function(new_df, expected_colnames) {
  
  # 1. Crea un dataframe "stampo" con una riga e le colonne giuste,
  #    riempito interamente di zeri.
  aligned_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(expected_colnames)))
  colnames(aligned_df) <- expected_colnames
  
  # 2. Trova quali colonne del dataframe "stampo" esistono anche nel nuovo dataframe.
  common_cols <- intersect(expected_colnames, colnames(new_df))
  
  # 3. Se ci sono colonne in comune, copia i loro valori dal nuovo dataframe
  #    allo "stampo", sovrascrivendo gli zeri.
  if (length(common_cols) > 0) {
    # Usiamo un loop per copiare colonna per colonna,
    # in modo da preservare i tipi di dato (es. fattori, numerici).
    for (col_name in common_cols) {
      aligned_df[[col_name]] <- new_df[[col_name]]
    }
  }
  
  # log_debug(paste("Dataframe allineato a", ncol(aligned_df), "colonne."))
  
  # 4. Restituisce il dataframe allineato, pronto per la previsione.
  return(aligned_df)
}

# Crea una lista di modelli validi (non NULL)
models <- list()
if (!is.null(m.ppr1)) models[["PPR_1"]] <- m.ppr1
if (!is.null(m.ppr2)) models[["PPR_2"]] <- m.ppr2
if (!is.null(m.net)) models[["NNET"]] <- m.net
if (!is.null(m.xgb)) models[["XGBoost"]] <- m.xgb
if (!is.null(m.light)) models[["LightGBM"]] <- m.light
if (!is.null(m.cat)) models[["CatBoost"]] <- m.cat


# Estrai variabili necessarie dai modelli che ne hanno bisogno (PPR e NNET)
extract_model_vars <- function(model) {
  if (!is.null(model) && !is.null(model$terms)) {
    vars <- all.vars(model$terms)
    # Rimuovi la variabile di risposta (solitamente 'y' o la prima)
    return(setdiff(vars, "y"))
  }
  return(NULL)
}

ppr1_vars <- extract_model_vars(m.ppr1)
ppr2_vars <- extract_model_vars(m.ppr2)
net_vars <- extract_model_vars(m.net)


# Valori min e max per il capping delle predizioni
miny <- 4.61
maxy <- 16.15

# Lista completa di tutti i possibili holds nel Moonboard 2016
all_possible_holds <- expand.grid(
  col = LETTERS[1:11],
  row = 1:18
) %>%
  mutate(hold_name = paste0(col, row)) %>%
  pull(hold_name)

# Definizione delle prese non standard del Moonboard 2016
non_standard_holds <- c(
  "A1", "A2", "A3", "A4", "A7", "A8", "A17", 
  "B1", "B2", "B5", "B14", "B17", 
  "C1", "C2", "C3", "C4", "C17", 
  "D1", "D2", "D4", 
  "E1", "E2", "E3", "E4", "E5", "E17", 
  "F1", "F2", "F3", "F4", "F17", "F18", 
  "G1", "G3", "G5", 
  "H1", "H2", "H3", "H4", "H6", "H17", 
  "I1", "I2", "I3", "I17", 
  "J1", "J3", "J4", "J15", "J17", "J18", 
  "K1", "K2", "K3", "K4", "K15", "K17"
)

standard_holds <- setdiff(all_possible_holds, non_standard_holds)

# Variabili che devono essere convertite in fattori
factor_variables <- c(
  all_possible_holds,
  "diag_1", "diag_2", 
  "mean_rowband_18_1", "mean_rowband_18_4", "mean_rowband_18_14", "mean_rowband_18_17",
  "mean_colband_11_1", "mean_colband_11_8", "mean_colband_11_9"
)

# Tema personalizzato minimal e moderno
minimal_theme <- tags$head(
  tags$style(HTML("
    body {
      font-family: 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif;
      background-color: #fafafa;
    }
    .skin-blue .main-header .logo {
      background-color: #222;
      font-weight: 300;
      font-size: 22px;
      letter-spacing: 1px;
    }
    .skin-blue .main-header .navbar {
      background-color: #222;
    }
    .skin-blue .main-sidebar {
      background-color: #fff;
      color: #444;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
    }
    .skin-blue .main-sidebar .sidebar a {
      color: #444;
    }
    /* Fix per il testo bianco su bianco */
    .skin-blue .sidebar-menu>li>a {
      color: #333;
    }
    .radio label, .checkbox label {
      color: #333;
    }
    .form-group label {
      color: #333;
      font-weight: 600;
    }
    .box {
      border-radius: 2px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      border: none;
    }
    .box-header {
      border-bottom: 1px solid #f0f0f0;
    }
    /* Cambiamo il colore delle intestazioni dei box in nero */
    .box.box-primary .box-header, .box.box-info .box-header {
      background-color: #222 !important;
    }
    .btn-default {
      border: none;
      background-color: #f8f8f8;
      border-radius: 3px;
      transition: background-color 0.2s;
    }
    .btn-default:hover {
      background-color: #eee;
    }
    .cell-btn {
      width: 45px;
      height: 45px;
      border-radius: 0;
      margin: 2px;
      border: none;
      outline: none;
      transition: all 0.2s;
    }
    .cell-btn-unselected {
      background-color: #f0f0f0;
      border: 1px solid #ddd;
    }
    .cell-btn-selected {
      background-color: #222;
    }
    .cell-btn-nonstandard {
      background-color: #e0e0e0;
      opacity: 0.2;
      position: relative;
      pointer-events: none;
    }
    /* Aggiungi un segno 'X' sulle prese non standard */
    .cell-btn-nonstandard::before,
    .cell-btn-nonstandard::after {
      content: '';
      position: absolute;
      width: 80%;
      height: 2px;
      background-color: #999;
      top: 50%;
      left: 10%;
      transform: translateY(-50%);
    }
    .cell-btn-nonstandard::before {
      transform: translateY(-50%) rotate(45deg);
    }
    .cell-btn-nonstandard::after {
      transform: translateY(-50%) rotate(-45deg);
    }
    .moonboard-grid {
      display: grid;
      grid-template-columns: repeat(11, 45px);
      grid-template-rows: repeat(18, 45px);
      gap: 4px;
      margin-bottom: 10px;
      background-color: #f9f9f9;
      padding: 10px;
      border-radius: 4px;
    }
    .row-labels {
      display: flex;
      flex-direction: column;
      margin-right: 8px;
      justify-content: space-between;
      height: calc((45px * 18) + (4px * 17));
    }
    .row-label {
      height: 45px;
      display: flex;
      align-items: center;
      justify-content: flex-end;
      font-weight: 600;
      color: #666;
      font-size: 14px;
    }
    .col-labels {
      display: flex;
      justify-content: space-between;
      margin-left: 36px; 
      margin-top: 8px;
      width: calc((45px * 11) + (4px * 10));
    }
    .col-label {
      width: 45px;
      text-align: center;
      font-weight: 600;
      color: #666;
      font-size: 14px;
    }
    .moonboard-container {
      display: flex;
      justify-content: center;
      padding: 20px 0;
    }
    .action-btn {
      border: none;
      background: #222;
      color: white;
      padding: 10px 20px;
      border-radius: 3px;
      margin: 5px 0;
      transition: background-color 0.2s;
    }
    .action-btn:hover {
      background: #444;
    }
    pre {
      background-color: #f8f8f8;
      border: none;
      padding: 10px;
      border-radius: 3px;
    }
    .grid-container {
      display: flex;
      flex-direction: column;
      align-items: center;
    }
    .grid-with-rows {
      display: flex;
      align-items: flex-start;
    }
    /* Stile per la visualizzazione dei modelli */
    .model-card {
      background: white;
      border-radius: 4px;
      padding: 15px;
      margin-bottom: 15px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }
    .model-title {
      font-size: 16px;
      font-weight: 600;
      margin-bottom: 5px;
      color: #333;
    }
    .model-value {
      font-size: 24px;
      font-weight: 700;
      color: #222;
    }
    .model-cards-container {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;
    }
    .model-card {
      flex: 1;
      min-width: 150px;
    }
    .majority-vote {
      background-color: #f0f9ff;
      border-left: 4px solid #0288d1;
    }
    .selected-params {
      margin-top: 15px;
      padding: 10px;
      background-color: #f8f8f8;
      border-radius: 4px;
    }
    /* Stile per il debug panel */
    .debug-panel {
      font-family: monospace;
      font-size: 12px;
      background-color: #f5f5f5;
      border: 1px solid #ddd;
      padding: 10px;
      white-space: pre-wrap;
    }
  "))
)

ui <- dashboardPage(
  dashboardHeader(title = "Moonboard Analyzer"),
  dashboardSidebar(
    minimal_theme,
    div(style = "padding: 15px;",
        actionButton("reset", "Reset selezione", class = "action-btn btn-block"),
        actionButton("compute_features", "Calcola Feature", class = "action-btn btn-block"),
        actionButton("predict", "Predici difficoltà", class = "action-btn btn-block"),
        # Aggiungiamo un bottone per il debug
        actionButton("debug_button", "Debug Modelli", class = "btn-block", style = "margin-top: 10px;")
    ),
    hr(),
    div(style = "padding: 0 15px;",
        # Selettore per isBenchmark
        radioButtons("is_benchmark", "È un Benchmark:",
                     choices = c("False" = "False", "True" = "True"),
                     selected = "False"),
        # Selettore per holdsets - adattato per essere coerente con i modelli
        selectInput("holdset", "Hold Set:",
                    choices = c("Hold Set A", 
                                "Hold Set A | Hold Set B", 
                                "Hold Set B",
                                "Hold Set B | Hold Set C",
                                "Hold Set C",
                                "Hold Set Original",
                                "Hold Set Original | Hold Set A"),
                    selected = "Hold Set Original")
    ),
    hr(),
    div(style = "padding: 0 15px;",
        h4("Prese selezionate:"),
        verbatimTextOutput("selected_holds")
    )
  ),
  dashboardBody(
    # NUOVA STRUTTURA: Parete e Feature affiancate
    fluidRow(
      # Prima colonna: Parete Moonboard
      column(width = 7,
             box(
               title = "Moonboard 2016",
               width = NULL,
               status = "primary", # Mantenuto primary ma sovrascriviamo con CSS
               solidHeader = TRUE,
               div(style = "overflow-x: auto;", uiOutput("moonboard_grid"))
             )
      ),
      # Seconda colonna: Feature Engineering
      column(width = 5,
             box(
               title = "Feature Engineering",
               width = NULL,
               status = "info", # Mantenuto info ma sovrascriviamo con CSS
               solidHeader = TRUE,
               verbatimTextOutput("feature_output"),
               downloadButton("download_features", "Scarica feature", class = "action-btn")
             )
      )
    ),
    # NUOVA SEZIONE: Risultati predizione in un box separato sotto
    fluidRow(
      box(
        title = "Predizione della Difficoltà",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        
        # Mostra la predizione principale (voto di maggioranza)
        uiOutput("prediction_ui"),
        
        # Mostra dettagli dei singoli modelli
        h4("Dettaglio Predizioni dei Modelli"),
        uiOutput("model_cards"),
        
        # Info aggiuntive
        verbatimTextOutput("prediction_details")
      )
    ),
    # Sezione di debug nascosta finché non si preme il bottone debug
    conditionalPanel(
      condition = "input.debug_button > 0",
      box(
        title = "Debug Informazioni",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        verbatimTextOutput("debug_info")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # All'avvio, verifichiamo che Python sia disponibile
  observe({
    tryCatch({
      # Verifica che Python sia disponibile
      py_available <- py_available()
      if (!py_available) {
        showNotification("Python non è disponibile. Verifica l'installazione.", type = "error", duration = NULL)
      } else {
        py_version <- py_version()
        showNotification(paste("Usando Python", py_version), type = "message")
      }
    }, error = function(e) {
      showNotification("Errore durante la verifica di Python. Verifica l'installazione.", type = "error", duration = NULL)
    })
  })
  
  # Dimensioni della matrice Moonboard
  rows <- LETTERS[1:11]  # A a K per colonne
  cols <- 18:1  # Da 18 a 1 per righe (dall'alto verso il basso)
  
  # Stato di selezione - ora 18x11 con la giusta orientazione
  selected_cells <- reactiveVal(matrix(FALSE, nrow = 18, ncol = 11))
  features_df <- reactiveVal(NULL)
  feature_names <- reactiveVal(NULL)
  predictions <- reactiveVal(NULL)
  
  # Dati preparati per i modelli
  model_data <- reactiveVal(NULL)   # Dataframe completo con fattori
  model_matrix <- reactiveVal(NULL) # Matrice puramente numerica per XGBoost, LightGBM, CatBoost
  ppr1_data <- reactiveVal(NULL)    # Dati specifici per PPR1
  ppr2_data <- reactiveVal(NULL)    # Dati specifici per PPR2
  net_data <- reactiveVal(NULL)     # Dati specifici per NNET
  cat_pool <- reactiveVal(NULL)     # Pool per CatBoost
  
  majority_vote <- reactiveVal(NULL)
  debug_text <- reactiveVal("")
  
  # Visualizza informazioni di debug
  output$debug_info <- renderPrint({
    cat(debug_text())
  })
  
  # Funzione per loggare informazioni di debug
  log_debug <- function(text) {
    current <- debug_text()
    debug_text(paste0(current, "\n", format(Sys.time(), "[%H:%M:%S]"), " ", text))
  }
  
  # Pulsante di debug - mostra informazioni sui modelli
  observeEvent(input$debug_button, {
    log_debug("=== INIZIO DEBUG ===")
    
    log_debug(paste("Numero di modelli caricati:", length(models)))
    for (i in seq_along(models)) {
      model_name <- names(models)[i]
      model <- models[[i]]
      log_debug(paste("Modello", i, "(", model_name, ") - Classe:", class(model)[1]))
    }
    
    # Verifica dati preparati
    if(!is.null(model_data())) log_debug(paste("model_data (dataframe) dimensions:", paste(dim(model_data()), collapse="x")))
    if(!is.null(model_matrix())) log_debug(paste("model_matrix (matrix) dimensions:", paste(dim(model_matrix()), collapse="x")))
    if(!is.null(ppr1_data())) log_debug(paste("ppr1_data dimensions:", paste(dim(ppr1_data()), collapse="x")))
    if(!is.null(ppr2_data())) log_debug(paste("ppr2_data dimensions:", paste(dim(ppr2_data()), collapse="x")))
    if(!is.null(net_data())) log_debug(paste("net_data dimensions:", paste(dim(net_data()), collapse="x")))
    if(!is.null(cat_pool())) log_debug("cat_pool è stato creato.")
    
    log_debug("=== FINE DEBUG ===")
  })
  
  # Genera la UI per la griglia Moonboard
  output$moonboard_grid <- renderUI({
    div(class = "moonboard-container",
        div(class = "grid-container",
            div(class = "grid-with-rows",
                div(class = "row-labels",
                    lapply(18:1, function(row_num) {
                      div(row_num, class = "row-label")
                    })
                ),
                div(
                  class = "moonboard-grid",
                  lapply(18:1, function(r_idx) {
                    row_num <- r_idx
                    lapply(1:11, function(c_idx) {
                      col_letter <- rows[c_idx]
                      cell_id <- paste0("cell_", row_num, "_", col_letter)
                      r_matrix <- 19 - row_num
                      is_selected <- selected_cells()[r_matrix, c_idx]
                      hold_name <- paste0(col_letter, row_num)
                      is_standard <- hold_name %in% standard_holds
                      
                      if (is_standard) {
                        actionButton(
                          inputId = cell_id,
                          label = "",
                          class = paste0("cell-btn ", if(is_selected) "cell-btn-selected" else "cell-btn-unselected")
                        )
                      } else {
                        div(class = "cell-btn cell-btn-nonstandard", style = "border: none;")
                      }
                    })
                  })
                )
            ),
            div(class = "col-labels",
                lapply(rows, function(col_label) {
                  div(col_label, class = "col-label")
                })
            )
        )
    )
  })
  
  # Gestire i click sulle celle
  observe({
    lapply(18:1, function(row_num) {
      lapply(1:11, function(c_idx) {
        col_letter <- rows[c_idx]
        hold_name <- paste0(col_letter, row_num)
        
        if (hold_name %in% standard_holds) {
          cell_id <- paste0("cell_", row_num, "_", col_letter)
          observeEvent(input[[cell_id]], {
            r_matrix <- 19 - row_num
            current <- selected_cells()
            current[r_matrix, c_idx] <- !current[r_matrix, c_idx]
            selected_cells(current)
          })
        }
      })
    })
  })
  
  # Reset della selezione
  observeEvent(input$reset, {
    selected_cells(matrix(FALSE, nrow = 18, ncol = 11))
    features_df(NULL)
    feature_names(NULL)
    predictions(NULL)
    model_data(NULL)
    model_matrix(NULL)
    ppr1_data(NULL)
    ppr2_data(NULL)
    net_data(NULL)
    cat_pool(NULL)
    majority_vote(NULL)
    debug_text("")
    updateRadioButtons(session, "is_benchmark", selected = "False")
    updateSelectInput(session, "holdset", selected = "Hold Set Original")
  })
  
  # Mostra celle selezionate
  output$selected_holds <- renderPrint({
    cells <- which(selected_cells() == TRUE, arr.ind = TRUE)
    
    if(nrow(cells) > 0) {
      holds <- sapply(1:nrow(cells), function(i) {
        r_matrix <- cells[i, 1]
        c_idx <- cells[i, 2]
        row_num <- 19 - r_matrix
        col_letter <- rows[c_idx]
        paste0(col_letter, row_num)
      })
      
      cat(paste(holds, collapse = ", "))
      cat("\n\n")
      cat(paste("isBenchmark:", input$is_benchmark, "\n"))
      cat(paste("Hold Set:", input$holdset))
    } else {
      cat("Nessuna presa selezionata")
    }
  })
  
  # Funzione di standardizzazione
  standardize_data <- function(df) {
    vars_to_scale <- intersect(var_quantitative, names(df))
    if (length(vars_to_scale) > 0) {
      for (var in vars_to_scale) {
        if (var %in% names(media_stima) && var %in% names(sd_stima) && sd_stima[var] > 0) {
          df[[var]] <- (df[[var]] - media_stima[var]) / sd_stima[var]
        }
      }
    }
    return(df)
  }
  
  # Prepara un dataframe per il modello
  prepare_model_data <- function(features, names) {
    base_data <- data.frame(matrix(features, nrow = 1))
    colnames(base_data) <- names
    
    df <- data.frame(matrix(0, nrow = 1, ncol = 0))
    
    # Imposta isBenchmark e holdset
    df$isBenchmark <- factor(input$is_benchmark, levels = levels_map[["isBenchmark"]] %||% c("False", "True"))
    df$holdsets <- factor(input$holdset, levels = levels_map[["holdsets"]] %||% c("Hold Set A", "Hold Set A | Hold Set B", "Hold Set B", "Hold Set B | Hold Set C", "Hold Set C", "Hold Set Original", "Hold Set Original | Hold Set A"))
    
    df <- cbind(df, base_data)
    
    # Aggiungi TUTTE le prese come colonne binarie
    for (hold in all_possible_holds) df[[hold]] <- 0
    
    cells <- which(selected_cells() == TRUE, arr.ind = TRUE)
    if(nrow(cells) > 0) {
      for(i in 1:nrow(cells)) {
        r_matrix <- cells[i, 1]
        c_idx <- cells[i, 2]
        row_num <- 19 - r_matrix
        col_letter <- rows[c_idx]
        hold_name <- paste0(col_letter, row_num)
        if (hold_name %in% names(df)) df[[hold_name]] <- 1
      }
    }
    
    # Converte le variabili in fattori
    for (var_name in factor_variables) {
      if (var_name %in% names(df)) {
        df[[var_name]] <- factor(df[[var_name]], levels = c("0", "1"))
      }
    }
    
    df <- standardize_data(df)
    log_debug(paste("Dataframe preparato con", ncol(df), "colonne."))
    return(df)
  }
  
  # Crea dataframe specifici per modelli basati su formula
  create_formula_specific_data <- function(df, vars_list, model_name) {
    if (is.null(vars_list) || length(vars_list) == 0) {
      log_debug(paste("Nessuna variabile specifica trovata per", model_name))
      return(NULL)
    }
    log_debug(paste("Creazione dati specifici per", model_name, "..."))
    
    missing_vars <- setdiff(vars_list, names(df))
    if (length(missing_vars) > 0) {
      log_debug(paste("Attenzione: variabili mancanti per", model_name, ":", paste(missing_vars, collapse=", ")))
      for(v in missing_vars) df[[v]] <- 0
    }
    
    specific_df <- df[, intersect(vars_list, names(df)), drop = FALSE]
    log_debug(paste("Dati", model_name, "creati con", ncol(specific_df), "colonne."))
    return(specific_df)
  }
  
  # Crea la matrice numerica per i modelli ad albero
  create_model_matrix <- function(df) {
    log_debug("Creazione matrice numerica con `model.matrix`...")
    
    # Rimuovi eventuali colonne che sono interamente NA, se ce ne fossero
    df <- df[, colSums(is.na(df)) < nrow(df)]
    
    # model.matrix crea una matrice numerica, gestendo i fattori con one-hot encoding.
    # La formula `~ . - 1` dice: "usa tutte le variabili (.) e non creare una colonna per l'intercetta (- 1)"
    # `contrasts.arg` assicura che i fattori siano trattati in modo standard
    factor_cols <- names(which(sapply(df, is.factor)))
    contrasts_list <- lapply(factor_cols, function(col) contrasts(df[[col]], contrasts = FALSE))
    names(contrasts_list) <- factor_cols
    
    tryCatch({
      mat <- model.matrix(~ . - 1, data = df, contrasts.arg = contrasts_list)
      
      # --- INIZIO ALLINEAMENTO COLONNE ---
      if (!is.null(model_colnames)) {
        # Crea una matrice vuota con le colonne giuste e riempila
        final_mat <- matrix(0, nrow = 1, ncol = length(model_colnames))
        colnames(final_mat) <- model_colnames
        
        # Trova le colonne in comune e riempi i valori
        common_cols <- intersect(model_colnames, colnames(mat))
        final_mat[1, common_cols] <- mat[1, common_cols]
        
        log_debug(paste("Matrice modello creata e allineata a", ncol(final_mat), "colonne."))
        return(final_mat)
        
      } else {
        log_debug("ATTENZIONE: Nomi colonne non trovati, uso il metodo standard.")
        log_debug(paste("Matrice modello creata con dimensioni:", paste(dim(mat), collapse = "x")))
        return(mat)
      }
      # --- FINE ALLINEAMENTO COLONNE ---
      
    }, error = function(e) {
      log_debug(paste("ERRORE durante la creazione di model.matrix:", e$message))
      df_numeric <- df
      for(col in names(df_numeric)){
        if(is.factor(df_numeric[[col]])){
          df_numeric[[col]] <- as.numeric(factor(df_numeric[[col]], levels = levels(df_numeric[[col]]))) - 1
        }
      }
      mat <- as.matrix(df_numeric)
      return(mat)
    })
  }
  
  # Feature engineering e preparazione dati
  observeEvent(input$compute_features, {
    if (sum(selected_cells()) == 0) {
      showNotification("Seleziona almeno una presa!", type = "error")
      return()
    }
    
    tryCatch({
      log_debug("Avvio calcolo feature e preparazione dati...")
      result <- compute_features(selected_cells())
      features <- result[[1]]
      names <- result[[2]]
      
      features_df(data.frame(Feature = names, Value = features))
      feature_names(names)
      
      # 1. Crea il dataframe completo con fattori
      df <- prepare_model_data(features, names)
      model_data(df)
      
      # 2. Crea la matrice puramente numerica da esso
      mat <- create_model_matrix(df)
      model_matrix(mat)
      
      # 3. Crea i dati specifici per ogni modello basato su formula
      ppr1_data(create_formula_specific_data(df, ppr1_vars, "PPR_1"))
      ppr2_data(create_formula_specific_data(df, ppr2_vars, "PPR_2"))
      net_data(create_formula_specific_data(df, net_vars, "NNET"))
      
      # 4. Crea il pool per CatBoost usando la matrice numerica
      if(!is.null(m.cat) && !is.null(mat)) {
        cat_pool(catboost.load_pool(data = mat))
        log_debug("CatBoost pool creato dalla matrice numerica.")
      }
      
      output$feature_output <- renderPrint({
        df_features <- features_df()
        if (!is.null(df_features)) {
          print(head(df_features, 20))
          cat("\nNumero totale di feature:", nrow(df_features))
        }
      })
      
      showNotification("Feature calcolate e dati pronti!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Errore nel feature engineering:", e$message), type = "error")
      log_debug(paste("ERRORE feature engineering:", e$message))
    })
  })
  
  # Predizione con gestione specifica per ogni tipo di modello
  observeEvent(input$predict, {
    if (sum(selected_cells()) == 0) {
      showNotification("Seleziona almeno una presa!", type = "error")
      return()
    }
    
    if (is.null(model_data())) {
      showNotification("Calcola prima le feature!", type = "warning")
      return()
    }
    
    model_predictions <- list()
    log_debug("Inizio predizioni...")
    
    # Ottieni i dati preparati
    df_ppr1 <- ppr1_data()
    df_ppr2 <- ppr2_data()
    df_net <- net_data()
    mat_model <- model_matrix()
    pool_cat <- cat_pool()
    
    tryCatch({
      # PPR 1
      if (!is.null(m.ppr1) && !is.null(df_ppr1)) {
        tryCatch({
          pred <- predict(m.ppr1, newdata = df_ppr1)
          model_predictions[["PPR_1"]] <- pmin(pmax(pred, miny), maxy)
          log_debug(paste("Predizione PPR_1:", model_predictions$PPR_1))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione PPR_1:", e$message)) 
          model_predictions[["PPR_1"]] <- NULL
        })
      }
      # PPR 2
      if (!is.null(m.ppr2) && !is.null(df_ppr2)) {
        tryCatch({
          pred <- predict(m.ppr2, newdata = df_ppr2)
          model_predictions[["PPR_2"]] <- pmin(pmax(pred, miny), maxy)
          log_debug(paste("Predizione PPR_2:", model_predictions$PPR_2))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione PPR_2:", e$message)) 
          model_predictions[["PPR_2"]] <- NULL
        })
      }
      # NNET
      if (!is.null(m.net) && !is.null(df_net)) {
        tryCatch({
          pred <- predict(m.net, newdata = df_net)
          model_predictions[["NNET"]] <- pmin(pmax(pred, miny), maxy)
          log_debug(paste("Predizione NNET:", model_predictions$NNET))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione NNET:", e$message))
          model_predictions[["NNET"]] <- NULL
        })
      }
      # XGBoost
      if (!is.null(m.xgb) && !is.null(mat_model)) {
        tryCatch({
          pred <- predict(m.xgb, newdata = mat_model)
          model_predictions[["XGBoost"]] <- pmin(pmax(pred, miny), maxy)
          log_debug(paste("Predizione XGBoost:", model_predictions$XGBoost))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione XGBoost:", e$message))
          model_predictions[["XGBoost"]] <- NULL
        })
      }
      # LightGBM
      if (!is.null(m.light) && !is.null(mat_model)) {
        tryCatch({
          pred <- predict(m.light, newdata = mat_model)
          model_predictions[["LightGBM"]] <- pmin(pmax(pred, miny), maxy)
          log_debug(paste("Predizione LightGBM:", model_predictions$LightGBM))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione LightGBM:", e$message))
          model_predictions[["LightGBM"]] <- NULL
        })
      }
      # CatBoost
      if (!is.null(m.cat) && !is.null(pool_cat)) {
        tryCatch({
          pred <- catboost.predict(m.cat, pool = pool_cat)
          model_predictions[["CatBoost"]] <- pmin(pmax(pred, miny), maxy)
          log_debug(paste("Predizione CatBoost:", model_predictions$CatBoost))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione CatBoost:", e$message))
          model_predictions[["CatBoost"]] <- NULL
        })
      }
      
      valid_preds <- Filter(Negate(is.null), model_predictions)
      
      if (length(valid_preds) > 0) {
        predictions(valid_preds)
        
        # Calcola il voto di maggioranza
        all_v_grades <- sapply(unlist(valid_preds), convert_to_v_grade)
        grade_table <- table(all_v_grades)
        most_freq_grade <- names(which.max(grade_table))
        numeric_vote <- scale_vals[which(scale_codes == most_freq_grade)]
        
        majority_vote(list(grade = most_freq_grade, numeric = numeric_vote))
        log_debug(paste("Voto di maggioranza:", most_freq_grade, "(", numeric_vote, ")"))
        
        # Aggiorna UI
        output$prediction_ui <- renderUI({
          vote_result <- majority_vote()
          if (is.null(vote_result)) return(NULL)
          div(
            h3("Risultato della predizione:"),
            div(class = "model-card majority-vote",
                h3(class = "model-title", "Difficoltà stimata"),
                p(class = "model-value", vote_result$grade)
            ),
            div(class = "selected-params",
                p(strong("Parametri selezionati:")),
                p("isBenchmark: ", input$is_benchmark),
                p("Hold Set: ", input$holdset)
            )
          )
        })
        
        output$model_cards <- renderUI({
          model_preds <- predictions()
          if (is.null(model_preds)) return(NULL)
          div(class = "model-cards-container",
              lapply(names(model_preds), function(model_name) {
                div(class = "model-card",
                    h4(class = "model-title", model_name),
                    p(class = "model-value", convert_to_v_grade(model_preds[[model_name]]))
                )
              })
          )
        })
        
        output$prediction_details <- renderPrint({
          model_preds <- predictions()
          vote_result <- majority_vote()
          if (is.null(model_preds)) return(NULL)
          
          pred_df <- data.frame(
            Modello = names(model_preds),
            Valore_Numerico = unlist(model_preds),
            Grado_V = sapply(unlist(model_preds), convert_to_v_grade)
          )
          row.names(pred_df) <- NULL
          
          cat("Predizioni dei singoli modelli:\n")
          print(pred_df)
          
          if (!is.null(vote_result)) {
            cat("\nVoto di maggioranza: ", vote_result$grade, " (", round(vote_result$numeric, 2), ")\n")
          }
        })
      } else {
        showNotification("Nessuna predizione valida ottenuta dai modelli.", type = "error")
      }
    }, error = function(e) {
      log_debug(paste("ERRORE generale nella predizione:", e$message))
      showNotification(paste("Errore nella predizione:", e$message), type = "error")
    })
  })
  
  # Download delle feature
  output$download_features <- downloadHandler(
    filename = function() {
      paste("moonboard_features_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      df <- features_df()
      if (!is.null(df)) {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)