if (!requireNamespace("reticulate", quietly = TRUE)) {
  install.packages("reticulate")
}

# Initialize Python environment
if (Sys.info()[["sysname"]] == "Linux") {
  reticulate::use_python("/usr/bin/python3")
}


library(shiny)
library(shinydashboard)
library(reticulate)
library(caret)
library(dplyr)
library(DT)
#library(rlang)

# Carica i pacchetti necessari per i modelli
library_list <- c("xgboost", "lightgbm", "nnet", "catboost")
for (lib in library_list) {
  if (requireNamespace(lib, quietly = TRUE)) {
    library(lib, character.only = TRUE)
  } else {
    warning(paste("Pacchetto", lib, "non disponibile. Alcuni modelli potrebbero non funzionare."))
  }
}

# # # Esempio: installazione automatica solo se non disponibile
# if (!requireNamespace("catboost", quietly = TRUE)) {
#    remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.8/catboost-R-windows-x86_64-1.2.8.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
# }

# Debug
print(getwd())
print(file.exists("python/feature_engineering.py"))

# Carica se esiste
if (file.exists("python/feature_engineering.py")) {
  source_python("python/feature_engineering.py")
} else {
  stop("feature_engineering.py non trovato nella cartella python/")
}

# Carica i parametri di pre-processing
preprocessing_params <- readRDS("data/preprocessing_params.rds")

# Estrai i parametri necessari
var_quantitative <- preprocessing_params$var_quantitative
media_stima <- preprocessing_params$media_stima
sd_stima <- preprocessing_params$sd_stima
levels_list = preprocessing_params$levels_list
levels_map = preprocessing_params$levels_list
single_level_train <- preprocessing_params$single_level_train

calib = preprocessing_params$calib
y_calib = preprocessing_params$y_calib
xmat.model.cal = preprocessing_params$xmat.model.cal
cal_pool = preprocessing_params$cal_pool

y_calib_pred_ppr1 = preprocessing_params$y_calib_pred_ppr1
y_calib_pred_ppr2 = preprocessing_params$y_calib_pred_ppr2
y_calib_pred_nnet = preprocessing_params$y_calib_pred_nnet
y_calib_pred_xgb = preprocessing_params$y_calib_pred_xgb
y_calib_pred_lgb = preprocessing_params$y_calib_pred_lgb
y_calib_pred_cat = preprocessing_params$y_calib_pred_cat

# Definisci la mappatura dei valori numerici ai gradi V
scale_vals <- c(4.61, 5.77, 6.92, 8.08, 9.23, 10.38, 11.54, 12.69, 13.84, 15.00, 16.15)
scale_codes <- c("V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14")

convert_to_v_grade <- function(x) {
  midpoints <- (head(scale_vals, -1) + tail(scale_vals, -1)) / 2
  breaks <- c(-Inf, midpoints, Inf)
  idx <- findInterval(x, breaks, rightmost.closed = TRUE)
  out <- rep(NA_character_, length(idx))
  K <- length(scale_codes)
  ok <- !is.na(idx) & idx >= 1 & idx <= K
  out[ok] <- scale_codes[idx[ok]]
  (out)
}

# Carica tutti i modelli con gestione errori migliorata

# PPR 1 -----------------------------
tryCatch({
  m.ppr1 <- readRDS("models/model_ppr1.rds")
  message("Modello PPR 1 caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_ppr1.rds: ", e$message)
  m.ppr1 <- NULL
})

# PPR 2 -----------------------------
tryCatch({
  m.ppr2 <- readRDS("models/model_ppr2.rds")
  message("Modello PPR 2 caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_ppr2.rds: ", e$message)
  m.ppr2 <- NULL
})

# NNET -------------------------------
tryCatch({
  m.net <- readRDS("models/model_nn.rds") 
  message("Modello NNET caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_nn.rds: ", e$message)
  m.net <- NULL
})

# Extreme Boosting (XGBoost) --------------------------
tryCatch({
  m.xgb <- readRDS("models/model_xgb.rds")
  message("Modello XGBoost caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_xgb.rds: ", e$message)
  m.xgb <- NULL
})

# Light Boosting (LightGBM) -----------------------------
tryCatch({
  m.light <- readRDS("models/model_lightboost.rds")
  message("Modello LightGBM caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_lightboost.rds: ", e$message)
  m.light <- NULL
})

#Cat Boosting (CatBoost) -------------------------------
tryCatch({
  m.cat <- readRDS("models/model_catboost.rds")
  message("Modello CatBoost caricato con successo")
}, error = function(e) {
  message("Errore nel caricare model_catboost.rds: ", e$message)
  m.cat <- NULL
})

tryCatch({
  model_colnames <- readRDS("data/model_colnames.rds")
  message("Nomi delle colonne del modello caricati con successo.")
}, error = function(e) {
  model_colnames <- NULL
  message("ATTENZIONE: File model_colnames.rds non trovato. Potrebbero esserci errori di predizione.")
})

model_df_colnames <- readRDS("data/model_df_colnames.rds")
input_vars <- setdiff(model_df_colnames, "y")

model_template_df = readRDS("data/model_template_df.rds")
model_template_df$y = NULL

build_observation <- function(df_input, template, tol = 1e-8) {
  find_nearest_level <- function(val, levels, tol = 1e-8) {
    levs_num <- suppressWarnings(as.numeric(levels))
    if (!is.na(val) && any(!is.na(levs_num))) {
      diffs <- abs(levs_num - as.numeric(val))
      idx <- which.min(diffs)
      if (diffs[idx] < tol) {
        return(levels[idx])
      }
    }
    if ("unknown" %in% levels) return("unknown")
    return(NA)
  }
  obs <- template[1, , drop = FALSE]
  for (col in colnames(template)) {
    if (col %in% names(df_input)) {
      val <- df_input[[col]]
      # ----> INSERISCI QUI LA STAMPA DI DEBUG <----
      print(sprintf("Colonna: %s, Valore input: %s, Livelli template: %s", 
                    col, as.character(val), paste(levels(template[[col]]), collapse = ", ")))
      # -------------------------------------------
      if (is.factor(template[[col]])) {
        new_val <- val
        if (!is.na(val) && !(val %in% levels(template[[col]]))) {
          new_val <- find_nearest_level(val, levels(template[[col]]), tol)
          if (is.na(new_val)) {
            warning(sprintf("Valore '%s' per la variabile '%s' NON è tra i livelli del template: %s. Verrà convertito in NA.", 
                            as.character(val), col, paste(levels(template[[col]]), collapse = ", ")))
          }
        }
        obs[[col]] <- factor(new_val, levels = levels(template[[col]]))
      } else {
        obs[[col]] <- val
      }
    }
  }
  obs
}

# Crea una lista di modelli validi (non NULL)
models <- list()
if (!is.null(m.ppr1)) models[["PPR_1"]] <- m.ppr1
if (!is.null(m.ppr2)) models[["PPR_2"]] <- m.ppr2
if (!is.null(m.net)) models[["NNET"]] <- m.net
if (!is.null(m.xgb)) models[["XGBoost"]] <- m.xgb
if (!is.null(m.light)) models[["LightGBM"]] <- m.light
#if (!is.null(m.cat)) models[["CatBoost"]] <- m.cat

# Valori min e max per il capping delle predizioni
miny <- 4.61
maxy <- 16.15

conformal_split <- function(y_calib, y_calib_pred, y_test_pred, alpha = 0.1) {
  # Calcola gli errori assoluti sulla calibrazione
  calib_residuals <- abs(y_calib - y_calib_pred)
  
  # Scegli il quantile desiderato
  q <- quantile(calib_residuals, probs = 1 - alpha)
  
  # Costruisci intervalli per le predizioni di test
  lower <- y_test_pred - q
  upper <- y_test_pred + q
  
  # Applica i limiti
  lower <- pmax(lower, miny)
  upper <- pmin(upper, maxy)
  
  intervals <- data.frame(lower = lower, pred = y_test_pred, upper = upper)
  return(intervals)
}

# Lista completa di tutti i possibili holds nel Moonboard 2016
all_possible_holds <- expand.grid(
  col = LETTERS[1:11],
  row = 1:18
) %>%
  mutate(hold_name = paste0(col, row)) %>%
  pull(hold_name)

# Definizione delle prese non standard del Moonboard 2016
non_standard_holds <- c(
  "A1", "A2", "A3", "A4", "A6", "A7", "A8", "A17", 
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

# 1. Crea la mappa dei livelli dai dati di training
unknown_label <- "unknown"
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
    .main-sidebar { 
      width: 250px; 
    }
    .content-wrapper, .main-footer, .right-side {
      margin-left: 250px;
    }
  "))
)