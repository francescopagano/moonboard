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
    #df$isBenchmark <- factor(input$is_benchmark, levels = levels_map[["isBenchmark"]] %||% c("False", "True"))
    
    
    #df$holdsets <- factor(input$holdset, levels = levels_map[["holdsets"]] %||% c("Hold Set A", "Hold Set A | Hold Set B", "Hold Set B", 
                                                                                  #"Original School Holds", "Original School Holds | Hold Set A", "Original School Holds | Hold Set A | Hold Set B",
                                                                                  #"Original School Holds | Hold Set B"))
    
    levels_isBenchmark <- if (!is.null(levels_map[["isBenchmark"]])) levels_map[["isBenchmark"]] else c("False", "True")
    df$isBenchmark <- factor(input$is_benchmark, levels = levels_isBenchmark)
    
    levels_holdsets <- if (!is.null(levels_map[["holdsets"]])) levels_map[["holdsets"]] else c(
      "Hold Set A", "Hold Set A | Hold Set B", "Hold Set B", 
      "Original School Holds", "Original School Holds | Hold Set A", 
      "Original School Holds | Hold Set A | Hold Set B", 
      "Original School Holds | Hold Set B"
    )
    df$holdsets <- factor(input$holdset, levels = levels_holdsets)
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
    
    tolerance <- 1e-8
    
    for (var_name in names(levels_list)) {
      if (var_name %in% names(df)) {
        vals <- as.character(df[[var_name]])
        levels_var <- levels_list[[var_name]]
        
        # Prova a fare il match approx se NON trovi il valore tra i livelli
        for (i in seq_along(vals)) {
          val <- vals[i]
          # Se NA o già tra i livelli, ok
          if (is.na(val) || val %in% levels_var) next
          
          # Se numeric e livelli numerici, cerca approx
          val_num <- suppressWarnings(as.numeric(val))
          levels_num <- suppressWarnings(as.numeric(levels_var))
          if (!is.na(val_num) && any(!is.na(levels_num))) {
            diffs <- abs(levels_num - val_num)
            idx <- which.min(diffs)
            # Se la differenza è piccola, prendi quel livello
            if (!is.na(diffs[idx]) && diffs[idx] < tolerance) {
              vals[i] <- levels_var[idx]
              next
            }
          }
          # Se "unknown" è tra i livelli, usa quello
          if ("unknown" %in% levels_var) {
            vals[i] <- "unknown"
          } else {
            vals[i] <- NA
          }
        }
        
        # Debug: stampa valori non tra i livelli dopo correzione approx
        not_in_levels <- vals[!is.na(vals) & !(vals %in% levels_var)]
        if (length(not_in_levels) > 0) {
          print(sprintf("ATTENZIONE: %s ha valori non tra i livelli (dopo approx): %s", var_name, paste(not_in_levels, collapse=", ")))
        }
        
        df[[var_name]] <- factor(vals, levels = levels_var)
      }
    }
    
    df <- standardize_data(df)
    log_debug(paste("Osservazione pronta per il modello:\n", paste(capture.output(print(df)), collapse = "\n")))
    log_debug(paste("Dataframe preparato con", ncol(df), "colonne."))
    
    return(df)
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
      
      # 1. Crea il dataframe delle feature (una riga)
      df_raw <- prepare_model_data(features, names)
      #df = df_raw
      
      # 2. Allinea la struttura al template
      df <- build_observation(df_raw, model_template_df)
      # Imposta il nome della riga a '1' (se vuoi)
      rownames(df) <- "1"
      model_data(df)
      
      # 3. Crea la matrice numerica come sempre
      mat <- create_model_matrix(df)
      model_matrix(mat)
      
      log_debug("==== [DEBUG] Dataframe singola osservazione ====")
      log_debug(paste(capture.output(str(df)), collapse = "\n"))
      
      log_debug("==== [DEBUG] Contenuto osservazione ====")
      log_debug(paste(capture.output(print(df)), collapse = "\n"))
      
      na_cols <- names(df)[apply(df, 2, function(x) any(is.na(x)))]
      if (length(na_cols) > 0) {
        log_debug(paste("ATTENZIONE: Le seguenti colonne hanno NA:", paste(na_cols, collapse = ", ")))
        for (col in na_cols) {
          log_debug(paste("Colonna:", col, "-> valore:", as.character(df[1, col])))
        }
        # Correggi NA automaticamente
        for (col in na_cols) {
          if (is.factor(df[[col]])) {
            df[[col]][is.na(df[[col]])] <- levels(df[[col]])[1]
          } else {
            df[[col]][is.na(df[[col]])] <- 0
          }
        }
        log_debug("Correzione NA completata.")
        log_debug(paste(capture.output(print(df)), collapse = "\n"))
      } else {
        log_debug("Nessuna colonna ha valori NA.")
      }
      
      # Debug matrice numerica
      log_debug("==== [DEBUG] Matrice numerica singola osservazione ====")
      log_debug(paste(capture.output(str(mat)), collapse = "\n"))
      log_debug("==== [DEBUG] Contenuto matrice ====")
      log_debug(paste(capture.output(print(mat)), collapse = "\n"))
      
      #4. Crea il pool per CatBoost usando la matrice numerica
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
    
    df_p = model_data()
    mat_model <- model_matrix()
    pool_cat <- cat_pool()
    
    tryCatch({
      # PPR 1
      if (!is.null(m.ppr1) && !is.null(df_p)) {
        tryCatch({
          pred <- predict(m.ppr1, newdata = df_p)
          pred <- pmin(pmax(pred, miny), maxy)
          
          cat("y_calib:", str(y_calib), "\n")
          cat("y_calib_pred:", str(y_calib_pred_ppr1), "\n")
          cat("pred:", str(pred), "\n")
          intervals <- conformal_split(y_calib, y_calib_pred_ppr1, pred, alpha = 0.1)
          model_predictions[["PPR_1"]] <- intervals
          log_debug(paste("Predizione PPR_1:", intervals))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione PPR_1:", e$message)) 
          model_predictions[["PPR_1"]] <- NULL
        })
      }
      # PPR 2
      if (!is.null(m.ppr2) && !is.null(df_p)) {
        tryCatch({
          pred <- predict(m.ppr2, newdata = df_p)
          pred <- pmin(pmax(pred, miny), maxy)
          intervals <- conformal_split(y_calib, y_calib_pred_ppr2, pred, alpha = 0.1)
          model_predictions[["PPR_2"]] <- intervals
          log_debug(paste("Predizione PPR_2:", intervals))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione PPR_2:", e$message)) 
          model_predictions[["PPR_2"]] <- NULL
        })
      }
      # NNET
      if (!is.null(m.net) && !is.null(df_p)) {
        tryCatch({
          pred <- predict(m.net, newdata = df_p)
          pred <- pmin(pmax(pred, miny), maxy)
          intervals <- conformal_split(y_calib, y_calib_pred_nnet, pred, alpha = 0.1)
          model_predictions[["NNET"]] <- intervals
          log_debug(paste("Predizione NNET:", intervals))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione NNET:", e$message))
          model_predictions[["NNET"]] <- NULL
        })
      }
      # XGBoost
      if (!is.null(m.xgb) && !is.null(mat_model)) {
        tryCatch({
          pred <- predict(m.xgb, newdata = mat_model)
          pred <- pmin(pmax(pred, miny), maxy)
          
          intervals <- conformal_split(y_calib, y_calib_pred_xgb, pred, alpha = 0.1)
          model_predictions[["XGBoost"]] <- intervals
          log_debug(paste("Predizione XGBoost:", intervals))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione XGBoost:", e$message))
          model_predictions[["XGBoost"]] <- NULL
        })
      }
      # LightGBM
      if (!is.null(m.light) && !is.null(mat_model)) {
        tryCatch({
          pred <- predict(m.light, newdata = mat_model)
          pred <- pmin(pmax(pred, miny), maxy)
          intervals <- conformal_split(y_calib, y_calib_pred_lgb, pred, alpha = 0.1)
          model_predictions[["LightGBM"]] <- intervals
          log_debug(paste("Predizione LightGBM:", intervals))
        }, error = function(e) { 
          log_debug(paste("ERRORE predizione LightGBM:", e$message))
          model_predictions[["LightGBM"]] <- NULL
        })
      }
      # CatBoost
      if (!is.null(m.cat) && !is.null(pool_cat)) {
        tryCatch({
          pred <- catboost.predict(m.cat, pool = pool_cat)
          pred <- pmin(pmax(pred, miny), maxy)
          intervals <- conformal_split(y_calib, y_calib_pred_cat, pred, alpha = 0.1)
          model_predictions[["CatBoost"]] <- intervals
          log_debug(paste("Predizione CatBoost:", intervals))
        }, error = function(e) {
          log_debug(paste("ERRORE predizione CatBoost:", e$message))
          model_predictions[["CatBoost"]] <- NULL
        })
      }
      
      valid_preds <- Filter(Negate(is.null), model_predictions)
      
      if (length(valid_preds) > 0) {
        predictions(valid_preds)
        
        # Calcola il voto di maggioranza
        all_v_grades <- sapply(valid_preds, function(df) convert_to_v_grade(df$pred))
        grade_table <- table(all_v_grades)
        most_freq_grade <- names(which.max(grade_table))
        numeric_vote <- scale_vals[which(scale_codes == most_freq_grade)]
        majority_vote(list(grade = most_freq_grade, numeric = numeric_vote))
        log_debug(paste("Voto di maggioranza:", most_freq_grade))
        
        # Aggiorna UI
        output$prediction_ui <- renderUI({
          vote_result <- majority_vote()
          if (is.null(vote_result)) return(NULL)
          div(
            h3("Risultato della predizione:"),
            div(class = "model-card majority-vote",
                h3(class = "model-title", "Difficoltà stimata"),
                tags$p(class = "model-value", vote_result$grade)
            ),
            div(class = "selected-params",
                tags$p(strong("Parametri selezionati:")),
                tags$p("isBenchmark: ", input$is_benchmark),
                tags$p("Hold Set: ", input$holdset)
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
                    tags$p(class = "model-value", convert_to_v_grade(model_preds[[model_name]]$pred))
                )
              })
          )
        })
        
        output$prediction_details <- renderPrint({
          model_preds <- predictions()
          vote_result <- majority_vote()
          if (is.null(model_preds)) return(NULL)
          
          pred_df <- do.call(rbind, lapply(names(model_preds), function(model_name) {
            df <- model_preds[[model_name]]
            data.frame(
              Modello = model_name,
              Grado_consigliato = round(df$pred, 2),
              Grado_lower = round(df$lower, 2),
              Grado_upper = round(df$upper, 2),
              
              Grado_V = convert_to_v_grade(df$pred),
              Grado_V_lower = convert_to_v_grade(df$lower),
              Grado_V_upper = convert_to_v_grade(df$upper)
            )
          }))
          row.names(pred_df) <- NULL
          
          print(pred_df)
          
          if (!is.null(vote_result)) {
            cat("\nVoto di maggioranza: ", vote_result$grade, "\n")
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