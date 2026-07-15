library(geojsonio)
library(jsonlite)
library(sf)
library(ggplot2)
library(dplyr)
library(Matrix)
library(stringr)
library(viridis)
library(ggnewscale)

# Funzioni globali ----

clean_vars_base <- function(df) {
  # Crea la versione "ripulita" del nome variabile
  var_nopoint <- gsub("_(\\d+)\\.\\d+", "_2", df$var)
  df$var_nopoint <- var_nopoint
  # Trova gli indici dei max abs(coeff) per ogni var_nopoint
  unique_vars <- unique(var_nopoint)
  idx <- sapply(unique_vars, function(v) {
    ind <- which(var_nopoint == v)
    abs_coeffs <- abs(df$coeff[ind])
    ind[which.max(abs_coeffs)]
  })
  
  # Estrai le righe corrispondenti e aggiorna il nome variabile
  df_clean <- df[idx, ]
  df_clean$var <- df_clean$var_nopoint
  df_clean$var_nopoint <- NULL
  
  # Rownames numerici
  rownames(df_clean) <- seq_len(nrow(df_clean))
  
  df_clean
}

block_clean_exact <- function(df) {
  # Estrai il nome gruppo fino a "_mean" (es: blocco3x3_0_0_mean)
  group_name <- sub("(_mean).*", "\\1", df$var)
  group_name <- sub("(_mean)", "\\1", df$var)
  group_name <- sub("(_mean).*", "\\1", df$var)
  # Oppure, più robusto:
  group_name <- sub("(_mean)[0-9\\.]+$", "\\1", df$var)
  
  # Trova l'indice di coefficiente assoluto massimo in ogni gruppo
  unique_groups <- unique(group_name)
  idx <- sapply(unique_groups, function(g) {
    ind <- which(group_name == g)
    abs_coeffs <- abs(df$coeff[ind])
    ind[which.max(abs_coeffs)]
  })
  
  # Estrai le righe corrispondenti e aggiorna il nome variabile
  df_clean <- df[idx, ]
  df_clean$var <- unique_groups
  
  # Rownames numerici
  rownames(df_clean) <- seq_len(nrow(df_clean))
  df_clean
}

blocchi_to_sf <- function(df_blocchi, grid_sf) {
  library(dplyr)
  library(stringr)
  library(sf)
  library(purrr)
  
  df2 <- df_blocchi %>%
    mutate(
      X = as.numeric(str_extract(var, "(?<=blocco)\\d+(?=x)")),   # righe
      Y = as.numeric(str_extract(var, "(?<=x)\\d+")),             # colonne
      I = as.numeric(sapply(str_split(var, "_"), `[`, 2)),        # riga di partenza (da 0)
      J = as.numeric(sapply(str_split(var, "_"), `[`, 3)),        # colonna di partenza (da 0)
      riga_min = I + 1,                                           # riga di partenza (in R, da 1)
      riga_max = I + X,                                           # riga finale INCLUSIVA
      col_min = J + 1,                                            # colonna di partenza (in R, da 1)
      col_max = J + Y,                                            # colonna finale INCLUSIVA
      mean = coeff
    ) %>%
    select(var, riga_min, riga_max, col_min, col_max, mean)
  
  blocchi_sf <- map(1:nrow(df2), function(i) {
    # Poligono: si va in senso orario, (x=colonna, y=riga)
    st_polygon(list(matrix(c(
      df2$col_min[i],     df2$riga_min[i],   # basso sx
      df2$col_max[i]+1,   df2$riga_min[i],   # basso dx (+1 inclusivo)
      df2$col_max[i]+1,   df2$riga_max[i]+1, # alto dx (+1 inclusivo)
      df2$col_min[i],     df2$riga_max[i]+1, # alto sx (+1 inclusivo)
      df2$col_min[i],     df2$riga_min[i]    # chiusura
    ), ncol=2, byrow=TRUE)))
  })
  
  st_sf(
    blocco = df2$var,
    mean = df2$mean,
    geometry = st_sfc(blocchi_sf),
    crs = st_crs(grid_sf)
  )
}

pal <- viridis(10, option = "inferno", direction = -1)
sub_pal <- pal

plot_blocchi <- function(blocchi_sf, grid_sf, df_col, df_row, title = "Blocchi macro-zone", fill_lab = "Media blocchi", sub_pal) {
  ggplot() +
    geom_sf(data = grid_sf, fill = "grey95", color = "grey80") +
    geom_sf(
      data = blocchi_sf,
      aes(fill = mean, alpha = !is.na(mean)),  # trasparente se NA
      color = NA,         # nessun bordo
      size = 1,
      show.legend = "fill"
    ) +
    # scale_fill_viridis_c(
    #   option = pal, direction = -1, na.value = NA, name = fill_lab
    # ) +
    scale_fill_gradientn(colors = sub_pal, na.value =  "transparent", name = fill_lab)+
    scale_alpha_manual(values = c("TRUE" = 0.8, "FALSE" = 0), guide = "none") +
    theme_void() +
    labs(title = title) +
    theme(
      legend.position = "right",  # legenda a destra
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.key.height = unit(2, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.margin = margin(0, 8, 0, 8),
      
      plot.title.position = "plot",
      plot.title = element_text(
        size = 15,
        hjust = 0.1,
        margin = margin(b = 5)
      )
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        barwidth = 1.5,   # stringe la barra (orizzontalmente)
        barheight = 7     # accorcia la barra (verticalmente)
      )
    ) +
    geom_text(
      data = df_col,
      aes(x = x, y = y, label = label),
      size = 4
    ) +
    geom_text(
      data = df_row,
      aes(x = x, y = y, label = label),
      size = 4
    ) +
    coord_sf(
      xlim = c(0, 12),
      ylim = c(0, 19),
      expand = FALSE
    )
}
# Parete ----

# Parametri della griglia
nx <- 11
ny <- 18

# Funzione per generare il nome della presa
presa_name <- function(x, y) {
  col <- LETTERS[x]      # Colonne da A a K
  row <- y               # Riga da 1 a 18
  paste0(col, row)
}

# Funzione per creare la lista dei poligoni con nome presa
create_pixel_geojson <- function(nx, ny) {
  features <- list()
  id <- 1
  for (i in 1:nx) {
    for (j in 1:ny) {
      coords <- list(list(
        c(i, j),
        c(i+1, j),
        c(i+1, j+1),
        c(i, j+1),
        c(i, j)
      ))
      features[[id]] <- list(
        type = "Feature",
        properties = list(
          x = i,
          y = j,
          pixel_id = id,
          nome_presa = presa_name(i, j)
        ),
        geometry = list(
          type = "Polygon",
          coordinates = coords
        )
      )
      id <- id + 1
    }
  }
  geojson <- list(
    type = "FeatureCollection",
    features = features
  )
  geojson_json <- jsonlite::toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)
  return(geojson_json)
}

# PPR (1) ----

df_ppr = as.data.frame(as.matrix(m.ppr$alpha))
colnames(df_ppr)[1] = "coeff"
df_ppr

df_ppr <- df_ppr %>%
  tibble::rownames_to_column("var")

df_ppr = df_ppr[,1:2]
coef_df_ppr = df_ppr

coef_df_ppr

coef_df_ppr$coeff

# 1. Oggetto prese (A-K, senza 1 finale)
prese <- coef_df_ppr %>%
  filter(grepl("^[A-K]\\d+$", var)) %>%
  mutate(var = ifelse(str_sub(var, -1) == "1", str_sub(var, 1, -2), var))

# 2. mean_rowband
mean_rowband_vars <- coef_df_ppr %>%
  filter(str_detect(var, "^mean_rowband"))

# 3. mean_colband
mean_colband_vars <- coef_df_ppr %>%
  filter(str_detect(var, "^mean_colband"))

# 4. Blocchi per tipo e solo quelli con "mean"
blocchi_6x4 <- coef_df_ppr %>% filter(str_detect(var, "^blocco6x4.*mean"))
blocchi_9x4 <- coef_df_ppr %>% filter(str_detect(var, "^blocco9x4.*mean"))
blocchi_6x2 <- coef_df_ppr %>% filter(str_detect(var, "^blocco6x2.*mean"))
blocchi_3x3 <- coef_df_ppr %>% filter(str_detect(var, "^blocco3x3.*mean"))

blocchi_6x3 <- coef_df_ppr %>% filter(str_detect(var, "^blocco6x3.*mean"))
blocchi_9x2 <- coef_df_ppr %>% filter(str_detect(var, "^blocco9x2.*mean"))

# Applica la funzione a tutti i tuoi df
mean_rowband_vars <- clean_vars_base(mean_rowband_vars)
mean_colband_vars <- clean_vars_base(mean_colband_vars)
blocchi_6x4 <- block_clean_exact(blocchi_6x4)
blocchi_9x4 <- block_clean_exact(blocchi_9x4)
blocchi_6x2 <- block_clean_exact(blocchi_6x2)
blocchi_3x3 <- block_clean_exact(blocchi_3x3)

blocchi_6x3 <- block_clean_exact(blocchi_6x3)
blocchi_9x2 <- block_clean_exact(blocchi_9x2)

rownames(prese) <- seq_len(nrow(prese))
rownames(mean_rowband_vars) <- seq_len(nrow(mean_rowband_vars))
rownames(mean_colband_vars) <- seq_len(nrow(mean_colband_vars))

rownames(blocchi_6x4) <- seq_len(nrow(blocchi_6x4))
rownames(blocchi_9x4) <- seq_len(nrow(blocchi_9x4))
rownames(blocchi_6x2) <- seq_len(nrow(blocchi_6x2))
rownames(blocchi_3x3) <- seq_len(nrow(blocchi_3x3))
rownames(blocchi_6x3) <- seq_len(nrow(blocchi_6x3))
rownames(blocchi_9x2) <- seq_len(nrow(blocchi_9x2))

# max(prese$coeff)
# min(prese$coeff)
# 
# prese$coeff[prese$coeff >= -0.02 & prese$coeff <= 0.02] <- NA
# prese
# 
# mean_rowband_vars$coeff[mean_rowband_vars$coeff >= -0.02 & mean_rowband_vars$coeff <= 0.02] <- NA
# mean_colband_vars$coeff[mean_colband_vars$coeff >= -0.02 & mean_colband_vars$coeff <= 0.02] <- NA
# 
# blocchi_6x4$coeff[blocchi_6x4$coeff >= -0.02 & blocchi_6x4$coeff <= 0.02] <- NA
# blocchi_9x4$coeff[blocchi_9x4$coeff >= -0.02 & blocchi_9x4$coeff <= 0.02] <- NA
# blocchi_6x2$coeff[blocchi_6x2$coeff >= -0.02 & blocchi_6x2$coeff <= 0.02] <- NA
# blocchi_3x3$coeff[blocchi_3x3$coeff >= -0.02 & blocchi_3x3$coeff <= 0.02] <- NA
# blocchi_6x3$coeff[blocchi_6x3$coeff >= -0.02 & blocchi_6x3$coeff <= 0.02] <- NA
# blocchi_9x2$coeff[blocchi_9x2$coeff >= -0.02 & blocchi_9x2$coeff <= 0.02] <- NA

# mean_rowband_vars$coeff[mean_rowband_vars$coeff == 0] <- NA
# mean_colband_vars$coeff[mean_colband_vars$coeff == 0] <- NA
# 
# blocchi_6x4$coeff[blocchi_6x4$coeff == 0] <- NA
# blocchi_9x4$coeff[blocchi_9x4$coeff == 0] <- NA
# blocchi_6x2$coeff[blocchi_6x2$coeff == 0] <- NA
# blocchi_3x3$coeff[blocchi_3x3$coeff == 0] <- NA
# blocchi_6x3$coeff[blocchi_6x3$coeff == 0] <- NA
# blocchi_9x2$coeff[blocchi_9x2$coeff == 0] <- NA

q_low  <- quantile(coef_df_ppr$coeff, 0.15)
q_high <- quantile(coef_df_ppr$coeff, 0.85)

prese$coeff[prese$coeff >= q_low & prese$coeff <= q_high] <- NA
mean_rowband_vars$coeff[mean_rowband_vars$coeff >= q_low & mean_rowband_vars$coeff <= q_high] <- NA
mean_colband_vars$coeff[mean_colband_vars$coeff >= q_low & mean_colband_vars$coeff <= q_high] <- NA

blocchi_6x4$coeff[blocchi_6x4$coeff >= q_low & blocchi_6x4$coeff <= q_high] <- NA
blocchi_9x4$coeff[blocchi_9x4$coeff >= q_low & blocchi_9x4$coeff <= q_high] <- NA
blocchi_6x2$coeff[blocchi_6x2$coeff >= q_low & blocchi_6x2$coeff <= q_high] <- NA
blocchi_3x3$coeff[blocchi_3x3$coeff >= q_low & blocchi_3x3$coeff <= q_high] <- NA
blocchi_6x3$coeff[blocchi_6x3$coeff >= q_low & blocchi_6x3$coeff <= q_high] <- NA
blocchi_9x2$coeff[blocchi_9x2$coeff >= q_low & blocchi_9x2$coeff <= q_high] <- NA

## Creazione parete ----

# Genera GeoJSON con nomi prese
geojson_grid <- create_pixel_geojson(nx, ny)
write(geojson_grid, "parete_arrampicata_grid.geojson")

# Carica il GeoJSON
grid_sf <- st_read("parete_arrampicata_grid.geojson", quiet = TRUE)

# Unisci i coefficienti alla griglia tramite il nome presa
grid_sf <- grid_sf %>%
  left_join(prese, by = c("nome_presa" = "var"))


## Creazione etichette ----

# Etichette colonne (A-K)
df_col <- data.frame(
  x = 1:11 + 0.5,         # posizione centrale di ogni colonna
  y = 0.5,                # appena sotto la griglia
  label = LETTERS[1:11]
)

# Etichette righe (18-1)
df_row <- data.frame(
  x = 0.3,                # appena a sinistra della griglia
  y = 18:1 + 0.5,         # posizione centrale di ogni riga
  label = as.character(18:1)
)

## 1. BANDE ORIZZONTALI ----

mean_rowband_vars

# Supponiamo che nx, ny, grid_sf siano già definiti

# 1. Estrai N, I dai nomi delle variabili
mean_rowband_vars <- mean_rowband_vars %>%
  mutate(
    N = as.numeric(str_match(var, "^mean_rowband_(\\d+)_")[,2]),
    I = as.numeric(str_match(var, "^mean_rowband_\\d+_(\\d+)$")[,2])
  )

# 2. Ciclo su tutti i valori unici di N

band_sf_list <- lapply(unique(mean_rowband_vars$N), function(n_bande) {
  this_n <- mean_rowband_vars %>% filter(N == n_bande) %>% arrange(I)
  righe_per_banda <- ny / n_bande
  
  # Crea banda, y_min, y_max
  band_df <- data.frame(
    I = 1:n_bande,
    banda = paste0("rowband_", 1:n_bande),
    y_min = seq(1, ny, by = righe_per_banda),
    y_max = seq(righe_per_banda, ny, by = righe_per_banda)
  )
  
  band_df <- merge(
    band_df,
    data.frame(I = this_n$I, mean = this_n$coeff),
    by = "I",
    all.x = TRUE
  )
  
  band_df$mean[is.na(band_df$mean)] <- NA
  
  # Crea poligoni
  band_polys <- lapply(1:nrow(band_df), function(i) {
    st_polygon(list(matrix(c(
      1, band_df$y_min[i],
      nx+1, band_df$y_min[i],
      nx+1, band_df$y_max[i]+1,
      1, band_df$y_max[i]+1,
      1, band_df$y_min[i]
    ), ncol=2, byrow=TRUE)))
  })
  
  st_sf(
    banda = band_df$banda,
    mean = band_df$mean,
    geometry = st_sfc(band_polys),
    crs = st_crs(grid_sf)
  )
})

# 3. Dai nomi agli oggetti della lista
names(band_sf_list) <- paste0("band_sf_", unique(mean_rowband_vars$N))

## 2. BANDE VERTICALI ----

# Supponiamo che nx, ny, grid_sf siano già definiti

# 1. Estrai N, I dai nomi delle variabili
mean_colband_vars <- mean_colband_vars %>%
  mutate(
    N = as.numeric(str_match(var, "^mean_colband_(\\d+)_")[,2]),
    I = as.numeric(str_match(var, "^mean_colband_\\d+_(\\d+)$")[,2])
  )

# 2. Ciclo su tutti i valori unici di N
colband_sf_list <- lapply(unique(mean_colband_vars$N), function(n_bande) {
  this_n <- mean_colband_vars %>% filter(N == n_bande) %>% arrange(I)
  col_per_banda <- nx / n_bande
  
  # Crea banda, x_min, x_max
  band_df <- data.frame(
    I = 1:n_bande,
    banda = paste0("colband_", 1:n_bande),
    x_min = seq(1, nx, by = col_per_banda),
    x_max = seq(col_per_banda, nx, by = col_per_banda)
  )
  
  band_df <- merge(
    band_df,
    data.frame(I = this_n$I, mean = this_n$coeff),
    by = "I",
    all.x = TRUE
  )
  
  band_df$mean[is.na(band_df$mean)] <- NA
  
  # Crea poligoni
  band_polys <- lapply(1:nrow(band_df), function(i) {
    st_polygon(list(matrix(c(
      band_df$x_min[i], 1,
      band_df$x_max[i]+1, 1,
      band_df$x_max[i]+1, ny+1,
      band_df$x_min[i], ny+1,
      band_df$x_min[i], 1
    ), ncol=2, byrow=TRUE)))
  })
  
  st_sf(
    banda = band_df$banda,
    mean = band_df$mean,
    geometry = st_sfc(band_polys),
    crs = st_crs(grid_sf)
  )
})

# 3. Dai nomi agli oggetti della lista
names(colband_sf_list) <- paste0("colband_sf_", unique(mean_colband_vars$N))

## 3. BLOCCHI (MACROZONE) ----

df2 <- blocchi_3x3 %>%
  mutate(
    X = as.numeric(str_extract(var, "\\d+(?=x)")),
    Y = as.numeric(str_extract(var, "(?<=x)\\d+")),
    # split per estrarre I e J
    I = as.numeric(sapply(str_split(var, "_"), function(x) x[2])),
    J = as.numeric(sapply(str_split(var, "_"), function(x) x[3])),
    x_min = J+1,
    x_max = J + X,
    y_min = I+1,
    y_max = I + Y
  ) %>%
  select(var, x_min, x_max, y_min, y_max, coeff)
df2

blocchi_sf <- lapply(1:nrow(df2), function(i) {
  st_polygon(list(matrix(c(
    df2$x_min[i], df2$y_min[i],
    df2$x_max[i]+1, df2$y_min[i],
    df2$x_max[i]+1, df2$y_max[i]+1,
    df2$x_min[i], df2$y_max[i]+1,
    df2$x_min[i], df2$y_min[i]
  ), ncol=2, byrow=TRUE)))
})

blocchi_sf <- st_sf(
  blocco = df2$var,
  coef = df2$coef,
  geometry = st_sfc(blocchi_sf),
  crs = st_crs(grid_sf)
)
blocchi_sf

sf_blocchi_3x3 <- blocchi_to_sf(blocchi_3x3, grid_sf)
sf_blocchi_6x4 <- blocchi_to_sf(blocchi_6x4, grid_sf)
sf_blocchi_6x2 <- blocchi_to_sf(blocchi_6x2, grid_sf)
sf_blocchi_9x4 <- blocchi_to_sf(blocchi_9x4, grid_sf)

sf_blocchi_6x3 <- blocchi_to_sf(blocchi_6x3, grid_sf)
sf_blocchi_9x2 <- blocchi_to_sf(blocchi_9x2, grid_sf)

## GRAFICI ----

# Prese singole

p1 <- ggplot() +
  geom_sf(data = grid_sf, aes(fill = coeff), color = "grey80") +
  # scale_fill_viridis_c(
  #   option = "rocket", direction = -1, na.value = "grey95", name = "Coefficiente Prese"
  # ) +
  #scale_fill_distiller(palette = "RdGy", na.value = "grey95")+
  scale_fill_gradientn(colors = sub_pal, na.value =  "transparent", name = "Coefficiente Prese")+
  theme_void() +
  labs(title = "Intensità coefficienti delle prese - PPR (1)") +
  theme(
    legend.position = "right",  # legenda a destra
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(0, 8, 0, 8),
    
    plot.title.position = "plot",
    plot.title = element_text(
      size = 15,
      hjust = 0.15,
      margin = margin(b = 5)
    )
  ) +
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = 1.5,   # stringe la barra (orizzontalmente)
      barheight = 7     # accorcia la barra (verticalmente)
    )
  ) +
  geom_text(
    data = df_col,
    aes(x = x, y = y, label = label),
    size = 4
  ) +
  geom_text(
    data = df_row,
    aes(x = x, y = y, label = label),
    size = 4
  ) +
  coord_sf(
    xlim = c(0, 12),
    ylim = c(0, 19),
    expand = FALSE
  )
p1

# Bande Orizzontali

for(n in names(band_sf_list)) {
  p <- ggplot() +
    geom_sf(data = grid_sf, fill = "transparent", color = "grey80") +
    geom_sf(
      data = band_sf_list[[n]],
      aes(fill = mean, alpha = !is.na(mean)),
      color = NA
    ) +
    # scale_fill_viridis_c(
    #   option = "rocket", direction = -1, na.value = NA,
    #   name = paste("Beta bande orizzontali (N =", gsub("band_sf_", "", n), ")")
    # ) +
    scale_fill_gradientn(colors = sub_pal, na.value =  "transparent",
                         name = paste("Beta bande orizzontali (N =", gsub("band_sf_", "", n), ")"))+
    scale_alpha_manual(values = c("TRUE" = 0.8, "FALSE" = 0), guide = "none") +
    theme_void() +
    labs(title = paste("Bande orizzontali (righe) - N =", gsub("band_sf_", "", n))) +
    theme(
      legend.position = "right",  # legenda a destra
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.key.height = unit(2, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.margin = margin(0, 8, 0, 8),
      
      plot.title.position = "plot",
      plot.title = element_text(
        size = 15,
        hjust = 0.1,
        margin = margin(b = 5)
      )
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        barwidth = 1.5,   # stringe la barra (orizzontalmente)
        barheight = 7     # accorcia la barra (verticalmente)
      )
    ) +
    geom_text(
      data = df_col,
      aes(x = x, y = y, label = label),
      size = 4
    ) +
    geom_text(
      data = df_row,
      aes(x = x, y = y, label = label),
      size = 4
    ) +
    coord_sf(
      xlim = c(0, 12),
      ylim = c(0, 19),
      expand = FALSE
    )
  print(p)
}

# Bande Verticali

for(n in names(colband_sf_list)) {
  p <- ggplot() +
    geom_sf(data = grid_sf, fill = "transparent", color = "grey80") +
    geom_sf(
      data = colband_sf_list[[n]],
      aes(fill = mean, alpha = !is.na(mean)),
      color = NA
    ) +
    # scale_fill_viridis_c(
    #   option = "rocket", direction = -1, na.value = NA,
    #   name = paste("Beta bande verticali (N =", gsub("colband_sf_", "", n), ")")
    # ) +
    scale_fill_gradientn(colors = sub_pal, na.value =  "transparent",
                         name = paste("Beta bande verticali (N =", gsub("colband_sf_", "", n), ")"))+
    scale_alpha_manual(values = c("TRUE" = 0.8, "FALSE" = 0), guide = "none") +
    theme_void() +
    labs(title = paste("Bande verticali (colonne) - N =", gsub("colband_sf_", "", n))) +
    theme(
      legend.position = "right",  # legenda a destra
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.key.height = unit(2, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.margin = margin(0, 8, 0, 8),
      
      plot.title.position = "plot",
      plot.title = element_text(
        size = 15,
        hjust = 0.125,
        margin = margin(b = 5)
      )
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        barwidth = 1.5,   # stringe la barra (orizzontalmente)
        barheight = 7     # accorcia la barra (verticalmente)
      )
    ) +
    geom_text(
      data = df_col,
      aes(x = x, y = y, label = label),
      size = 4
    ) +
    geom_text(
      data = df_row,
      aes(x = x, y = y, label = label),
      size = 4
    ) +
    coord_sf(
      xlim = c(0, 12),
      ylim = c(0, 19),
      expand = FALSE
    )
  print(p)
}

# Blocchi 3x3
p_blocchi_3x3 <- plot_blocchi(sf_blocchi_3x3, grid_sf, df_col, df_row,
                              title = "Blocchi 3x3 - PPR (1)", fill_lab = "Coefficiente blocchi 3x3", sub_pal)
print(p_blocchi_3x3)

# Blocchi 6x4
p_blocchi_6x4 <- plot_blocchi(sf_blocchi_6x4, grid_sf, df_col, df_row,
                              title = "Blocchi 6x4 - PPR (1)", fill_lab = "Coefficiente blocchi 6x4", sub_pal)
print(p_blocchi_6x4)

# Blocchi 6x2
p_blocchi_6x2 <- plot_blocchi(sf_blocchi_6x2, grid_sf, df_col, df_row,
                              title = "Blocchi 6x2 - PPR (1)", fill_lab = "Coefficiente blocchi 6x2", sub_pal)
print(p_blocchi_6x2)

# Blocchi 9x4
p_blocchi_9x4 <- plot_blocchi(sf_blocchi_9x4, grid_sf, df_col, df_row,
                              title = "Blocchi 9x4 - PPR (1)", fill_lab = "Coefficiente blocchi 9x4", sub_pal)
print(p_blocchi_9x4)

# Blocchi 6x2
p_blocchi_6x3 <- plot_blocchi(sf_blocchi_6x3, grid_sf, df_col, df_row,
                              title = "Blocchi 6x3 - PPR (1)", fill_lab = "Coefficiente blocchi 6x3", sub_pal)
print(p_blocchi_6x3)

# Blocchi 9x2
p_blocchi_9x2 <- plot_blocchi(sf_blocchi_9x2, grid_sf, df_col, df_row,
                              title = "Blocchi 9x2 - PPR (1)", fill_lab = "Coefficiente blocchi 9x2", sub_pal)
print(p_blocchi_9x2)

plot(m.ppr, main = "")


### VISUALIZZAZIONE INTEGRATA ----

# blocchi_sf <- sf_blocchi_3x3          # Blocchi 3x3 (puoi cambiare con sf_blocchi_6x4, ecc)
# band_sf <- band_sf_list$band_sf_18     # Bande orizzontali N=6 (puoi cambiare N)
# colband_sf <- colband_sf_list$colband_sf_11  # Bande verticali N=4 (puoi cambiare N)
# 
# ggplot() +
#   geom_sf(data = grid_sf, aes(fill = coeff), color = "grey80", alpha = 2) +
#   scale_fill_gradientn(colors = sub_pal, na.value =  "transparent", name = "Coefficiente Prese")+
#   #scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey95", name = "Coefficiente Prese") +
#   ggnewscale::new_scale_fill() +
#   
#   geom_sf(data = band_sf, aes(fill = mean), alpha = 0.3, color = NA) +
#   scale_fill_gradientn(colors = sub_pal, na.value =  "transparent", name = "Coefficiente Bande Orizzontali")+
#   #scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey95", name = "Coefficiente Bande Orizzontali") +
#   ggnewscale::new_scale_fill() +
#   
#   geom_sf(data = colband_sf, aes(fill = mean), alpha = 0.3, color = NA) +
#   scale_fill_gradientn(colors = sub_pal, na.value =  "transparent", name = "Coefficiente Bande Verticali")+
#   #scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey95", name = "Coefficiente Bande Verticali") +
#   ggnewscale::new_scale_fill() +
#   
#   geom_sf(
#     data = blocchi_sf,
#     aes(fill = mean, alpha = !is.na(mean)),  # alpha 0 se NA
#     color = NA, 
#     size = 1.2
#   ) +
#   scale_fill_gradientn(colors = sub_pal, na.value =  "transparent", name = "Coefficiente Blocchi")+
#   #scale_fill_viridis_c(option = "rocket", direction = -1, na.value = NA, name = "Coefficiente Blocchi") +
#   scale_alpha_manual(values = c("TRUE" = 0.2, "FALSE" = 0), guide = "none") +
#   theme_void() +
#   labs(title = "Prese, sezioni e blocchi sovrapposti") +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 12),
#     legend.text = element_text(size = 10)
#   ) +
#   geom_text(
#     data = df_col,
#     aes(x = x, y = y, label = label),
#     size = 4
#   ) +
#   geom_text(
#     data = df_row,
#     aes(x = x, y = y, label = label),
#     size = 4
#   ) +
#   coord_sf(
#     xlim = c(0, 12),
#     ylim = c(0, 19),
#     expand = FALSE
#   )

blocchi_sf1 = sf_blocchi_6x2          # Blocchi 3x3 (puoi cambiare con sf_blocchi_6x4, ecc)
blocchi_sf2 = sf_blocchi_9x2

# Bande orizzontali N=6 (puoi cambiare N)
band_sf1 <- band_sf_list$band_sf_18
band_sf2 = band_sf_list$band_sf_9

colband_sf1 <- colband_sf_list$colband_sf_11 # Bande verticali N=4 (puoi cambiare N)

library(dplyr)
library(sf)

# Unisci tutti i dati con bind_rows (aggiungi una colonna 'layer')
df_plot <- bind_rows(
  band_sf1 %>% mutate(layer = "Bande Orizzontali"),
  band_sf2 %>% mutate(layer = "Bande Orizzontali"),
  colband_sf1 %>% mutate(layer = "Bande Verticali"),
  blocchi_sf1 %>% mutate(layer = "Blocchi"),
  blocchi_sf2 %>% mutate(layer = "Blocchi")
)
range_means <- range(df_plot$mean, na.rm = TRUE)
# Se serve anche grid_sf, aggiungilo sopra
# grid_sf %>% mutate(layer = "Prese")

ggplot(df_plot) +
  #Primo layer: no alpha manuale, solo fill
  geom_sf(data = grid_sf, color = "grey87", alpha = 1) +
  #geom_sf(data = grid_sf, aes(fill=coeff), color = "grey85", alpha = 1) +
  #scale_fill_gradientn(colors = sub_pal, na.value = "transparent", name = "Coefficiente Prese", limits = range_means) +
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = mean, alpha = !is.na(mean)), color = NA, size = 1.2) +
  scale_fill_gradientn(colors = sub_pal, na.value = "transparent", name = "Coefficiente", limits = range_means) +
  scale_alpha_manual(values = c("TRUE" = 0.5, "FALSE" = 0), guide = "none") +
  theme_void() +
  labs(title = "") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  geom_text(data = df_col, aes(x = x, y = y, label = label), size = 4) +
  geom_text(data = df_row, aes(x = x, y = y, label = label), size = 4) +
  coord_sf(xlim = c(0, 12), ylim = c(0, 19), expand = FALSE)
