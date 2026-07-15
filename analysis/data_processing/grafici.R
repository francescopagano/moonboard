library(ggplot2)
library(viridis)

# Boxplot per 'totale'
ggplot(stima_NONTOCCARE, aes(x = grade_v, y = totale, fill = grade_v)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_viridis_d(option = "inferno", direction = -1, guide = "none") +
  labs(x = "Grado (V)", y = "Totale") +
  theme_minimal(base_size = 14)

# Boxplot per 'aspect_ratio'
ggplot(stima_NONTOCCARE, aes(x = grade_v, y = aspect_ratio, fill = grade_v)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_viridis_d(option = "inferno", direction = -1, guide = "none") +
  labs(x = "Grado (V)", y = "Aspect Ratio") +
  theme_minimal(base_size = 14)

# Boxplot per 'bbox_h'
ggplot(stima_NONTOCCARE, aes(x = grade_v, y = bbox_h, fill = grade_v)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_viridis_d(option = "inferno", direction = -1, guide = "none") +
  labs(x = "Grado (V)", y = "Bounding Box Height") +
  theme_minimal(base_size = 14)

# Boxplot per 'bbox_w'
ggplot(stima_NONTOCCARE, aes(x = grade_v, y = bbox_w, fill = grade_v)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_viridis_d(option = "inferno", direction = -1, guide = "none") +
  labs(x = "Grado (V)", y = "Bounding Box Width") +
  theme_minimal(base_size = 14)


# Calcolo delle frequenze relative
freq_df <- as.data.frame(prop.table(table(stima_NONTOCCARE$holdsets)) * 100)
colnames(freq_df) <- c("holdsets", "perc")

# Ordina i livelli dal più frequente al meno frequente
freq_df <- freq_df[order(-freq_df$perc), ]
freq_df$holdsets <- factor(freq_df$holdsets, levels = freq_df$holdsets)

# Barplot orizzontale con colore grigio
ggplot(freq_df, aes(x = holdsets, y = perc)) +
  geom_bar(stat = "identity", width = 0.7, fill = "grey", alpha = 0.6) +
  coord_flip() +
  labs(x = "holdsets", y = "Percentuale (%)") +
  theme_minimal(base_size = 14)

# Istogrammi delle variabili in grigio
variabili <- c("totale", "var_x", "var_y", "aspect_ratio", "entropy")

for (var in variabili) {
  p <- ggplot(stima_NONTOCCARE, aes_string(x = var)) +
    geom_histogram(aes(y = ..count..), fill = "grey", color = "black", bins = 35, alpha = 0.6) +
    # geom_density(aes(y = ..count..), color = "black", size = 1, alpha = 0.6) + # se vuoi aggiungere la densità in nero
    labs(
      x = var,
      y = "Frequenza",
      title = ""
    ) +
    theme_minimal(base_size = 14)
  print(p)
}


