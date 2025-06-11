# Descrizione delle feature estratte per ogni tracciato Moonboard

Ogni osservazione rappresenta un tracciato di arrampicata su Moonboard 2016 (matrice 18x11).  
I valori 1 indicano le prese utilizzate nel tracciato.

| Nome Feature        | Descrizione                                                                                  |
|---------------------|---------------------------------------------------------------------------------------------|
| **totale**          | Numero totale di prese usate nel tracciato                                                  |
| **densita**         | Densità di prese usate (totale / 198)                                                      |
| **centroide_y**     | Coordinata y del baricentro delle prese                                                     |
| **centroide_x**     | Coordinata x del baricentro delle prese                                                     |
| **var_y**           | Varianza delle posizioni y delle prese                                                      |
| **var_x**           | Varianza delle posizioni x delle prese                                                      |
| **sim_vert**        | Simmetria verticale (tra la parte sinistra e destra della board)                            |
| **sim_oriz**        | Simmetria orizzontale (tra la parte alta e bassa della board)                              |
| **diag_1**          | Numero di prese sulla diagonale principale                                                  |
| **diag_2**          | Numero di prese sulla diagonale secondaria (antidiagonale)                                 |
| **media_loc_mean**  | Media del filtro locale (media su finestre 3x3)                                            |
| **media_loc_std**   | Deviazione standard del filtro locale                                                      |
| **colonne_usate**   | Numero di colonne in cui è presente almeno una presa                                      |
| **righe_usate**     | Numero di righe in cui è presente almeno una presa                                        |

---

## Segmentazioni in bande

Per ogni suddivisione in bande orizzontali (righe) e verticali (colonne), vengono calcolate:

- **sum_rowband_N_I**: Numero di prese nella banda I-esima su N totali in orizzontale (righe)  
- **mean_rowband_N_I**: Media dei valori (prese) nella banda I-esima su N totali in orizzontale  
- **sum_colband_N_I**: Numero di prese nella banda I-esima su N totali in verticale (colonne)  
- **mean_colband_N_I**: Media dei valori (prese) nella banda I-esima su N totali in verticale  

**Esempio:**  
- `sum_rowband_3_2` = numero di prese nella seconda banda su 3 orizzontali  
- `mean_colband_4_1` = media prese nella prima banda su 4 verticali  

---

## Macro-zone (blocchi di varie dimensioni)

Per ogni blocco fisso (ad esempio 6x4, 9x4, 6x2, 3x3) vengono calcolate:

- **bloccoXxY_I_J_sum**: Numero di prese nel blocco di dimensione XxY che parte dalla riga I e colonna J  
- **bloccoXxY_I_J_mean**: Media delle prese nel blocco  
- **bloccoXxY_I_J_centroide_y**: Baricentro y delle prese nel blocco  
- **bloccoXxY_I_J_centroide_x**: Baricentro x delle prese nel blocco  
- **bloccoXxY_I_J_var**: Varianza delle posizioni delle prese nel blocco  

Queste feature permettono di analizzare la distribuzione locale delle prese in diverse zone della Moonboard, a seconda della dimensione e posizione del blocco.

---

## Statistiche avanzate

| Nome Feature          | Descrizione                                                                                |
|----------------------|--------------------------------------------------------------------------------------------|
| **skew_y**           | Skewness (asimmetria) della distribuzione delle prese sulle righe                         |
| **skew_x**           | Skewness (asimmetria) della distribuzione delle prese sulle colonne                       |
| **entropy**          | Entropia della distribuzione delle prese sulle colonne                                   |
| **n_clusters**       | Numero di cluster di prese connesse (gruppi di prese adiacenti anche in diagonale)       |
| **mean_cluster_size**| Dimensione media dei cluster                                                              |
| **max_cluster_size** | Dimensione massima di un cluster                                                          |
| **clusters_per_hold**| Rapporto tra numero di cluster e numero totale di prese                                   |
| **max_density_3x3**  | Massima densità di prese in una finestra 3x3                                              |
| **bbox_h**           | Altezza della bounding box che contiene tutte le prese                                  |
| **bbox_w**           | Larghezza della bounding box che contiene tutte le prese                                |
| **aspect_ratio**     | Rapporto larghezza/altezza della bounding box                                           |
| **switch_righe**     | Numero di transizioni (on/off) tra prese su tutte le righe                              |
| **switch_colonne**   | Numero di transizioni (on/off) tra prese su tutte le colonne                            |

---

**Nota:**  
Le feature di segmentazione in bande (`sum_rowband_*`, `mean_rowband_*`, `sum_colband_*`, `mean_colband_*`) e quelle basate sui blocchi (`blocco*`) vengono generate dinamicamente in base a quante bande e blocchi si scelgono di analizzare.  
Di conseguenza, il numero totale di colonne/features nel dataset può variare a seconda della granularità scelta.
