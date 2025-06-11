# Descrizione delle Variabili/Colonne del Dataset

---

## Variabili generali

| Nome Variabile | Descrizione                                  |
|----------------|---------------------------------------------|
| **setby**      | Utente o sistema che ha creato il tracciato |
| **userRating** | Valutazione data dall’utente al tracciato   |
| **repeats**    | Numero di ripetizioni del tracciato          |
| **isBenchmark**| Indica se il tracciato è un benchmark (sì/no)|
| **holdsets**   | Identificatori o gruppi di prese usate       |

---

## Caratteristiche principali (coordinate prese sulla board)

Le colonne che indicano la presenza o meno di una presa in una specifica posizione della board sono elencate di seguito:

- **A5, A6, A9, A10, A11, A12, A13, A14, A15, A16, A18**  
- **B3, B4, B6, B7, B8, B9, B10, B11, B12, B13, B15, B16, B18**  
- **C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C18**  
- **D3, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17, D18**  
- **E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E18**  
- **F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16**  
- **G2, G4, G6, G7, G8, G9, G10, G11, G12, G13, G14, G15, G16, G17, G18**  
- **H5, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, H18**  
- **I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I18**  
- **J2, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J16**  
- **K5, K6, K7, K8, K9, K10, K11, K12, K13, K14, K16, K18**  

---

# Descrizione delle feature estratte per ogni tracciato Moonboard

Ogni osservazione rappresenta un tracciato di arrampicata su Moonboard 2016 (matrice 18x11).  
I valori 1 indicano le prese utilizzate nel tracciato.

---

## Feature principali

| Nome Feature              | Descrizione                                                                                   |
|--------------------------|----------------------------------------------------------------------------------------------|
| **totale**                | Numero totale di prese usate nel tracciato                                                   |
| **densita**               | Densità di prese usate (totale / 198)                                                        |
| **centroide_y**           | Coordinata y (verticale) del baricentro delle prese usate. Valori bassi = prese in basso, valori alti = prese in alto. |
| **centroide_x**           | Coordinata x (orizzontale) del baricentro delle prese usate. Valori bassi = prese a sinistra, valori alti = prese a destra. |
| **var_y**                 | Varianza delle posizioni y delle prese                                                       |
| **var_x**                 | Varianza delle posizioni x delle prese                                                       |
| **sim_vert**              | Simmetria verticale della disposizione delle prese, calcolata confrontando la parte sinistra e destra della board. Valori vicini a 1 indicano disposizione molto simmetrica rispetto all’asse verticale centrale. |
| **sim_oriz**              | Simmetria orizzontale della disposizione delle prese, calcolata confrontando la parte alta e bassa della board. Valori vicini a 1 indicano disposizione molto simmetrica rispetto all’asse orizzontale centrale. |
| **diag_1**                | Numero di prese sulla diagonale principale                                                   |
| **diag_2**                | Numero di prese sulla diagonale secondaria (antidiagonale)                                  |
| **media_loc_mean**        | La media dei valori ottenuti applicando un filtro di media locale (finestra 3x3) sulla matrice delle prese. In pratica, per ogni posizione della matrice, si calcola la media dei valori nelle celle vicine (in una finestra 3x3 centrata su quella cella), poi si fa la media di tutti questi valori. Un valore alto indica che le prese sono raggruppate (molte finestre 3x3 hanno almeno una presa). |
| **media_loc_std**         | La deviazione standard dei valori ottenuti dal filtro di media locale (finestra 3x3). Indica quanto variano le densità locali di prese sulla board. Un valore alto indica che la densità locale delle prese è molto variabile (zone molto dense e zone vuote). |
| **colonne_usate**         | Numero di colonne in cui è presente almeno una presa                                         |
| **righe_usate**           | Numero di righe in cui è presente almeno una presa                                           |

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

| Nome Feature            | Descrizione                                                                                   |
|------------------------|----------------------------------------------------------------------------------------------|
| **skew_y**              | Skewness (asimmetria) della distribuzione delle prese sulle righe. Valori positivi indicano che le prese sono concentrate nella parte alta della board, negativi nella parte bassa, valori vicini a zero indicano distribuzione bilanciata tra alto e basso. |
| **skew_x**              | Skewness (asimmetria) della distribuzione delle prese sulle colonne. Valori positivi indicano prese concentrate a destra, negativi a sinistra, valori vicini a zero distribuzione bilanciata. |
| **entropy**             | Entropia della distribuzione delle prese sulle colonne. Valori alti indicano prese distribuite su molte colonne (distribuzione uniforme), valori bassi indicano prese concentrate su poche colonne. |
| **n_clusters**          | Numero di cluster di prese connesse (gruppi di prese adiacenti, anche in diagonale). Un numero alto indica prese sparse in piccoli gruppi, un numero basso indica prese raggruppate in pochi cluster grandi. |
| **mean_cluster_size**   | Dimensione media dei cluster (numero medio di prese per cluster). Valori alti indicano cluster grandi, valori bassi cluster piccoli e sparsi. |
| **max_cluster_size**    | Dimensione massima di un cluster (il gruppo più grande di prese connesse). Indica quanto può essere grande la zona più densa di prese. |
| **clusters_per_hold**   | Rapporto tra numero di cluster e numero totale di prese. Valori vicini a 1 indicano prese molto sparse, valori vicini a 0 prese molto raggruppate. |
| **max_density_3x3**     | Massima densità di prese trovata in una finestra 3x3 (il massimo numero di prese in qualsiasi blocco 3x3 della board). Valori alti indicano la presenza di zone molto dense, valori bassi prese più disperse. |
| **bbox_h**              | Altezza della bounding box che contiene tutte le prese (numero di righe coperte). Valori alti indicano prese distribuite su molte righe, valori bassi prese concentrate verticalmente. |
| **bbox_w**              | Larghezza della bounding box che contiene tutte le prese (numero di colonne coperte). Valori alti indicano prese distribuite su molte colonne, valori bassi prese concentrate orizzontalmente. |
| **aspect_ratio**        | Rapporto larghezza/altezza della bounding box (bbox_w / bbox_h). Valori >1 indicano prese più distribuite in orizzontale, <1 più in verticale, ≈1 distribuzione quadrata. |
| **switch_righe**        | Numero totale di transizioni (on/off) tra prese su tutte le righe (cambiamenti tra 0 e 1 da una colonna all’altra). Valori alti indicano prese alternate/spaziate sulle righe, valori bassi prese raggruppate senza molte interruzioni. |
| **switch_colonne**      | Numero totale di transizioni (on/off) tra prese su tutte le colonne (cambiamenti tra 0 e 1 da una riga all’altra). Valori alti indicano prese alternate/spaziate sulle colonne, valori bassi prese raggruppate senza molte interruzioni. |

---

## Nota

Le feature `sum_rowband_*`, `mean_rowband_*`, `sum_colband_*`, `mean_colband_*` e le feature basate sui blocchi (`blocco*`) sono generate dinamicamente in base a quante bande e blocchi si scelgono di analizzare.  
Di conseguenza, il numero totale di colonne/features nel dataset può variare a seconda della granularità scelta.
