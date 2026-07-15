import numpy as np
import pandas as pd
from scipy.stats import skew, entropy
from scipy.ndimage import center_of_mass, uniform_filter, gaussian_filter, laplace, label
from skimage.util import view_as_windows

def safe_skew(arr):
    if np.std(arr) < 1e-8:
        return 0.0  # oppure np.nan
    return skew(arr)

def estrai_feature_complete_con_nomi(matrice):
    features = []
    feature_names = []

    # --- Globali base ---
    totale = np.sum(matrice)
    centroide = center_of_mass(matrice)
    coords = np.argwhere(matrice)
    var_y, var_x = (np.var(coords[:, 0]), np.var(coords[:, 1])) if coords.size > 0 else (0, 0)
    sim_vert = np.mean(matrice[:, :5] == np.fliplr(matrice[:, 6:]))
    sim_oriz = np.mean(matrice[:9, :] == np.flipud(matrice[9:, :]))
    diag_1 = np.trace(matrice)
    diag_2 = np.trace(np.fliplr(matrice))
    media_loc_u_3 = uniform_filter(matrice.astype(float), size=3)
    media_loc_u_5 = uniform_filter(matrice.astype(float), size=5)
    media_loc_g = gaussian_filter(matrice.astype(float), sigma=1)
    media_loc_l = laplace(matrice.astype(float))

    features += [totale, *centroide, var_y, var_x, sim_vert, sim_oriz, diag_1, diag_2]
    feature_names += [
        "totale", "centroide_y", "centroide_x", "var_y", "var_x",
        "sim_vert", "sim_oriz", "diag_1", "diag_2"
    ]
    features += [np.mean(media_loc_u_3), np.std(media_loc_u_3), np.mean(media_loc_u_5), np.std(media_loc_u_5),
                 np.mean(media_loc_g), np.std(media_loc_g), np.mean(media_loc_l), np.std(media_loc_l)]
    feature_names += ["media_loc_u_3_mean", "media_loc_u_3_std", "media_loc_u_5_mean", "media_loc_u_5_std",
                      "media_loc_g_mean", "media_loc_g_std", "media_loc_l_mean", "media_loc_l_std"]
    features += [np.count_nonzero(np.sum(matrice, axis=0)), np.count_nonzero(np.sum(matrice, axis=1))]
    feature_names += ["colonne_usate", "righe_usate"]

    # --- Segmentazioni orizzontali e verticali ---

    # Segmentazioni sulle righe: 2, 3, 4, 6, 9, 18 bande
    for n in [2, 3, 4, 6, 9, 18]:
        for i in range(n):
            features += [np.mean(matrice[i*18//n:(i+1)*18//n, :])]
            feature_names += [f"mean_rowband_{n}_{i+1}"]

    # Segmentazioni sulle colonne: 2, 3, 4, 5, 11 bande
    for n in [2, 3, 4, 5, 11]:
        for i in range(n):
            features += [np.mean(matrice[:, i*11//n:(i+1)*11//n])]
            feature_names += [f"mean_colband_{n}_{i+1}"]

    # --- Blocchi fissi (macro-zone 6x4, 9x4, 6x2, 3x3) ---

    # Blocchi 6x4
    for i in range(0, 18, 6):
        for j in range(0, 11, 4):
            blocco = matrice[i:i+6, j:j+4]
            if blocco.shape == (6,4):
                blocco_coords = np.argwhere(blocco)
                #blocco_centroide = center_of_mass(blocco) if np.any(blocco) else (0, 0)
                blocco_var = np.var(blocco_coords[:, 0]) + np.var(blocco_coords[:, 1]) if blocco_coords.size else 0
                features += [np.mean(blocco), blocco_var]
                feature_names += [
                    f"blocco6x4_{i}_{j}_mean",
                    f"blocco6x4_{i}_{j}_var"
                ]
    # Blocchi 9x4
    for i in range(0, 18, 9):
        for j in range(0, 11, 4):
            blocco = matrice[i:i+9, j:j+4]
            if blocco.shape == (9,4):
                blocco_coords = np.argwhere(blocco)
                #blocco_centroide = center_of_mass(blocco) if np.any(blocco) else (0, 0)
                blocco_var = np.var(blocco_coords[:, 0]) + np.var(blocco_coords[:, 1]) if blocco_coords.size else 0
                features += [np.mean(blocco), blocco_var]
                feature_names += [
                    f"blocco9x4_{i}_{j}_mean",
                    f"blocco9x4_{i}_{j}_var"
                ]
    # Blocchi 6x2
    for i in range(0, 18, 6):
        for j in range(0, 11, 2):
            blocco = matrice[i:i+6, j:j+2]
            if blocco.shape == (6,2):
                blocco_coords = np.argwhere(blocco)
                #blocco_centroide = center_of_mass(blocco) if np.any(blocco) else (0, 0)
                blocco_var = np.var(blocco_coords[:, 0]) + np.var(blocco_coords[:, 1]) if blocco_coords.size else 0
                features += [np.mean(blocco), blocco_var]
                feature_names += [
                    f"blocco6x2_{i}_{j}_mean",
                    f"blocco6x2_{i}_{j}_var"
                ]
    # Blocchi 3x3
    for i in range(0, 18, 3):
        for j in range(0, 11, 3):
            blocco = matrice[i:i+3, j:j+3]
            if blocco.shape == (3,3):
                blocco_coords = np.argwhere(blocco)
                #blocco_centroide = center_of_mass(blocco) if np.any(blocco) else (0, 0)
                blocco_var = np.var(blocco_coords[:, 0]) + np.var(blocco_coords[:, 1]) if blocco_coords.size else 0
                features += [np.mean(blocco), blocco_var]
                feature_names += [
                    f"blocco3x3_{i}_{j}_mean",
                    f"blocco3x3_{i}_{j}_var"
                ]

    # Blocchi 6x3 (verticali, copertura completa)
    for i in [0, 6, 12]:
        i_end = 18 if i == 12 else i + 6
        for j in [0, 3, 6, 9]:
            j_end = 11 if j == 9 else j + 3
            blocco = matrice[i:i_end, j:j_end]
            if blocco.shape == (i_end - i, j_end - j):
                blocco_coords = np.argwhere(blocco)
                #blocco_centroide = center_of_mass(blocco) if np.any(blocco) else (0, 0)
                blocco_var = np.var(blocco_coords[:, 0]) + np.var(blocco_coords[:, 1]) if blocco_coords.size else 0
                features += [np.mean(blocco), blocco_var]
                feature_names += [
                    f"blocco6x3_{i}_{j}_mean",
                    f"blocco6x3_{i}_{j}_var"
                ]
    
    # Blocchi 9x2 (orizzontali, copertura completa)
    for i in [0, 9]:
        i_end = 18 if i == 9 else i + 9
        for j in [0, 2, 4, 6, 8, 10]:
            j_end = 11 if j == 10 else j + 2
            blocco = matrice[i:i_end, j:j_end]
            if blocco.shape == (i_end - i, j_end - j):
                blocco_coords = np.argwhere(blocco)
                #blocco_centroide = center_of_mass(blocco) if np.any(blocco) else (0, 0)
                blocco_var = np.var(blocco_coords[:, 0]) + np.var(blocco_coords[:, 1]) if blocco_coords.size else 0
                features += [np.mean(blocco), blocco_var]
                feature_names += [
                    f"blocco9x2_{i}_{j}_mean",
                    f"blocco9x2_{i}_{j}_var"
                ]

    # Skewness e entropia
    skew_y = safe_skew(np.sum(matrice, axis=1))
    skew_x = safe_skew(np.sum(matrice, axis=0))
    p_col = np.sum(matrice, axis=0)
    p = p_col / (np.sum(p_col) + 1e-6)
    entr = entropy(p)
    features += [skew_y, skew_x, entr]
    feature_names += ["skew_y", "skew_x", "entropy"]

    # Clustering
    clusters, n_clusters = label(matrice)
    cluster_sizes = np.bincount(clusters.flatten())[1:]
    mean_cluster_size = cluster_sizes.mean() if len(cluster_sizes) > 0 else 0
    max_cluster_size = cluster_sizes.max() if len(cluster_sizes) > 0 else 0
    features += [n_clusters, mean_cluster_size, max_cluster_size, n_clusters / (totale + 1e-6)]
    feature_names += ["n_clusters", "mean_cluster_size", "max_cluster_size", "clusters_per_hold"]

    # Sliding window densità massima
    if matrice.shape[0] >= 3 and matrice.shape[1] >= 3:
        finestra = view_as_windows(matrice, (3, 3))
        dens_blocchi = np.sum(finestra, axis=(2,3))
        max_dens = np.max(dens_blocchi)
        features.append(max_dens)
        feature_names.append("max_density_3x3")
    else:
        features.append(0)
        feature_names.append("max_density_3x3")

    # Bounding box e forma
    if coords.size > 0:
        min_y, min_x = coords.min(axis=0)
        max_y, max_x = coords.max(axis=0)
        h = max_y - min_y + 1
        w = max_x - min_x + 1
        aspect_ratio = w / h if h > 0 else 0
        features += [h, w, aspect_ratio]
        feature_names += ["bbox_h", "bbox_w", "aspect_ratio"]
    else:
        features += [0, 0, 0]
        feature_names += ["bbox_h", "bbox_w", "aspect_ratio"]

    # Transizioni (switch)
    switch_righe = np.sum(np.abs(np.diff(matrice, axis=1)))
    switch_colonne = np.sum(np.abs(np.diff(matrice, axis=0)))
    features += [switch_righe, switch_colonne]
    feature_names += ["switch_righe", "switch_colonne"]

    return np.array(features), feature_names

# Funzione per calcolare le feature da una matrice - QUESTA FUNZIONE MANCAVA!
def compute_features(matrix):
    """
    Funzione wrapper per calcolare le feature dalla matrice Moonboard.
    Questa funzione viene chiamata dall'app Shiny.
    
    Args:
        matrix: Una matrice booleana 18x11 che rappresenta le prese selezionate
        
    Returns:
        Una tupla (features, feature_names) con le feature calcolate e i loro nomi
    """
    # Verifica se la matrice è un array numpy
    if not isinstance(matrix, np.ndarray):
        matrix = np.array(matrix)
    
    # Assicurarsi che la matrice sia booleana
    matrix = matrix.astype(bool)
    
    # Calcolo delle feature
    features, feature_names = estrai_feature_complete_con_nomi(matrix)
    
    return features.tolist(), feature_names