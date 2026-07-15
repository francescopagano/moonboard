# Moonboard Route Classification

> Repository unificato di **applicazione Shiny** e **pipeline di analisi/ML** per Moonboard 2016.
> Unified repository for the **Shiny application** and **analysis/ML workflows** for Moonboard 2016.

## Panoramica / Overview

Questo repository unifica i precedenti progetti `moonboard` e `moonboard_app` in una struttura unica e manutenibile.

This repository consolidates the former `moonboard` and `moonboard_app` projects into a single maintainable structure.

## Struttura del progetto / Project Structure

- `app/` — Shiny app runtime (UI/server, modelli serializzati, feature engineering Python, deploy manifest).
- `analysis/` — notebook, script e report di analisi/modellazione.
  - `analysis/data_processing/`
  - `analysis/data_mining/`
  - `analysis/data_viz/`
- `data/` — riferimento dati condivisi e note sulla sorgente.

## Setup rapido / Quick Setup

### Requisiti / Requirements

- R (consigliato >= 4.2)
- Python 3 (usato via `reticulate`)
- Pacchetti R indicati in `app/init.R` e `app/global.R`
- Dipendenze Python in `app/requirements.txt`

### Avvio applicazione / Run the App

Da root repository / From repository root:

```r
shiny::runApp("app")
```

Oppure entrando in `app/` e avviando `app.R`.

## Analisi / Analysis Workflows

Gli asset di analisi sono separati in `analysis/` per facilitare ricerca, evoluzione dei modelli e riproducibilità.

Analysis artifacts are organized under `analysis/` to keep experimentation and research clearly separated from runtime code.

## Dati / Data Source

I dataset non sono versionati integralmente nel repository.
Riferimento dati originale:

- https://drive.google.com/drive/folders/1-KI3rkEKUp-kfoWxsWlLJ6sKM6QOYs56?usp=drive_link

Ulteriori note in `data/README.md`.

## Contribuire / Contributing

1. Mantieni separazione tra codice applicativo (`app/`) e ricerca (`analysis/`).
2. Evita di committare artefatti locali (`.Rproj.user`, `.Rhistory`, cache Python).
3. Aggiorna la documentazione se modifichi struttura, dati o istruzioni di run.

## Note migrazione / Migration Notes

- Storico utile di `moonboard_app` importato con merge dedicato e contenuto sotto `app/`.
- Contenuti analitici dei branch `data_processing`, `data_mining`, `data_viz` consolidati in `analysis/`.
