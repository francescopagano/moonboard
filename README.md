# 🧗‍♂️ Moonboard Builder 2016 — Shiny App

A Shiny web application for building, visualizing, and analyzing **Moonboard 2016** climbing problems.  
The app integrates R and Python, enabling interactive exploration, custom problem creation, and predictive modeling.

---

## 🌟 Features

- 🧱 Interactive **Moonboard 2016 problem builder**
- 📊 Visualization of problem data and model results
- 🔗 Seamless integration between **R and Python** using `reticulate`
- 🚀 Ready for deployment on **RStudio Connect** or **shinyapps.io**

---

## 📁 Project Structure

├── .git/ # Git version control files
├── .Rproj.user/ # RStudio project settings
├── data/ # Data files (Moonboard configuration, problem sets, etc.)
├── models/ # Saved models or weights (e.g., ML models)
├── python/ # Python scripts (e.g., data processing, model training)
├── rsconnect/ # Deployment configuration
│
├── .gitattributes # Git attributes
├── .Rhistory # R session history
├── .Rprofile # Custom R startup settings
│
├── app.R # Main app entry point
├── global.R # Global variables, packages, data
├── init.R # Environment and dependency setup
├── manifest.json # Deployment manifest
├── moonboard_app.Rproj # RStudio project file
├── requirements.txt # Python dependencies
├── server.R # Shiny server logic
├── ui.R # Shiny user interface definition
