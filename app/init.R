# Install packages from CRAN
required_packages <- c("shiny", "shinydashboard", "reticulate", "caret", 
                       "dplyr", "DT", "ggplot2", "lattice", "xgboost", 
                       "lightgbm", "nnet")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# For catboost, use standard install instead of URL install
if (!requireNamespace("catboost", quietly = TRUE)) {
  install.packages("catboost")
}