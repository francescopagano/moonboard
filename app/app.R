# Add this at the beginning of your app.R file
options(warn = 1)  # Show warnings as they occur

# Check for required packages
required_packages <- c("shiny", "shinydashboard", "reticulate", 
                       "caret", "dplyr", "DT")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

# Optional packages - your app should work without these
optional_packages <- c("xgboost", "lightgbm", "catboost")
for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(paste("Package", pkg, "is not available. Some models will not work."))
  }
}

# Source your app files
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)